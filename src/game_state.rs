extern crate num;
extern crate num_derive;
use std::cmp::Ordering;
use std::iter::*;
use num_derive::FromPrimitive;

#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive)]
pub enum Color {
    White,
    Black
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive)]
pub enum Piece {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King
}

// Rank are in reverse numerical number so that array initialization
// Matches chess board setup
#[derive(Clone, Copy, Debug, Eq, FromPrimitive, Ord, PartialEq, PartialOrd)]
pub enum Rank {
    _8,
    _7,
    _6,
    _5,
    _4,
    _3,
    _2,
    _1
}

#[derive(Clone, Copy, Debug, Eq, FromPrimitive, Ord, PartialEq, PartialOrd)]
pub enum File {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Pos {
    file: File,
    rank: Rank
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Move {
    old_position : Pos,
    new_position : Pos,
    capture : bool,
    check : bool,
    promotion: Option<Piece>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GameState {
    board: [[Option<(Color,Piece)>; 8]; 8],
    to_move: Color
    // @TODO - move count since pawn move/capture
    // @TODO -all previous hashes to check for 3 move repition draw
    // @TODO - also need previous move for en-passan
}

impl GameState {

    pub fn get_legal_moves(&self) -> Vec<Move> {

        let create_ranks = || (0..7).map(|r| num::FromPrimitive::from_u32(r).expect(&String::from("Expected 0 to 7 to be able to be mapped to a rank")));

        let files = (0..7).map(|f| num::FromPrimitive::from_u32(f).expect(&String::from("Expected 0 to 7 to be able to be mapped to a file")));
        
        let squares = files.flat_map(|file:File| create_ranks().map(move |rank:Rank| Pos{file, rank}));

        let squars_occupied_by_to_move = squares.filter_map(|pos| match self.get_square_state(pos) {
            Some((color,piece)) if color == self.to_move => Some((pos, (color,piece))),
            _ => None
        });

        squars_occupied_by_to_move.flat_map(|tuple| {
            let (pos, (color, piece)) = tuple;

            self.get_legal_moves_for_piece_on_square(
                pos,
                color,
                piece
            )
        })
        .collect()
    }

    fn get_square_state(&self, pos:Pos) -> Option<(Color,Piece)> {
        self.board[pos.rank as usize][pos.file as usize]
    }

    fn get_legal_moves_for_piece_on_square(&self, pos:Pos, c: Color, p: Piece) -> Vec<Move> {
        assert!(c == self.to_move, "Only pieces of color to move should be able to move");

        match p {
            Piece::Pawn => self.get_legal_pawn_moves(pos),
            _ => panic!("Unimplemented")
        }
    }
    
    fn get_legal_pawn_moves(&self, pos: Pos) -> Vec<Move> {

        assert!(pos.rank != Rank::_1 && pos.rank != Rank::_8, "Pawn can not be on first or last rank");
        
        match (self.to_move, pos.rank) {
            // first move for white
            (Color::White, Rank::_2) => vec![Rank::_3, Rank::_4],
            // first move for black
            (Color::Black, Rank::_7) => vec![Rank::_6, Rank::_5],
            // ranks are in inverse numerical value (ie Rank_8 = 0, in order to make array initialization match chess board setup,
            // hence non-intuitive negative for white and positive for black
            (Color::White, rank) => vec![num::FromPrimitive::from_i32(rank as i32 - 1).unwrap()],
            (Color::Black, rank) => vec![num::FromPrimitive::from_i32(rank as i32 + 1).unwrap()],
        }
        .into_iter()
        .map(|rank:Rank| Move{old_position:pos, new_position:Pos{file:pos.file, rank:rank}, capture:false, check:false, promotion:None})
        // filter any moves that would involve jumping over or landing on another piece
        .filter(|&m| self.get_first_occupied_square(m.old_position, m.new_position).is_none())
        .chain(self.get_legal_pawn_capture(pos))
        .flat_map(|m:Move| match m.new_position.rank {
            Rank::_1 | Rank::_8 => {
                vec![Move{promotion:Some(Piece::Knight), ..m},
                     Move{promotion:Some(Piece::Bishop), ..m},
                     Move{promotion:Some(Piece::Rook), ..m},
                     Move{promotion:Some(Piece::Queen), ..m}]
            },
            _ => vec![m]
        })
        .collect()
    }

    fn get_legal_pawn_capture(&self, pos: Pos) -> Vec<Move> {

        let rank:Rank = match self.to_move {
            Color::White => num::FromPrimitive::from_i32(pos.rank as i32 - 1).unwrap(),
            Color::Black => num::FromPrimitive::from_i32(pos.rank as i32 + 1).unwrap()
        };

        // can capture either right or left diagnal
        vec![pos.file as i32 - 1, pos.file as i32 + 1]
        .iter()
        // from_i32 returns none for invalid enum which takes care of filtering
        // off the board captures if on A or H file
        .filter_map(|file| num::FromPrimitive::from_i32(*file))
        .map(|file| Pos{file, rank})
        .filter_map(|capture_pos| match self.get_square_state(capture_pos) {
            Some((color, _)) if color != self.to_move => 
                Some(Move{old_position:pos, new_position:capture_pos, check:false, capture:true, promotion: None}),
            _ => None
         })
         .collect()
    }

    fn get_first_occupied_square(&self, start: Pos, finish: Pos) -> Option<Pos> {

        let get_first_occupied = |files:&mut dyn DoubleEndedIterator<Item=u8>, ranks:&mut dyn DoubleEndedIterator<Item=u8>| 
            files.zip(ranks)
            .skip(1) // first square is current position
            .find_map(|pos| {
                let (file, rank) = pos;

                let file = num::FromPrimitive::from_u8(file).unwrap();
                let rank = num::FromPrimitive::from_u8(rank).unwrap();

                let pos = Pos{file, rank};

                match self.get_square_state(pos) {
                    Some(_) => Some(pos),
                    None => None
                }
            });

        let files:&mut dyn DoubleEndedIterator<Item=u8> = &mut (start.file as u8..=finish.file as u8);
        let files_rev:&mut dyn DoubleEndedIterator<Item=u8> = &mut (finish.file as u8..=start.file as u8).rev();
        let files_repeat:&mut dyn DoubleEndedIterator<Item=u8> = &mut repeat(start.file as u8);

        let ranks:&mut dyn DoubleEndedIterator<Item=u8> = &mut (start.rank as u8..=finish.rank as u8);
        let ranks_rev:&mut dyn DoubleEndedIterator<Item=u8> = &mut (finish.rank as u8..=start.rank as u8).rev();
        let ranks_repeat:&mut dyn DoubleEndedIterator<Item=u8> = &mut repeat(start.rank as u8);

        // @TODO - assert is straight line or diagnal
        match (finish.file.cmp(&start.file), finish.rank.cmp(&start.rank)) {
            (Ordering::Greater, Ordering::Greater) => get_first_occupied(files, ranks),
            (Ordering::Greater, Ordering::Less) => get_first_occupied(files, ranks_rev),
            (Ordering::Greater, Ordering::Equal) => get_first_occupied(files, ranks_repeat),

            (Ordering::Equal, Ordering::Greater) => get_first_occupied(files_repeat, ranks),
            (Ordering::Equal, Ordering::Less) => get_first_occupied(files_repeat, ranks_rev),
            (Ordering::Equal, Ordering::Equal) => panic!("Can't move to same square"),

            (Ordering::Less, Ordering::Greater) => get_first_occupied(files_rev, ranks),
            (Ordering::Less, Ordering::Less) => get_first_occupied(files_rev, ranks_rev),
            (Ordering::Less, Ordering::Equal) => get_first_occupied(files_rev, ranks_repeat),
        }
    }
/*
    pub fn is_checkmate(game_state: GameState) -> bool {
        false
    }

    pub fn is_stalemate(game_state: GameState) -> bool {
        false
    }

    pub fn is_draw(game_state: GameState) -> bool {
        false
        // Insufficient Material
        // 3-fold repetition
        // 50 move rule
    }*/
}

// @TODO - move to seperate file
#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::zip;
    use std::fmt::Debug;

    // @TODO - move to generic helper libray
    fn are_equivelent<T>(actual:&Vec<T>, expected:&Vec<T>) -> bool 
        where T: Debug + PartialEq
    {
        if actual.len() != expected.len() {
            println!("Length of actual ({:?}) does not equal length of expected ({:?})", actual.len(), expected.len());
            false
        } else {
            zip(actual, expected)
            .all(|tuple| { 
                let (actual, expected) = tuple; 

                actual == expected
            })
        }
    }

    macro_rules! legal_move_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (game_state, expected) = $value;

                    let actual = game_state.get_legal_moves();

                    assert!(are_equivelent(&actual, &expected), "expected '{:?}' actual '{:?}'", expected, actual);
                }
            )*
        }
    }

    mod pawn_tests {
        use super::*;

        legal_move_tests! {
            white_pawn_should_be_able_to_move_one_or_two_squares_on_initial_move: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::White
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_3}, capture: false, check: false, promotion: None },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_4}, capture: false, check: false, promotion: None }]),

            black_pawn_should_be_able_to_move_one_or_two_squares_on_initial_move: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::Black
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_6}, capture:false, check: false, promotion: None },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check: false, promotion: None }]),

            white_pawn_should_be_able_to_move_one_squares_after_initial_move: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::White
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check: false, promotion: None }]),

            black_pawn_should_be_able_to_move_one_squares_after_initial_move: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::Black
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check: false, promotion: None }]),

            white_pawn_should_not_be_able_to_move_if_blocked_by_another_piece: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::White
            }, Vec::new()),

            black_pawn_should_not_be_able_to_move_if_blocked_by_another_piece: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::Black
            }, Vec::new()),

            white_pawn_should_not_be_able_to_jump_over_another_piece: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::White
            }, Vec::new()),

            black_pawn_should_not_be_able_to_jump_over_another_piece: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::Black
            }, Vec::new()),

            white_pawn_should_be_able_to_capture_on_diaganol: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::White
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check: false, promotion: None },
                    Move{old_position: Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::B, rank:Rank::_4}, capture:true, check: false, promotion: None }]),

            black_pawn_should_be_able_to_capture_on_diaganol: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,Some((Color::White, Piece::Pawn)),None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::Black
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check: false, promotion: None },
                    Move{old_position: Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:true, check: false, promotion: None }]),
        
            white_pawn_should_not_be_able_to_capture_same_color: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,Some((Color::White, Piece::Pawn)),None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::White
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture: false, check: false, promotion: None },
                    Move{old_position: Pos{file:File::B, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_5}, capture: false, check: false, promotion: None }]),

            black_pawn_should_not_be_able_to_capture_same_color: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::Black
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::A, rank:Rank::_5}, capture: false, check: false, promotion: None },
                    Move{old_position: Pos{file:File::B, rank:Rank::_5}, new_position:Pos{file:File::B, rank:Rank::_4}, capture: false, check: false, promotion: None }]),

            white_pawn_should_be_able_to_promote_if_on_7th_rank: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::White
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture: false, check: false, promotion: Some(Piece::Knight) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture: false, check: false, promotion: Some(Piece::Bishop) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture: false, check: false, promotion: Some(Piece::Rook) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture: false, check: false, promotion: Some(Piece::Queen) }]),
            
            black_pawn_should_be_able_to_promote_if_on_2nd_rank: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::Black
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture: false, check: false, promotion: Some(Piece::Knight) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture: false, check: false, promotion: Some(Piece::Bishop) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture: false, check: false, promotion: Some(Piece::Rook) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture: false, check: false, promotion: Some(Piece::Queen) }]),

            white_pawn_should_be_able_to_promote_when_capturing_on_8th_rank: (GameState {
                board : [
                    [None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move : Color::White
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture: false, check: false, promotion: Some(Piece::Knight) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture: false, check: false, promotion: Some(Piece::Bishop) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture: false, check: false, promotion: Some(Piece::Rook) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture: false, check: false, promotion: Some(Piece::Queen) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture: true, check: false, promotion: Some(Piece::Knight) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture: true, check: false, promotion: Some(Piece::Bishop) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture: true, check: false, promotion: Some(Piece::Rook) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture: true, check: false, promotion: Some(Piece::Queen) }]),

            black_pawn_should_be_able_to_promote_when_capturing_on_1st_rank: (GameState {
                board : [
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None, Some((Color::White, Piece::Pawn)),None,None,None,None,None,None],
                ],
                to_move : Color::Black
            }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture: false, check: false, promotion: Some(Piece::Knight) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture: false, check: false, promotion: Some(Piece::Bishop) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture: false, check: false, promotion: Some(Piece::Rook) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture: false, check: false, promotion: Some(Piece::Queen) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture: true, check: false, promotion: Some(Piece::Knight) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture: true, check: false, promotion: Some(Piece::Bishop) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture: true, check: false, promotion: Some(Piece::Rook) },
                    Move{old_position: Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture: true, check: false, promotion: Some(Piece::Queen) }]),

            // en-passant
        }
    }
}
