extern crate num;
extern crate num_derive;
use std::cmp::Ordering;
use std::iter::*;
use num_derive::FromPrimitive;

#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive, PartialOrd)]
pub enum Color {
    White,
    Black
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive, Ord, PartialOrd)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Pos {
    file:File,
    rank:Rank
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Move {
    old_position :Pos,
    new_position :Pos,
    capture :bool,
    check :bool,
    promotion:Option<Piece>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GameState {
    board:[[Option<(Color,Piece)>; 8]; 8],
    to_move:Color,
    previous_moves:Vec<Move>,
    // @TODO - should be able to calculate below o from just
    // previous move list but probably won't be most optimal way 
    // @TODO - move count since pawn move/capture
    // @TODO -all previous hashes to check for 3 move repition draw
}

impl GameState {

    pub fn get_legal_moves(&self) -> Vec<Move> {

        let create_ranks = || (0..8).map(|r| num::FromPrimitive::from_u32(r).expect(&String::from("Expected 0 to 7 to be able to be mapped to a rank")));

        let files = (0..8).map(|f| num::FromPrimitive::from_u32(f).expect(&String::from("Expected 0 to 7 to be able to be mapped to a file")));
        
        let squares = files.flat_map(|file:File| create_ranks().map(move |rank:Rank| Pos{file, rank}));

        let squares_occupied_by_to_move = squares.filter_map(|pos| match self.get_square_state(&pos) {
            Some((color,piece)) if color == self.to_move => Some((pos, (color,piece))),
            _ => None
        });

        squares_occupied_by_to_move.flat_map(|tuple| {
            let (pos, (color, piece)) = tuple;

            self.get_legal_moves_for_piece_on_square(
                &pos,
                color,
                piece
            )
        })
        .collect()
    }

    fn get_square_state(&self, pos:&Pos) -> Option<(Color,Piece)> {
        self.board[pos.rank as usize][pos.file as usize]
    }

    fn get_legal_moves_for_piece_on_square(&self, pos:&Pos, c:Color, p:Piece) -> Vec<Move> {
        assert!(c == self.to_move, "Only pieces of color to move should be able to move");

        match p {
            Piece::Pawn => self.get_legal_pawn_moves(pos),
            Piece::Knight => self.get_legal_knight_moves(pos),
            _ => panic!("Unimplemented")
        }
    }
    
    fn get_legal_pawn_moves(&self, pos:&Pos) -> Vec<Move> {

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
        .map(|rank:Rank| Move{old_position:*pos, new_position:Pos{file:pos.file, rank:rank}, capture:false, check:false, promotion:None})
        // @TODO - may make sense to extract this since it is shared behavior with other piece movements 
        // filter any moves that would involve jumping over or landing on another piece
        .filter(|&m| self.get_first_occupied_square(&m.old_position, &m.new_position).is_none())
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

    fn get_legal_pawn_capture(&self, pos:&Pos) -> Vec<Move> {

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
        .filter_map(|capture_pos| match self.get_square_state(&capture_pos) {
            Some((color, _)) if color != self.to_move => 
                Some(Move{old_position:*pos, new_position:capture_pos, check:false, capture:true, promotion:None}),
            _ => None
         })
         // en passant?
         .collect()
    }

    fn get_legal_knight_moves(&self, pos:&Pos) -> Vec<Move> {
        println!("Get legal knight moves");
        // @TODO - extract all pairs method
        // @TODO - make extension subtree crate
        let moves = [(2,1),(2,-1),(1,2),(1,-2),(-1,2),(-1,-2),(-2,-1),(-2,1)];
        let ranks = moves.iter().map(|(r,_)| -> Option<Rank> { num::FromPrimitive::from_i32(pos.rank as i32 + *r) });
        let files = moves.iter().map(|(_,f)| -> Option<File> { num::FromPrimitive::from_i32(pos.file as i32 + *f) });
        
        ranks.zip(files)
        .inspect(|(r,f)| println!("Zipped moves: R:{:?} F:{:?}",r,f))
        .filter_map(|(r,f)| match (r,f) {
            (Some(rank),Some(file)) => Some(Move{old_position:*pos, new_position:Pos{rank,file}, check:false, capture:false, promotion:None}),
            _ => None
        })
        // can't land on own-piece
        .filter(|m| match self.get_square_state(&m.new_position) {
            Some((c, _)) if c == self.to_move => false,
            _ => true
        })
        .collect()
    }

    fn get_first_occupied_square(&self, start:&Pos, finish:&Pos) -> Option<Pos> {

        GameState::get_positions_between(start, finish).iter().find_map(|pos|
            match self.get_square_state(pos) {
                Some(_) => Some(*pos),
                None => None
        })
    }

    /// get_squares_betwen returns a vector of positions representing all of the squares in between start and finish.
    /// not couting the start square
    /// Example: get_squares_between(Pos{file:File::_A, rank::rank:1}, Pos{file:File::_A, rank::rank:5})
    ///          // Return: vec![Pos{file:File::_A, rank::Rank:2},
    ///                          Pos{file:File::_A, rank::Rank:3}
    ///                          Pos{file:File::_A, rank::Rank:4}
    ///                          Pos{file:File::_A, rank::Rank:5}
    /// Remarks: Start and finish must either be on the same rank, file or diaganol
    fn get_positions_between(start:&Pos, finish:&Pos) -> Vec<Pos> {

        let get_positions = |files:&mut dyn DoubleEndedIterator<Item=u8>, ranks:&mut dyn DoubleEndedIterator<Item=u8>| 
            files.zip(ranks)
            .skip(1) // first square is current position
            .map(|pos| {
                let (file, rank) = pos;

                let file = num::FromPrimitive::from_u8(file).unwrap();
                let rank = num::FromPrimitive::from_u8(rank).unwrap();

                Pos{file, rank}
            })
            .collect();

        let files:&mut dyn DoubleEndedIterator<Item=u8> = &mut (start.file as u8..=finish.file as u8);
        let files_rev:&mut dyn DoubleEndedIterator<Item=u8> = &mut (finish.file as u8..=start.file as u8).rev();
        let files_repeat:&mut dyn DoubleEndedIterator<Item=u8> = &mut repeat(start.file as u8);

        let ranks:&mut dyn DoubleEndedIterator<Item=u8> = &mut (start.rank as u8..=finish.rank as u8);
        let ranks_rev:&mut dyn DoubleEndedIterator<Item=u8> = &mut (finish.rank as u8..=start.rank as u8).rev();
        let ranks_repeat:&mut dyn DoubleEndedIterator<Item=u8> = &mut repeat(start.rank as u8);

        // @TODO - assert is straight line or diagnal
        match (finish.file.cmp(&start.file), finish.rank.cmp(&start.rank)) {
            (Ordering::Greater, Ordering::Greater) => get_positions(files, ranks),
            (Ordering::Greater, Ordering::Less) => get_positions(files, ranks_rev),
            (Ordering::Greater, Ordering::Equal) => get_positions(files, ranks_repeat),

            (Ordering::Equal, Ordering::Greater) => get_positions(files_repeat, ranks),
            (Ordering::Equal, Ordering::Less) => get_positions(files_repeat, ranks_rev),
            (Ordering::Equal, Ordering::Equal) => panic!("Can't move to same square"),

            (Ordering::Less, Ordering::Greater) => get_positions(files_rev, ranks),
            (Ordering::Less, Ordering::Less) => get_positions(files_rev, ranks_rev),
            (Ordering::Less, Ordering::Equal) => get_positions(files_rev, ranks_repeat),
        }
    }
/*
    pub fn is_checkmate(game_state:GameState) -> bool {
        false
    }

    pub fn is_stalemate(game_state:GameState) -> bool {
        false
    }

    pub fn is_draw(game_state:GameState) -> bool {
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
    use std::fmt::Debug;

    // @TODO - move to generic helper libray
    fn are_equivelent<T>(mut actual:Vec<T>, mut expected:Vec<T>) -> bool
        where T:Debug + PartialEq + Ord
    {
        if actual.len() != expected.len() {
            println!("Length of actual ({:?}) does not equal length of expected ({:?})", actual.len(), expected.len());
            false
        } else {
            actual.sort();
            expected.sort();
            actual.into_iter().zip(expected.into_iter())
            .all(|tuple| { 
                let (actual, expected) = tuple; 

                actual == expected
            })
        }
    }

    macro_rules! legal_move_tests {
        ($($name:ident:$value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (game_state, expected) = $value;

                    let actual = game_state.get_legal_moves();

                    assert!(are_equivelent(actual.clone(), expected.clone()), "expected '{:?}' actual '{:?}'", expected, actual);
                }
            )*
        }
    }

    mod pawn_tests {
        use super::*;

        legal_move_tests! {
            white_pawn_should_be_able_to_move_one_or_two_squares_on_initial_move:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None }]),

            black_pawn_should_be_able_to_move_one_or_two_squares_on_initial_move:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::Black,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check:false, promotion:None }]),

            white_pawn_should_be_able_to_move_one_squares_after_initial_move:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None }]),

            black_pawn_should_be_able_to_move_one_squares_after_initial_move:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::Black,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check:false, promotion:None }]),

            white_pawn_should_not_be_able_to_move_if_blocked_by_another_piece:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec!()),

            black_pawn_should_not_be_able_to_move_if_blocked_by_another_piece:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::Black,
                previous_moves:vec![],
            }, vec!()),

            white_pawn_should_not_be_able_to_jump_over_another_piece:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec!()),

            black_pawn_should_not_be_able_to_jump_over_another_piece:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::Black,
                previous_moves:vec![],
            }, vec!()),

            white_pawn_should_be_able_to_capture_on_diaganol:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::B, rank:Rank::_4}, capture:true, check:false, promotion:None }]),

            black_pawn_should_be_able_to_capture_on_diaganol:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,Some((Color::White, Piece::Pawn)),None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::Black,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:true, check:false, promotion:None }]),
        
            white_pawn_should_not_be_able_to_capture_same_color:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,Some((Color::White, Piece::Pawn)),None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::B, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:false, check:false, promotion:None }]),

            black_pawn_should_not_be_able_to_capture_same_color:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::Black,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::B, rank:Rank::_5}, new_position:Pos{file:File::B, rank:Rank::_4}, capture:false, check:false, promotion:None }]),

            white_pawn_should_be_able_to_promote_if_on_7th_rank:(GameState {
                board:[
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Knight) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Bishop) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Rook) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Queen) }]),
            
            black_pawn_should_be_able_to_promote_if_on_2nd_rank:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::Black,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Knight) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Bishop) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Rook) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Queen) }]),

            white_pawn_should_be_able_to_promote_when_capturing_on_8th_rank:(GameState {
                board :[
                    [None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Knight) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Bishop) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Rook) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Queen) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture:true, check:false, promotion:Some(Piece::Knight) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture:true, check:false, promotion:Some(Piece::Bishop) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture:true, check:false, promotion:Some(Piece::Rook) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture:true, check:false, promotion:Some(Piece::Queen) }]),

            black_pawn_should_be_able_to_promote_when_capturing_on_1st_rank:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None, Some((Color::White, Piece::Pawn)),None,None,None,None,None,None],
                ],
                to_move :Color::Black,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Knight) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Bishop) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Rook) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Queen) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture:true, check:false, promotion:Some(Piece::Knight) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture:true, check:false, promotion:Some(Piece::Bishop) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture:true, check:false, promotion:Some(Piece::Rook) },
                    Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture:true, check:false, promotion:Some(Piece::Queen) }]),

            white_pawn_should_be_able_to_capture_en_passant:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move:Color::White,
                previous_moves:vec![Move{old_position:Pos{file:File::B, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:false, check:false, promotion:None }],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::A, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::B, rank:Rank::_7}, capture:true, check:false, promotion:None }]),
        }
    }

    mod knight_tests {
        use super::*;

        legal_move_tests! {
            knight_should_be_able_to_move_in_l_shape:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,Some((Color::White, Piece::Knight)),None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_6}, capture:false, check:false, promotion:None }]),
        
            knight_can_not_move_off_edge_of_board:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,Some((Color::White, Piece::Knight)),None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::A, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::D, rank:Rank::_2}, capture:false, check:false, promotion:None }]),

            knight_can_not_land_on_own_piece:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black,Piece::Pawn)),None,None,None,None,None,None,None],
                    [Some((Color::White,Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,Some((Color::White, Piece::Knight)),None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::D, rank:Rank::_2}, capture:false, check:false, promotion:None }]),
            // @TODO - can't land on own piece
            // @TODO - check tests
        }
    }
}
