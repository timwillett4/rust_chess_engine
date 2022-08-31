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
        // see if it would be able to capture king on next move...
        // if so mark as check
        //.map(|mov| self.get_legal_moves_for_piece_on_square())
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
            Piece::Bishop => self.get_legal_bishop_moves(pos),
            Piece::Rook => self.get_legal_rook_moves(pos),
            Piece::Queen => self.get_legal_queen_moves(pos),
            Piece::King => self.get_legal_king_moves(pos),
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
            Color::White => num::FromPrimitive::from_i32(pos.rank as i32 - 1).expect("Pawn should never be on last rank"),
            Color::Black => num::FromPrimitive::from_i32(pos.rank as i32 + 1).expect("Pawn should never be on last rank"),
        };

        let opponent_is_on_diaganol = |cap_sq:&Pos| match self.get_square_state(cap_sq) {
            Some((color, _)) if color != self.to_move => true,
            _ => false,
        };

        let can_en_passant_capture = |cap_sq:&Pos| {
            let move_dist = |m:&Move| num::abs((m.new_position.rank as i32) - (m.old_position.rank as i32));

            let was_opponent_first_pawn_move_of_2 = |m:&Move| match self.get_square_state(&m.new_position) {
                Some((color,piece)) if color != self.to_move && piece == Piece::Pawn && move_dist(m) == 2 => true,
                _ => false
            };

            let prev_to_move = match self.to_move {
                Color::White => Color::Black,
                Color::Black => Color::White
            };

            // where they would have moved if they only moved one
            let move_one_rank = |prev_move_to_sq:&Pos| -> Rank { match prev_to_move {
                Color::White => num::FromPrimitive::from_i32(prev_move_to_sq.rank as i32 + 1).expect("Pawn should never be on last rank"),
                Color::Black => num::FromPrimitive::from_i32(prev_move_to_sq.rank as i32 - 1).expect("Pawn should never be on last rank")
            }};

            let move_one_pos = |prev_move_to_sq:&Pos| Pos{rank:move_one_rank(prev_move_to_sq),file:prev_move_to_sq.file};

            match self.previous_moves.get(0) {
                Some(m) if was_opponent_first_pawn_move_of_2(&m) && move_one_pos(&m.new_position) == *cap_sq => true,
                _ => false
            }
        };

        //let is_opponenet_on_capture_square = 
        // can capture either right or left diagnal
        vec![pos.file as i32 - 1, pos.file as i32 + 1]
        .iter()
        // from_i32 returns none for invalid enum which takes care of filtering
        // off the board captures if on A or H file
        .filter_map(|file| num::FromPrimitive::from_i32(*file))
        .map(|file| Pos{file, rank})
        .filter(|cap_sq| opponent_is_on_diaganol(cap_sq) || can_en_passant_capture(cap_sq))
        .map(|cap_sq| Move{old_position:*pos, new_position:cap_sq, capture:true, check: false, promotion: None})
        .collect()
    }

    fn get_legal_knight_moves(&self, pos:&Pos) -> Vec<Move> {
        let moves = [(2,1),(2,-1),(1,2),(1,-2),(-1,2),(-1,-2),(-2,-1),(-2,1)];

        let ranks = moves.iter().map(|(r,_)| -> Option<Rank> { num::FromPrimitive::from_i32(pos.rank as i32 + *r) });
        let files = moves.iter().map(|(_,f)| -> Option<File> { num::FromPrimitive::from_i32(pos.file as i32 + *f) });
        
        let is_landing_on_own_piece = |m:&Move| match self.get_square_state(&m.new_position) {
            Some((c, _)) if c == self.to_move => true,
            _ => false
        };

        let is_capturing = |m:&Move| match self.get_square_state(&m.new_position) {
            Some((c, _)) if c != self.to_move => true,
            _ => false
        };

        let moves = ranks
        .zip(files)
        .filter_map(|(r,f)| match (r,f) {
            // This automatically fileters out moves that go off of edge of board
            (Some(rank),Some(file)) => Some(Move{old_position:*pos, new_position:Pos{rank,file}, check:false, capture:false, promotion:None}),
            _ => None
        });

        moves.filter(|m| is_landing_on_own_piece(m) == false)
        .map(|m| match is_capturing(&m) {
            true => Move{capture:true,..m},
            false => m
        })
        .collect()
    }

    fn get_legal_bishop_moves(&self, pos:&Pos) -> Vec<Move> {

        // @TODO - comment/clean up
        let rank_offset = pos.rank as i32;
        let file_offset = pos.file as i32;

        let add_max = std::cmp::min(7-rank_offset, 7-file_offset);
        let sub_max = std::cmp::min(rank_offset, file_offset);

        let top_left_file = num::FromPrimitive::from_i32(pos.file as i32 - sub_max).unwrap();
        let top_left_rank = num::FromPrimitive::from_i32(pos.rank as i32 - sub_max).unwrap();
        let top_left = Pos{file:top_left_file, rank:top_left_rank, };
        let top_left = self.get_first_occupied_square(pos, &top_left).unwrap_or(top_left);

        let bottom_right_file = num::FromPrimitive::from_i32(pos.file as i32 + add_max).unwrap();
        let bottom_right_rank = num::FromPrimitive::from_i32(pos.rank as i32 + add_max).unwrap();
        let bottom_right = Pos{file:bottom_right_file, rank:bottom_right_rank};
        let bottom_right = self.get_first_occupied_square(pos, &bottom_right).unwrap_or(bottom_right);

        let bottom_left_offset = std::cmp::min(file_offset, 7 - rank_offset);
        let bottom_left_file = num::FromPrimitive::from_i32(pos.file as i32 - bottom_left_offset).unwrap();
        let bottom_left_rank = num::FromPrimitive::from_i32(pos.rank as i32 + bottom_left_offset).unwrap();
        let bottom_left = Pos{file:bottom_left_file, rank:bottom_left_rank};
        let bottom_left = self.get_first_occupied_square(pos, &bottom_left).unwrap_or(bottom_left);

        let top_right_offset = std::cmp::min(7 - file_offset, rank_offset);
        let top_right_file = num::FromPrimitive::from_i32(pos.file as i32 + top_right_offset).unwrap();
        let top_right_rank = num::FromPrimitive::from_i32(pos.rank as i32 - top_right_offset).unwrap();
        let top_right = Pos{file:top_right_file, rank:top_right_rank};
        let top_right = self.get_first_occupied_square(pos, &top_right).unwrap_or(top_right);

        // @TODO - extract this to private method as it is mostly identical to rook logic
        GameState::get_positions_between(&bottom_left, &top_right)
            .into_iter()
            .chain(GameState::get_positions_between(&top_left, &bottom_right))
            .map(|new_pos| Move{old_position:*pos, new_position:new_pos, check:false, capture: false, promotion: None})
            .filter_map(|mov| match self.get_square_state(&mov.new_position) {
                Some((Color::White, _)) => None,
                Some((Color::Black, _)) => Some(Move{capture:true,..mov}),
                None => Some(mov)
            })
            .collect()
    }

    fn get_legal_rook_moves(&self, pos:&Pos) -> Vec<Move> {
        
        let top = Pos{file: pos.file, rank:Rank::_1};
        let top = self.get_first_occupied_square(&pos, &top).unwrap_or(top);

        let bottom = Pos{file: pos.file, rank:Rank::_8};
        let bottom = self.get_first_occupied_square(&pos, &bottom).unwrap_or(bottom);

        let left = Pos{file: File::A, rank:pos.rank};
        let left = self.get_first_occupied_square(&pos, &left).unwrap_or(left);

        let right = Pos{file: File::H, rank:pos.rank};
        let right = self.get_first_occupied_square(&pos, &right).unwrap_or(right);

        GameState::get_positions_between(&top, &bottom)
            .into_iter()
            .chain(GameState::get_positions_between(&left, &right))
            .map(|new_pos| Move{old_position:*pos, new_position:new_pos, check:false, capture: false, promotion: None})
            .filter_map(|mov| match self.get_square_state(&mov.new_position) {
                Some((Color::White, _)) => None,
                Some((Color::Black, _)) => Some(Move{capture:true,..mov}),
                None => Some(mov)
            })
            .collect()
    }

    fn get_legal_queen_moves(&self, pos:&Pos) -> Vec<Move> {
        self.get_legal_bishop_moves(&pos)
            .into_iter()
            .chain(self.get_legal_rook_moves(&pos))
            .collect()
    }

    fn get_legal_king_moves(&self, pos:&Pos) -> Vec<Move> {
       let possible_moves = [(-1,0),(-1,1),(-1,-1),(0,1),(0,-1),(1,1),(1,0),(1,-1)];
       self.get_legal_moves_from_offset_list(&pos, &mut possible_moves.into_iter())
    }

    fn get_legal_moves_from_offset_list(&self, start:&Pos, move_offsets:&mut dyn Iterator<Item=(i32,i32)>) -> Vec<Move> {

        let is_landing_on_own_piece = |m:&Move| match self.get_square_state(&m.new_position) {
            Some((c, _)) if c == self.to_move => true,
            _ => false
        };

        let is_capturing = |m:&Move| match self.get_square_state(&m.new_position) {
            Some((c, _)) if c != self.to_move => true,
            _ => false
        };

        move_offsets.filter_map(|(r,f)| { 
            let file = num::FromPrimitive::from_i32(start.file as i32 + f);
            let rank = num::FromPrimitive::from_i32(start.rank as i32 + r);
            
            // invalid enum will map to none causing moves that would be for invalid
            // squares to get filtered
            match (file,rank) {
                (Some(file), Some(rank)) => Some(Pos{file,rank}),
                _ => None
            }
        })
        .map(|new_pos| Move{old_position:*start, new_position:new_pos, check:false, capture:false, promotion:None})
        .filter(|m| is_landing_on_own_piece(m) == false)
        .map(|m| match is_capturing(&m) {
            true => Move{capture:true,..m},
            false => m
        })
        .collect()
    }

    fn get_first_occupied_square(&self, start:&Pos, finish:&Pos) -> Option<Pos> {

        GameState::get_positions_between(start, finish)
        .iter()
        .skip(1)
        .find_map(|pos|
            match self.get_square_state(pos) {
                Some(_) => Some(*pos),
                None => None
        })
    }

    /// get_squares_betwen returns a vector of positions representing all of the squares in between start and finish.
    /// (including the start and finish square)
    /// Example: get_squares_between(Pos{file:File::_A, rank::rank:1}, Pos{file:File::_A, rank::rank:5})
    ///          // Return: vec![Pos{File::File_A, rank::Rank:_1},
    ///                          Pos{file:File::_A, rank::Rank:2},
    ///                          Pos{file:File::_A, rank::Rank:3},
    ///                          Pos{file:File::_A, rank::Rank:4},
    ///                          Pos{file:File::_A, rank::Rank:5}
    /// Remarks: Start and finish must either be on the same rank, file or diaganol
    fn get_positions_between(start:&Pos, finish:&Pos) -> Vec<Pos> {

        let get_positions = |files:&mut dyn DoubleEndedIterator<Item=u8>, ranks:&mut dyn DoubleEndedIterator<Item=u8>| 
            files.zip(ranks)
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
            (Ordering::Equal, Ordering::Equal) => vec![*start], // start and finish are same square, just return start

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
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move:Color::White,
                previous_moves:vec![Move{old_position:Pos{file:File::B, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:false, check:false, promotion:None }],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_5}, new_position:Pos{file:File::A, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_5}, new_position:Pos{file:File::B, rank:Rank::_6}, capture:true, check:false, promotion:None }]),

            black_pawn_should_be_able_to_capture_en_passant:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move:Color::Black,
                previous_moves:vec![Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None }],
            }, vec![Move{old_position:Pos{file:File::B, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::B, rank:Rank::_4}, new_position:Pos{file:File::A, rank:Rank::_3}, capture:true, check:false, promotion:None }]),

            /*white_pawn_move_marked_as_check:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,Some((Color::Black, Piece::King)),None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move:Color::White,
                previous_moves:vec![Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None }],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:true, promotion:None }]),*/
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

            knight_can_capture_opponent_piece:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::Black,Piece::Pawn)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,Some((Color::White, Piece::Knight)),None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::A, rank:Rank::_3}, capture:true, check:false, promotion:None },
                    Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::D, rank:Rank::_2}, capture:false, check:false, promotion:None }]),
            // @TODO - check tests
        }
    }

    mod bishop_tests {
        use super::*;

        legal_move_tests! {
            bishop_should_be_able_to_move_on_diaganol_from_center_of_board:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,Some((Color::White, Piece::Bishop)),None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::G, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::H, rank:Rank::_8}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::G, rank:Rank::_1}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::A, rank:Rank::_7}, capture:false, check:false, promotion:None }]),
        }

        legal_move_tests! {
            bishop_should_be_able_to_move_on_diaganol_from_left_side_of_board:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Bishop)),None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_1}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_8}, capture:false, check:false, promotion:None }]),
        }

        legal_move_tests! {
            bishop_should_be_able_to_move_on_diaganol_from_right_side_of_board:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,Some((Color::White, Piece::Bishop))],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::G, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::G, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_1}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_8}, capture:false, check:false, promotion:None }]),
        }

        legal_move_tests! {
            bishop_should_be_able_to_move_on_diaganol_from_top_of_board:(GameState {
                board :[
                    [None,None,None,Some((Color::White, Piece::Bishop)),None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::C, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::E, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::B, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::F, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::G, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::H, rank:Rank::_4}, capture:false, check:false, promotion:None }]),
        }

        legal_move_tests! {
            bishop_should_be_able_to_move_on_diaganol_from_diaganol:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Bishop)),None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::B, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::D, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::E, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::F, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::G, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::H, rank:Rank::_8}, capture:false, check:false, promotion:None }]),
        }
    
        legal_move_tests! {
            bishop_should_not_be_able_to_jump_over_own_piece:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,Some((Color::White, Piece::Pawn)),None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Bishop)),None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::B, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::C, rank:Rank::_3}, new_position:Pos{file:File::C, rank:Rank::_4}, capture:false, check:false, promotion:None }]),
        }

        legal_move_tests! {
            bishop_should_to_capture_opponent_piece:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [Some((Color::White, Piece::Bishop)),None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::B, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:true, check:false, promotion:None }]),
        }
        // @TODO 
        // check
    }

    mod rook_tests {
        use super::*;

        legal_move_tests! {
            rook_should_be_able_to_move_vertically_or_horizontally:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,Some((Color::White, Piece::Rook)),None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::G, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::H, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_1}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_8}, capture:false, check:false, promotion:None }]),
        }

        legal_move_tests! {
            rook_should_not_be_able_to_jump_over_own_piece:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,Some((Color::White, Piece::Rook)),Some((Color::White,Piece::Pawn)),None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::E, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_1}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_8}, capture:false, check:false, promotion:None }]),
        }

        legal_move_tests! {
            rook_should_be_able_to_capture_opponent_piece:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,Some((Color::White, Piece::Rook)),Some((Color::Black,Piece::Pawn)),None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_4}, capture:true, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_1}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_8}, capture:false, check:false, promotion:None }]),
        }
    }

    mod queen_tests {
        use super::*;

        legal_move_tests! {
            queen_sould_be_able_to_move_diagnanally_vertically_or_horizontally:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,Some((Color::White, Piece::Queen)),None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![// diag 1
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::G, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::H, rank:Rank::_8}, capture:false, check:false, promotion:None },

                    // diag 2
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::A, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::G, rank:Rank::_1}, capture:false, check:false, promotion:None },

                    //horizontal
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::G, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::H, rank:Rank::_4}, capture:false, check:false, promotion:None },

                    // vertical
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_1}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_2}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_6}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_7}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_8}, capture:false, check:false, promotion:None }]),
        }
    }

    mod king_tests {
        use super::*;

        legal_move_tests! {
            king_should_be_able_to_one_square_in_any_direction:(GameState {
                board :[
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,Some((Color::White, Piece::King)),None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                    [None,None,None,None,None,None,None,None],
                ],
                to_move :Color::White,
                previous_moves:vec![],
            }, vec![Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_5}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_4}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_3}, capture:false, check:false, promotion:None },
                    Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None }]),
        }
    }
}
