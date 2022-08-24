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
pub struct Move {
    old_position : (File, Rank),
    new_position : (File, Rank) // @TODO - may need to have new piece here to cover pawn promotion
    // capture : bool ?
    // check : bool ?
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

        let create_file = |f:File| (0..7)
            .map(|f| num::FromPrimitive::from_u32(f).unwrap())
            .map(move |r:Rank| match self.get_square_state(f, r) {
                Some(piece) => Some(((f,r), piece)),
                _ => None
        });

        (0..7)
        .map(|f| num::FromPrimitive::from_u32(f).unwrap())
        .flat_map(create_file)
        .filter_map(|tuple| {

            match tuple {
                Some((coord, (color, piece))) if color == self.to_move => Some((coord, (color, piece))),
                _ => None
            }
        })
        .flat_map(|tuple| {
            let ((file, rank), (color, piece)) = tuple;

            self.get_legal_moves_for_piece_on_square(
                file,
                rank,
                color,
                piece
            )
        })
        .collect()
    }

    fn get_square_state(&self, f: File, r: Rank) -> Option<(Color,Piece)> {
        self.board[r as usize][f as usize]
    }

    fn get_legal_moves_for_piece_on_square(&self, f: File, r: Rank, c: Color, p: Piece) -> Vec<Move> {
        assert!(c == self.to_move, "Only pieces of color to move should be able to move");

        match p {
            Piece::Pawn => self.get_legal_pawn_moves(f, r),
            _ => Vec::new()
        }
    }
    
    fn get_legal_pawn_moves(&self, f: File, r: Rank) -> Vec<Move> {

        assert!(r != Rank::_1 && r != Rank::_8, "Pawn can not be on first or last rank");
        
        (match (self.to_move,r) {
            (Color::White, Rank::_2) => {
                vec![Move{old_position: (f,r), new_position: (f, Rank::_3)},
                     Move{old_position: (f,r), new_position: (f, Rank::_4)}]
            },
            (Color::White, rank) => {
                let new_rank = rank as i32 - 1;
                let new_rank = num::FromPrimitive::from_i32(new_rank).unwrap();

                vec![Move{old_position: (f,r), new_position: (f, new_rank)}]
            },
            (Color::Black, Rank::_7) => {
                vec![Move{old_position: (f,r), new_position: (f, Rank::_6)},
                     Move{old_position: (f,r), new_position: (f, Rank::_5)}]
            },
            (Color::Black, rank) => {
                let new_rank = rank as i32 + 1;
                let new_rank = num::FromPrimitive::from_i32(new_rank).unwrap();

                vec![Move{old_position: (f,r), new_position: (f, new_rank)}]
            },
        })
        .into_iter()
        .filter(|&m| { 
            //assert_eq!(m.old_position.file, new_file, "Pawn should only be able to move vertically in non-capture situation");

            println!("Checking if piece between {:?} and {:?}", m.old_position, m.new_position);

            self.get_first_occupied_square(m.old_position, m.new_position).is_none()
        })
        .collect()
    
        // Can't move to square where piece already is
        // @TODO - this won't work with captures, perhaps can concat with captures???
    }

    fn get_first_occupied_square(&self, start: (File,Rank), finish: (File,Rank)) -> Option<(File,Rank)> {

        let get_first_occupied = |files:&mut dyn DoubleEndedIterator<Item=u8>, ranks:&mut dyn DoubleEndedIterator<Item=u8>| 
            files.zip(ranks)
            .skip(1)
            .find_map(|pos| {
                let (file, rank) = pos;

                println!("Checking square state for {:?}{:?}", file, rank);
                let file = num::FromPrimitive::from_u8(file).unwrap();
                let rank = num::FromPrimitive::from_u8(rank).unwrap();

                match self.get_square_state(file, rank) {
                    Some(_) => Some((file, rank)),
                    None => None
                }
            });

        let (start_file, start_rank) = start;
        let (finish_file, finish_rank) = finish;
        
       // @TODO - assert that range is straight line or diagnal
        let files:&mut dyn DoubleEndedIterator<Item=u8> = &mut (start_file as u8..=finish_file as u8);
        let files_rev:&mut dyn DoubleEndedIterator<Item=u8> = &mut (finish_file as u8..=start_file as u8).rev();
        let files_repeat:&mut dyn DoubleEndedIterator<Item=u8> = &mut repeat(start_file as u8);

        print!("Files: ");
        for f in files.skip(0) {
            let f:File = num::FromPrimitive::from_u8(f).unwrap();
            print!("{:?}, ", f);
        }
        print!("\n");

        print!("Files Rev: ");
        for f in files_rev.skip(0) {
            let f:File = num::FromPrimitive::from_u8(f).unwrap();
            print!("{:?}, ", f);
        }
        print!("\n");

        let ranks:&mut dyn DoubleEndedIterator<Item=u8> = &mut (start_rank as u8..=finish_rank as u8);
        let ranks_rev:&mut dyn DoubleEndedIterator<Item=u8> = &mut (finish_rank as u8..=start_rank as u8).rev();
        let ranks_repeat:&mut dyn DoubleEndedIterator<Item=u8> = &mut repeat(start_rank as u8);

        match (finish_file.cmp(&start_file), finish_rank.cmp(&start_rank)) {
            (Ordering::Greater, Ordering::Greater) => { 
                println!("Branch 1"); 
                get_first_occupied(files, ranks) 
            },
            (Ordering::Greater, Ordering::Less) => { 
                println!("Branch 2"); 
                get_first_occupied(files, ranks_rev) 
            },
            (Ordering::Greater, Ordering::Equal) => { 
                println!("Branch 3"); 
                get_first_occupied(files, ranks_repeat) 
            },
            (Ordering::Equal, Ordering::Greater) => { 
                println!("Branch 4"); 
                get_first_occupied(files_repeat, ranks) 
            },
            (Ordering::Equal, Ordering::Less) => { 
                println!("Branch 5"); 
                get_first_occupied(files_repeat, ranks_rev) 
            },
            (Ordering::Equal, Ordering::Equal) => { 
                println!("Branch 6"); 
                panic!("Illegal Case")
            },
            (Ordering::Less, Ordering::Greater) => { 
                println!("Branch 7"); 
                get_first_occupied(files_rev, ranks) 
            },
            (Ordering::Less, Ordering::Less) => { 
                println!("Branch 8"); 
                get_first_occupied(files_rev, ranks_rev) 
            },
            (Ordering::Less, Ordering::Equal) => { 
                println!("Branch 9"); 
                get_first_occupied(files_rev, ranks_repeat) 
            },
        }
        /*match (finish_file > start_file, finish_rank > start_rank) { 
            (true, true) => { println!("Branch 1"); get_first_occupied(files, ranks) },
            (false, true) => { println!("Branch 2"); get_first_occupied(files_rev, ranks) },
            (true, false) => { println!("Branch 3"); get_first_occupied(files, ranks_rev) },
            (false, false) => { println!("Branch 4"); get_first_occupied(files_rev, ranks_rev) },
        } */
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
            }, vec![Move{old_position: (File::A, Rank::_2), new_position:(File::A, Rank::_3) },
                    Move{old_position: (File::A, Rank::_2), new_position:(File::A, Rank::_4) }]),

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
            }, vec![Move{old_position: (File::A, Rank::_7), new_position:(File::A, Rank::_6) },
                    Move{old_position: (File::A, Rank::_7), new_position:(File::A, Rank::_5) }]),

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
            }, vec![Move{old_position: (File::A, Rank::_3), new_position:(File::A, Rank::_4) }]),

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
            }, vec![Move{old_position: (File::A, Rank::_6), new_position:(File::A, Rank::_5) }]),

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
        }

        // can capture if opponent piece is on diaganal

        // en-passant
    }
}
