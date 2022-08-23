extern crate num;
extern crate num_derive;
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive)]
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
    new_position : (File, Rank)
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

    pub fn new() -> GameState {
        GameState {
            board : [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move : Color::White
        }
    }

    pub fn get_legal_moves(&self) -> Vec<Move> {

        let create_row = |r:usize| (0..7).map(move |f:usize| match self.board[r][f] {
            Some(piece) => Some(((r,f), piece)),
            _ => None
        });

        (0..7)
        .flat_map(create_row)
        .filter_map(|tuple| {

            match tuple {
                Some((coord, (color, piece))) if color == self.to_move => Some((coord, (color, piece))),
                _ => None
            }
        })
        .flat_map(|tuple| {
            let ((rank, file), (color, piece)) = tuple;


            self.get_legal_moves_for_piece_on_square(
                num::FromPrimitive::from_usize(file).unwrap(),
                num::FromPrimitive::from_usize(rank).unwrap(),
                color,
                piece
            )
        })
        .collect()
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
            let (file, rank) = m.new_position;

            self.board[rank as usize][file as usize].is_none() 
        })
        .collect()
    
        // Can't move to square where piece already is
        // @TODO - this won't work with captures, perhaps can concat with captures???
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::zip;
    use std::fmt::Debug;

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
        }

        // can capture if opponent piece is on diaganal

        // en-passant
    }
}
