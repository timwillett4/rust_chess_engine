use super::*;
use std::fmt::Debug;

// @TODO - move to generic helper libray
fn are_equivelent<T>(mut actual: Vec<T>, mut expected: Vec<T>) -> bool
where
    T: Debug + PartialEq + Ord,
{
    if actual.len() != expected.len() {
        println!(
            "Length of actual ({:?}) does not equal length of expected ({:?})",
            actual.len(),
            expected.len()
        );
        false
    } else {
        actual.sort();
        expected.sort();
        actual.into_iter().zip(expected.into_iter()).all(|tuple| {
            let (actual, expected) = tuple;

            actual == expected
        })
    }
}

// Given a board state, to move and previous moves, assert against an expected set of legal moves
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

mod is_check_tests {
    use super::*;
}

mod basic_pawn_movement_tests {
    use super::*;

    legal_move_tests! {
        white_pawn_should_be_able_to_move_one_or_two_squares_on_initial_move: (ChessGame{
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![]
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None }]),

        black_pawn_should_be_able_to_move_one_or_two_squares_on_initial_move: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::Black,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_6}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check:false, promotion:None }]),

        white_pawn_should_be_able_to_move_one_squares_after_initial_move: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None }]),

        black_pawn_should_be_able_to_move_one_squares_after_initial_move: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::Black,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check:false, promotion:None }]),

        white_pawn_should_not_be_able_to_move_if_blocked_by_another_piece: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec!()),

        black_pawn_should_not_be_able_to_move_if_blocked_by_another_piece: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::Black,
            previous_moves: vec![],
        }, vec!()),

        white_pawn_should_not_be_able_to_jump_over_another_piece: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec!()),

        black_pawn_should_not_be_able_to_jump_over_another_piece: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::Black,
            previous_moves: vec![],
        }, vec!()),

        white_pawn_should_be_able_to_capture_on_diaganol: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::B, rank:Rank::_4}, capture:true, check:false, promotion:None }]),

        black_pawn_should_be_able_to_capture_on_diaganol: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,Some((Color::White, Piece::Pawn)),None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::Black,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:true, check:false, promotion:None }]),

        white_pawn_should_not_be_able_to_capture_same_color: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,Some((Color::White, Piece::Pawn)),None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::B, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:false, check:false, promotion:None }]),

        black_pawn_should_not_be_able_to_capture_same_color: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::Black,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_6}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::B, rank:Rank::_5}, new_position:Pos{file:File::B, rank:Rank::_4}, capture:false, check:false, promotion:None }]),

        white_pawn_should_be_able_to_promote_if_on_7th_rank: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Knight) },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Bishop) },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Rook) },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Queen) }]),

        black_pawn_should_be_able_to_promote_if_on_2nd_rank: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::Black,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Knight) },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Bishop) },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Rook) },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Queen) }]),

        white_pawn_should_be_able_to_promote_when_capturing_on_8th_rank: (ChessGame {
            board: [
                [None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Knight) },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Bishop) },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Rook) },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::A, rank:Rank::_8}, capture:false, check:false, promotion:Some(Piece::Queen) },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture:true, check:false, promotion:Some(Piece::Knight) },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture:true, check:false, promotion:Some(Piece::Bishop) },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture:true, check:false, promotion:Some(Piece::Rook) },
                Move{old_position:Pos{file:File::A, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_8}, capture:true, check:false, promotion:Some(Piece::Queen) }]),

        black_pawn_should_be_able_to_promote_when_capturing_on_1st_rank: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None,None],
                [None, Some((Color::White, Piece::Pawn)),None,None,None,None,None,None],
            ],
            to_move: Color::Black,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Knight) },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Bishop) },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Rook) },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_1}, capture:false, check:false, promotion:Some(Piece::Queen) },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture:true, check:false, promotion:Some(Piece::Knight) },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture:true, check:false, promotion:Some(Piece::Bishop) },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture:true, check:false, promotion:Some(Piece::Rook) },
                Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::B, rank:Rank::_1}, capture:true, check:false, promotion:Some(Piece::Queen) }]),

        white_pawn_should_be_able_to_capture_en_passant: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![Move{old_position:Pos{file:File::B, rank:Rank::_7}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:false, check:false, promotion:None }],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_5}, new_position:Pos{file:File::A, rank:Rank::_6}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_5}, new_position:Pos{file:File::B, rank:Rank::_6}, capture:true, check:false, promotion:None }]),

        black_pawn_should_be_able_to_capture_en_passant: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),Some((Color::Black, Piece::Pawn)),None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::Black,
            previous_moves: vec![Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None }],
        }, vec![Move{old_position:Pos{file:File::B, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::B, rank:Rank::_4}, new_position:Pos{file:File::A, rank:Rank::_3}, capture:true, check:false, promotion:None }]),

        white_pawn_move_marked_as_check: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,Some((Color::Black, Piece::King)),None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![Move{old_position:Pos{file:File::A, rank:Rank::_2}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:false, promotion:None }],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_3}, new_position:Pos{file:File::A, rank:Rank::_4}, capture:false, check:true, promotion:None }]),
    }
}

mod basic_knight_movement_tests {
    use super::*;

    legal_move_tests! {
        knight_should_be_able_to_move_in_l_shape: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,Some((Color::White, Piece::Knight)),None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_6}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_5}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_2}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_2}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_6}, capture:false, check:false, promotion:None }]),

        knight_can_not_move_off_edge_of_board: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,Some((Color::White, Piece::Knight)),None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::A, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::D, rank:Rank::_2}, capture:false, check:false, promotion:None }]),

        knight_can_not_land_on_own_piece: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black,Piece::Pawn)),None,None,None,None,None,None,None],
                [Some((Color::White,Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,Some((Color::White, Piece::Knight)),None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::D, rank:Rank::_2}, capture:false, check:false, promotion:None }]),

        knight_can_capture_opponent_piece: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::Black,Piece::Pawn)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,Some((Color::White, Piece::Knight)),None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::A, rank:Rank::_3}, capture:true, check:false, promotion:None },
                Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::B, rank:Rank::_1}, new_position:Pos{file:File::D, rank:Rank::_2}, capture:false, check:false, promotion:None }]),
        // @TODO - check tests
    }
}

mod basic_bishop_movement_tests {
    use super::*;

    legal_move_tests! {
        bishop_should_be_able_to_move_on_diaganol_from_center_of_board: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,Some((Color::White, Piece::Bishop)),None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
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
        bishop_should_be_able_to_move_on_diaganol_from_left_side_of_board: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Bishop)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::B, rank:Rank::_5}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_2}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::C, rank:Rank::_6}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_1}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_7}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_8}, capture:false, check:false, promotion:None }]),
    }

    legal_move_tests! {
        bishop_should_be_able_to_move_on_diaganol_from_right_side_of_board: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,Some((Color::White, Piece::Bishop))],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::G, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::G, rank:Rank::_5}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_2}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::F, rank:Rank::_6}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_1}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::E, rank:Rank::_7}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::H, rank:Rank::_4}, new_position:Pos{file:File::D, rank:Rank::_8}, capture:false, check:false, promotion:None }]),
    }

    legal_move_tests! {
        bishop_should_be_able_to_move_on_diaganol_from_top_of_board: (ChessGame {
            board: [
                [None,None,None,Some((Color::White, Piece::Bishop)),None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::C, rank:Rank::_7}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::E, rank:Rank::_7}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::B, rank:Rank::_6}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::F, rank:Rank::_6}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::A, rank:Rank::_5}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::G, rank:Rank::_5}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::D, rank:Rank::_8}, new_position:Pos{file:File::H, rank:Rank::_4}, capture:false, check:false, promotion:None }]),
    }

    legal_move_tests! {
        bishop_should_be_able_to_move_on_diaganol_from_diaganol: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Bishop)),None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::B, rank:Rank::_2}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::D, rank:Rank::_4}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::E, rank:Rank::_5}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::F, rank:Rank::_6}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::G, rank:Rank::_7}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::H, rank:Rank::_8}, capture:false, check:false, promotion:None }]),
    }

    legal_move_tests! {
        bishop_should_not_be_able_to_jump_over_own_piece: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,Some((Color::White, Piece::Pawn)),None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Bishop)),None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::B, rank:Rank::_2}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::C, rank:Rank::_3}, new_position:Pos{file:File::C, rank:Rank::_4}, capture:false, check:false, promotion:None }]),
    }

    legal_move_tests! {
        bishop_should_to_capture_opponent_piece: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,Some((Color::Black, Piece::Pawn)),None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Bishop)),None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::B, rank:Rank::_2}, capture:false, check:false, promotion:None },
                Move{old_position:Pos{file:File::A, rank:Rank::_1}, new_position:Pos{file:File::C, rank:Rank::_3}, capture:true, check:false, promotion:None }]),
    }
    // @TODO
    // check
}

mod basic_rook_movement_tests {
    use super::*;

    legal_move_tests! {
        rook_should_be_able_to_move_vertically_or_horizontally:(ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,Some((Color::White, Piece::Rook)),None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
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
        rook_should_not_be_able_to_jump_over_own_piece: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,Some((Color::White, Piece::Rook)),Some((Color::White,Piece::Pawn)),None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
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
        rook_should_be_able_to_capture_opponent_piece:(ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,Some((Color::White, Piece::Rook)),Some((Color::Black,Piece::Pawn)),None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
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

    legal_move_tests! {
        rook_should_be_able_to_capture_opponent_piece_on_1st_rank:(ChessGame {
            board: [
                [Some((Color::Black, Piece::Rook)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Rook)),None,None,None,None,None,None,None],
            ],
            to_move: Color::Black,
            previous_moves: vec![],
        }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::B, rank:Rank::_8}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::C, rank:Rank::_8}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::D, rank:Rank::_8}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::E, rank:Rank::_8}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::F, rank:Rank::_8}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::G, rank:Rank::_8}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::H, rank:Rank::_8}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::A, rank:Rank::_7}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::A, rank:Rank::_6}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::A, rank:Rank::_5}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::A, rank:Rank::_4}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::A, rank:Rank::_3}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::A, rank:Rank::_2}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_8}, new_position: Pos{file:File::A, rank:Rank::_1}, capture: true, check: false, promotion: None }]),
    }
}

mod basic_queen_movement_tests {
    use super::*;

    legal_move_tests! {
        queen_sould_be_able_to_move_diagnanally_vertically_or_horizontally: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,Some((Color::White, Piece::Queen)),None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
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

mod basic_king_movement_tests {
    use super::*;

    legal_move_tests! {
        king_should_be_able_to_one_square_in_any_direction: (ChessGame {
            board: [
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,Some((Color::White, Piece::King)),None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
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

mod can_not_leave_king_in_check_tests {
    use super::*;

    legal_move_tests! {
        rook_should_not_be_able_to_move_when_pinned:(ChessGame {
            board: [
                [Some((Color::Black, Piece::Rook)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Rook)),None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::King)),None,None,None,None,None,None,None],
            ],
            to_move: Color::White,
            previous_moves: vec![],
        }, vec![Move{old_position: Pos{file:File::A, rank:Rank::_4}, new_position: Pos{file:File::A, rank:Rank::_8}, capture: true, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_4}, new_position: Pos{file:File::A, rank:Rank::_7}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_4}, new_position: Pos{file:File::A, rank:Rank::_6}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_4}, new_position: Pos{file:File::A, rank:Rank::_5}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_4}, new_position: Pos{file:File::A, rank:Rank::_4}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_4}, new_position: Pos{file:File::A, rank:Rank::_3}, capture: false, check: false, promotion: None },
                Move{old_position: Pos{file:File::A, rank:Rank::_4}, new_position: Pos{file:File::A, rank:Rank::_2}, capture: false, check: false, promotion: None },
 
                Move{old_position: Pos{file:File::A, rank:Rank::_1}, new_position: Pos{file:File::B, rank:Rank::_1}, capture: false, check: false, promotion: None }]),
    }
}

// castle tests
// stalemate tests
// checkmate tests
// draws - 50 move, 3-fold repetition