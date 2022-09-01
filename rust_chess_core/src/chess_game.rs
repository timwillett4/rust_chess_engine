#[cfg(test)]
mod legal_move_tests;

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

impl Color {
    pub fn other(&self) -> Color {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White
        }
    }
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

impl Pos {

    pub fn is_on_same_file(&self, other:&Pos) -> bool {
        self.file == other.file
    }

    pub fn is_on_same_rank(&self, other:&Pos) -> bool {
        self.rank == other.rank
    }

    pub fn is_on_same_diagonal(&self, other:&Pos) -> bool {
        let file_offset = self.file as i32 - other.file as i32;
        let rank_offset = self.rank as i32 - other.rank as i32;
    
        file_offset.abs() == rank_offset.abs()
    }

    pub fn is_on_same_rank_file_or_diagnanol(&self, other:&Pos) -> bool {
        self.is_on_same_file(other) || self.is_on_same_rank(other) || self.is_on_same_diagonal(other)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Move {
    old_position:Pos,
    new_position:Pos,
    capture:bool,
    check:bool,
    promotion:Option<Piece>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ChessGame {
    board:[[Option<(Color,Piece)>; 8]; 8],
    to_move:Color,
    previous_moves:Vec<Move>,
    // @TODO - should be able to calculate below o from just
    // previous move list but probably won't be most optimal way 
    // @TODO - move count since pawn move/capture
    // @TODO -all previous hashes to check for 3 move repition draw
}

impl ChessGame {

    pub fn new() -> ChessGame {
        ChessGame {
            board:[
                [Some((Color::Black, Piece::Rook)),Some((Color::Black, Piece::Knight)),Some((Color::Black, Piece::Bishop)),Some((Color::Black, Piece::Queen)),Some((Color::Black, Piece::King)),Some((Color::Black, Piece::Bishop)),Some((Color::Black, Piece::Knight)),Some((Color::Black, Piece::Pawn))],
                [Some((Color::Black, Piece::Pawn)),Some((Color::Black, Piece::Pawn)),Some((Color::Black, Piece::Pawn)),Some((Color::Black, Piece::Pawn)),Some((Color::Black, Piece::Pawn)),Some((Color::Black, Piece::Pawn)),Some((Color::Black, Piece::Pawn)),Some((Color::Black, Piece::Pawn))],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [None,None,None,None,None,None,None,None],
                [Some((Color::White, Piece::Pawn)),Some((Color::White, Piece::Pawn)),Some((Color::White, Piece::Pawn)),Some((Color::White, Piece::Pawn)),Some((Color::White, Piece::Pawn)),Some((Color::White, Piece::Pawn)),Some((Color::White, Piece::Pawn)),Some((Color::White, Piece::Pawn))],
                [Some((Color::White, Piece::Rook)),Some((Color::White, Piece::Knight)),Some((Color::White, Piece::Bishop)),Some((Color::White, Piece::Queen)),Some((Color::White, Piece::King)),Some((Color::White, Piece::Bishop)),Some((Color::White, Piece::Knight)),Some((Color::White, Piece::Pawn))],
            ],
            to_move:Color::White,
            previous_moves:vec![],
        }
    }

    pub fn get_legal_moves(&self) -> Vec<Move> {

        self.get_all_moves()
            .into_iter()
            .map(|m| {
                let mut updated = self.apply_move(&m);
                updated.to_move = self.to_move;

                let can_capture_king = |m:&Move| self.get_square_state(&m.new_position) == Some((self.to_move.other(), Piece::King));

                match updated.get_all_moves().into_iter().any(|m| can_capture_king(&m)) {
                    true => Move{check:true,..m},
                    false => m
                }
            })
            .collect()
            /*
            .filter(|m| {
                self.apply_move(&m)
                    // @TODO - this strategy isn't going to work as it
                    // creates endless recursive method
                    // maybe need to extract private method first
                    .get_legal_moves_for_piece_on_square(&m.new_position) // this is incorrec we want to know if opponents, pieces
                    .into_iter()
                    .filter(|mv| mv.capture)
                    .all(|mv| matches!(mv, Move{newself.get_square_state(&mv.new_position) != Some((self.to_move, Piece::King)))
            })
            .collect()*/
    }

    fn get_all_moves(&self) -> Vec<Move> {
        let create_ranks = || (0..8).map(|r| num::FromPrimitive::from_u32(r).expect(&String::from("Expected 0 to 7 to be able to be mapped to a rank")));

        let files = (0..8).map(|f| num::FromPrimitive::from_u32(f).expect(&String::from("Expected 0 to 7 to be able to be mapped to a file")));
        
        let squares = files.flat_map(|file:File| create_ranks().map(move |rank:Rank| Pos{file, rank}));

        let squares_occupied_by_to_move = squares.filter_map(|pos| match self.get_square_state(&pos) {
            Some((color,_piece)) if color == self.to_move => Some(pos),
            _ => None
        });

        squares_occupied_by_to_move.flat_map(|pos| self.get_legal_moves_for_piece_on_square(&pos)).collect()
    }

    pub fn get_square_state(&self, pos:&Pos) -> Option<(Color,Piece)> {
        self.board[pos.rank as usize][pos.file as usize]
    }

    fn set_square_state(&mut self, pos:&Pos, piece: Option<(Color, Piece)>) {
        self.board[pos.rank as usize][pos.file as usize] = piece;
    }

    fn get_legal_moves_for_piece_on_square(&self, pos:&Pos) -> Vec<Move> {
        let piece = self.get_square_state(pos);

        let (color, piece) = piece.expect("Attempting to get legal moves for square with no piece on it");
        assert!(color == self.to_move, "Can not move opponents piece");

        match piece {
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
            let move_dist = |m:&Move| (m.new_position.rank as i32 - m.old_position.rank as i32).abs();

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

        // Bishop moves is looked at by finding 4 diaganols the piece can move to.
        //
        // Note: Up to 2 diagnols could be the same as the starting pos if the piece on
        // an edge or corner
        //
        // To move diagnolly an equal offset magnitued (i.e. abs value) must be applied to 
        // to both the file and rank of pos.
        // 
        // The four corners are determined, then by alternating the four possibilities of
        // - and + in a pairwise manner
        // 
        // Ex: the following shows each as positive or negative with respect to (rank,file)
        //      TL(-,-)         TR(+,-)
        //                 x
        //      BL(-,+)         TR(+,+)
        //
        //
        // If the move is a negative move we are finding the biggest offset that ensures either file or
        // rank does not go below zero
        //
        // If the move is a positive move we are finding the biggest offset that ensures either file or rank
        // does not go above 7 (the maximum value for a rank/file)
        //
        // Ex:  D4 = (3,4)
        // 
        // To calcualte top left you need to apply a negative offset in both directions, thus 3 is going to be the
        // largest offset that can be applied where both file and rank don't go below 0.
        //
        // This give top_left position of (3,4) - (3,3) = (0,1) = A7
        //
        // To calcualte bottom right you need to apply a positve offset in both directions, thus 3 is going to be the
        // largest offset that can be applied where both file and rank don't over 7.
        //
        // This give bottom_left position of (3,4) + (3,3) = (6,7) = G1
        let rank_offset = pos.rank as i32;
        let file_offset = pos.file as i32;

        // (-,-)
        let top_left_offset = std::cmp::min(rank_offset, file_offset);
        let top_left_file = num::FromPrimitive::from_i32(pos.file as i32 - top_left_offset).unwrap();
        let top_left_rank = num::FromPrimitive::from_i32(pos.rank as i32 - top_left_offset).unwrap();
        let top_left = Pos{file:top_left_file, rank:top_left_rank, };

        // (+,+)
        let top_right_offset = std::cmp::min(7 - rank_offset, 7 - file_offset);
        let bottom_right_file = num::FromPrimitive::from_i32(pos.file as i32 + top_right_offset).unwrap();
        let bottom_right_rank = num::FromPrimitive::from_i32(pos.rank as i32 + top_right_offset).unwrap();
        let bottom_right = Pos{file:bottom_right_file, rank:bottom_right_rank};

        // (-,+)
        let bottom_left_offset = std::cmp::min(file_offset, 7 - rank_offset);
        let bottom_left_file = num::FromPrimitive::from_i32(pos.file as i32 - bottom_left_offset).unwrap();
        let bottom_left_rank = num::FromPrimitive::from_i32(pos.rank as i32 + bottom_left_offset).unwrap();
        let bottom_left = Pos{file:bottom_left_file, rank:bottom_left_rank};

        // (+,-)
        let top_right_offset = std::cmp::min(7 - file_offset, rank_offset);
        let top_right_file = num::FromPrimitive::from_i32(pos.file as i32 + top_right_offset).unwrap();
        let top_right_rank = num::FromPrimitive::from_i32(pos.rank as i32 - top_right_offset).unwrap();
        let top_right = Pos{file:top_right_file, rank:top_right_rank};

        self.get_legal_moves_between_squares(&pos, &top_left)
        .into_iter()
        .chain(self.get_legal_moves_between_squares(&pos, &bottom_left))
        .chain(self.get_legal_moves_between_squares(&pos, &top_right))
        .chain(self.get_legal_moves_between_squares(&pos, &bottom_right))
        .collect()
    }

    fn get_legal_rook_moves(&self, pos:&Pos) -> Vec<Move> {
        
        let top = Pos{file: pos.file, rank:Rank::_1};
        let bottom = Pos{file: pos.file, rank:Rank::_8};
        let left = Pos{file: File::A, rank:pos.rank};
        let right = Pos{file: File::H, rank:pos.rank};

        self.get_legal_moves_between_squares(&pos, &top)
        .into_iter()
        .chain(self.get_legal_moves_between_squares(&pos, &bottom))
        .chain(self.get_legal_moves_between_squares(&pos, &left))
        .chain(self.get_legal_moves_between_squares(&pos, &right))
        .collect()
    }

    fn get_legal_queen_moves(&self, pos:&Pos) -> Vec<Move> {

        // queen can mvoe either as bishop or rook so just combine the two
        self.get_legal_bishop_moves(&pos)
            .into_iter()
            .chain(self.get_legal_rook_moves(&pos))
            .collect()
    }

    fn get_legal_king_moves(&self, pos:&Pos) -> Vec<Move> {
       let possible_moves = [(-1,0),(-1,1),(-1,-1),(0,1),(0,-1),(1,1),(1,0),(1,-1)];
       self.get_legal_moves_from_offset_list(&pos, &mut possible_moves.into_iter())
    }

    /// get_legal_moves_from_offset_list takes a starting position and a list of i32 pairs representing
    /// file and rank offset respectively.
    /// 
    /// It then returns a list of valid moves directly going from start position and applying the offset.
    /// 
    /// It is intended for knight/king movement where a list of direct offsets rather than a range is given 
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

    /// get_legal_moves_between_squares takes a starting position and an ending position and returns
    /// a list of legal moves between those 2 squares.
    /// 
    // Remarks: Start and end must either be on the same diagnol, rank, or file
    fn get_legal_moves_between_squares(&self, start:&Pos, end:&Pos) -> Vec<Move> {

        assert!(start.is_on_same_rank_file_or_diagnanol(end), "Start and end must be on the same file, rank, or diagnal");

        let end = self.get_first_occupied_square(&start, &end).unwrap_or(*end);

        ChessGame::get_positions_between(&start, &end)
            .into_iter()
            .map(|new_pos| Move{old_position:*start, new_position:new_pos, check:false, capture: false, promotion: None})
            .filter_map(|mov| match self.get_square_state(&mov.new_position) {
                Some((Color::White, _)) => None,
                Some((Color::Black, _)) => Some(Move{capture:true,..mov}),
                None => Some(mov)
            })
            .collect()
    }

    fn get_first_occupied_square(&self, start:&Pos, finish:&Pos) -> Option<Pos> {

        ChessGame::get_positions_between(start, finish)
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
    fn get_positions_between(start:&Pos, end:&Pos) -> Vec<Pos> {

        assert!(start.is_on_same_rank_file_or_diagnanol(end), "Start and end must be on the same file, rank, or diagnal");

        let get_positions = |files:&mut dyn DoubleEndedIterator<Item=u8>, ranks:&mut dyn DoubleEndedIterator<Item=u8>| 
            files.zip(ranks)
            .map(|pos| {
                let (file, rank) = pos;

                let file = num::FromPrimitive::from_u8(file).unwrap();
                let rank = num::FromPrimitive::from_u8(rank).unwrap();

                Pos{file, rank}
            })
            .collect();

        let files:&mut dyn DoubleEndedIterator<Item=u8> = &mut (start.file as u8..=end.file as u8);
        let files_rev:&mut dyn DoubleEndedIterator<Item=u8> = &mut (end.file as u8..=start.file as u8).rev();
        let files_repeat:&mut dyn DoubleEndedIterator<Item=u8> = &mut repeat(start.file as u8);

        let ranks:&mut dyn DoubleEndedIterator<Item=u8> = &mut (start.rank as u8..=end.rank as u8);
        let ranks_rev:&mut dyn DoubleEndedIterator<Item=u8> = &mut (end.rank as u8..=start.rank as u8).rev();
        let ranks_repeat:&mut dyn DoubleEndedIterator<Item=u8> = &mut repeat(start.rank as u8);

        // @TODO - assert is straight line or diagnal
        match (end.file.cmp(&start.file), end.rank.cmp(&start.rank)) {
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

    // updates
    fn apply_move(&self, mv:&Move) -> ChessGame {
        let piece_to_move = self.get_square_state(&mv.old_position);

        assert!(matches!(piece_to_move, Some((color, _piece)) if color == self.to_move), "Attempting an invalid move");

        let mut new_game = ChessGame {
            to_move: self.to_move.other(),
            ..
            self.clone()
        };

        new_game.set_square_state(&mv.old_position, None);
        new_game.set_square_state(&mv.new_position, piece_to_move);
        
        match mv.promotion {
            Some(promotion_piece) => new_game.set_square_state(&mv.new_position, Some((self.to_move, promotion_piece))),
            None => new_game.set_square_state(&mv.new_position, piece_to_move)
        }
        // @TODO - en_passant

        new_game
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
