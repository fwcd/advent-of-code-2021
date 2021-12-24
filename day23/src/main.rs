use std::str::FromStr;
use std::fs;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Board {
    hallway: [Option<char>; 11],
    rooms: [[Option<char>; 2]; 4],
}

impl Board {
    fn empty() -> Self {
        Self { hallway: [None; 11], rooms: [[None; 2]; 4] }
    }

    fn target() -> Self {
        Self {
            hallway: [None; 11],
            rooms: [[Some('A'); 2], [Some('B'); 2], [Some('C'); 2], [Some('D'); 2]],
        }
    }
}

impl FromStr for Board {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        let mut board = Board::empty();
        let lines: Vec<_> = s.lines().collect();
        for x in 0..4 {
            for y in 0..2 {
                board.rooms[x][y] = Some(lines[2 + y].as_bytes()[3 + 2 * x] as char);
            }
        }
        Ok(board)
    }
}

fn main() {
    let raw = fs::read_to_string("resources/demo.txt").expect("No input file");
    let start = Board::from_str(&raw).expect("Could not parse board");
    println!("{:?}", start);
}
