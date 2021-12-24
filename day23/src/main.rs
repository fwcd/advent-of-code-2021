use std::str::FromStr;
use std::{fs, fmt, io};
use std::ops::{Index, IndexMut};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Amphipod {
    A, B, C, D
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Block {
    Wall, Space, Amphipod(Amphipod)
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct Grid {
    elements: Vec<Block>,
    width: usize,
    height: usize,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct State {
    grid: Grid,
    energy: usize,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct Game {
    start: Grid,
    target: Grid
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Pos {
    y: usize,
    x: usize,
}

impl Pos {
    fn new(y: usize, x: usize) -> Self {
        Self { y, x }
    }

    fn neighbors(&self) -> [Pos; 4] {
        [
            Pos::new(self.y - 1, self.x),
            Pos::new(self.y + 1, self.x),
            Pos::new(self.y, self.x - 1),
            Pos::new(self.y, self.x + 1),
        ]
    }
}

impl Grid {
    fn from_file(path: &str) -> Result<Self, String> {
        let raw = fs::read_to_string(path).map_err(|e| format!("Could not read file: {:?}", e))?;
        Ok(Grid::from_str(&raw).unwrap())
    }
}

impl Game {

}

impl Index<Pos> for Grid {
    type Output = Block;

    fn index(&self, pos: Pos) -> &Block {
        &self.elements[pos.y * self.width + pos.x]
    }
}

impl IndexMut<Pos> for Grid {
    fn index_mut(&mut self, pos: Pos) -> &mut Block {
        &mut self.elements[pos.y * self.width + pos.x]
    }
}

impl fmt::Display for Amphipod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Amphipod::A => write!(f, "A"),
            Amphipod::B => write!(f, "B"),
            Amphipod::C => write!(f, "C"),
            Amphipod::D => write!(f, "D"),
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Block::Space => write!(f, "."),
            Block::Wall => write!(f, "#"),
            Block::Amphipod(amp) => write!(f, "{}", amp)
        }
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for y in 0..self.height {
            for x in 0..self.width {
                write!(f, "{}", self[Pos::new(y, x)])?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl From<char> for Block {
    fn from(raw: char) -> Block {
        match raw {
            '.' => Block::Space,
            'A' => Block::Amphipod(Amphipod::A),
            'B' => Block::Amphipod(Amphipod::B),
            'C' => Block::Amphipod(Amphipod::C),
            'D' => Block::Amphipod(Amphipod::D),
            _ => Block::Wall,
        }
    }
}

impl FromStr for Grid {
    type Err = ();

    fn from_str(raw: &str) -> Result<Grid, ()> {
        let mut grid = Grid { elements: Vec::new(), width: 0, height: 0 };
        for line in raw.lines() {
            if grid.width == 0 {
                grid.width = line.len();
            }
            for c in line.chars() {
                grid.elements.push(Block::from(c));
            }
            while grid.elements.len() % grid.width != 0 {
                grid.elements.push(Block::Wall);
            }
            grid.height += 1;
        }
        Ok(grid)
    }
}

fn main() {
    let start = Grid::from_file("resources/demo.txt").unwrap();
    let target = Grid::from_file("resources/target.txt").unwrap();
    let game = Game { start, target };
    println!("{}", game.target);
}
