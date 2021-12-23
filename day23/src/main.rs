use std::str::FromStr;
use std::{fs, fmt};
use std::ops::{Index, IndexMut};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Ampiphod {
    A, B, C, D
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Block {
    Wall, Space, Ampiphod(Ampiphod)
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct Grid {
    elements: Vec<Block>,
    width: usize,
    height: usize,
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

impl fmt::Display for Ampiphod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ampiphod::A => write!(f, "A"),
            Ampiphod::B => write!(f, "B"),
            Ampiphod::C => write!(f, "C"),
            Ampiphod::D => write!(f, "D"),
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Block::Space => write!(f, "."),
            Block::Wall => write!(f, "#"),
            Block::Ampiphod(amp) => write!(f, "{}", amp)
        }
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for y in 0..self.height {
            for x in 0..self.width {
                write!(f, "{}", self[Pos::new(y, x)]);
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl From<char> for Block {
    fn from(raw: char) -> Block {
        match raw {
            '.' => Block::Space,
            'A' => Block::Ampiphod(Ampiphod::A),
            'B' => Block::Ampiphod(Ampiphod::B),
            'C' => Block::Ampiphod(Ampiphod::C),
            'D' => Block::Ampiphod(Ampiphod::D),
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
    let raw_input = fs::read_to_string("resources/demo.txt").expect("No input");
    let grid = Grid::from_str(&raw_input).expect("Invalid grid");
    println!("{}", grid);
}
