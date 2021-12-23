use std::{fs, fmt};

enum Ampiphod {
    A, B, C, D
}

enum Block {
    Wall, Space, Ampiphod(Ampiphod)
}

struct Grid {
    elements: Vec<Block>,
    width: usize,
    height: usize
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
                write!(f, "{}", self.elements[y * self.width + x]);
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

fn parse_block(raw: char) -> Block {
    match raw {
        '.' => Block::Space,
        'A' => Block::Ampiphod(Ampiphod::A),
        'B' => Block::Ampiphod(Ampiphod::B),
        'C' => Block::Ampiphod(Ampiphod::C),
        'D' => Block::Ampiphod(Ampiphod::D),
        _ => Block::Wall,
    }
}

fn parse_grid(raw: &str) -> Grid {
    let mut grid = Grid { elements: Vec::new(), width: 0, height: 0 };
    for line in raw.lines() {
        if grid.width == 0 {
            grid.width = line.len();
        }
        for c in line.chars() {
            grid.elements.push(parse_block(c));
        }
        while grid.elements.len() % grid.width != 0 {
            grid.elements.push(Block::Wall);
        }
        grid.height += 1;
    }
    grid
}

fn main() {
    let raw_input = fs::read_to_string("resources/demo.txt").expect("No input");
    let grid = parse_grid(&raw_input);
    println!("{}", grid);
}
