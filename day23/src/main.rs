use std::collections::BinaryHeap;
use std::cmp::{Reverse, Ordering};
use std::str::FromStr;
use std::fs;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Board {
    hallway: [Option<char>; 11],
    rooms: [[Option<char>; 2]; 4],
}

#[derive(Debug, Copy, Clone)]
struct State {
    board: Board,
    energy: u64,
}

fn abs_diff(x: usize, y: usize) -> usize {
    (y as i64 - x as i64).abs() as usize
}

fn cost(amphipod: char) -> u64 {
    match amphipod {
        'A' => 1,
        'B' => 10,
        'C' => 100,
        'D' => 1000,
        _ => panic!("Invalid amphipod: {}", amphipod),
    }
}

fn move_cost(i: usize, x: usize, y: usize, amphipod: char) -> u64 {
    (1 + y + abs_diff(2 * x + 2, i)) as u64 * cost(amphipod)
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

    fn leavable_rooms(self) -> impl Iterator<Item=(usize, usize, char)> {
        self.rooms.into_iter()
            .enumerate()
            .filter_map(|(x, r)| r.into_iter().enumerate().find_map(|(y, o)| o.map(|a| (x, y, a))))
    }

    fn enterable_rooms(self, amphipod: char) -> impl Iterator<Item=(usize, usize)> {
        let x = amphipod as usize - 'A' as usize;
        self.rooms[x].into_iter()
            .enumerate()
            .filter_map(move |(y, o)| if o.is_none() { Some((x, y)) } else { None })
    }

    fn enterable_hallway_spots(self, x: usize) -> impl Iterator<Item=usize> {
        (0..self.hallway.len()).filter(move |&i| i != 2 * x + 2)
    }

    fn leavable_hallway_spots(self) -> impl Iterator<Item=(usize, char)> {
        self.hallway.into_iter().enumerate().filter_map(|(i, o)| o.map(|a| (i, a)))
    }
}

impl State {
    fn room_leaves(self) -> impl Iterator<Item=State> {
        self.board.leavable_rooms()
            .flat_map(move |(x, y, a)| self.board.enterable_hallway_spots(x).map(move |i| {
                let mut child = self;
                child.board.hallway[i] = child.board.rooms[x][y].take();
                child.energy += move_cost(i, x, y, a);
                child
            }))
    }

    fn room_enters(self) -> impl Iterator<Item=State> {
        self.board.leavable_hallway_spots()
            .flat_map(move |(i, a)| self.board.enterable_rooms(a).map(move |(x, y)| {
                let mut child = self;
                child.board.rooms[x][y] = child.board.hallway[i].take();
                child.energy += move_cost(i, x, y, a);
                child
            }))
    }

    fn next_states(self) -> Vec<State> {
        self.room_leaves().chain(self.room_enters()).collect()
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.energy == other.energy
    }
}

impl Eq for State {}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.energy.partial_cmp(&other.energy)
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        self.energy.cmp(&other.energy)
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

fn shortest_path(start: Board, target: Board) -> u64 {
    // Use Dijkstra search
    let mut heap = BinaryHeap::<Reverse<State>>::new();
    heap.push(Reverse(State { board: start, energy: 0 }));

    while let Some(Reverse(current)) = heap.pop() {
        if current.board == target {
            return current.energy;
        }
        for next in current.next_states() {
            heap.push(Reverse(next));
        }
    }

    panic!("No shortest path found!");
}

fn main() {
    let raw = fs::read_to_string("resources/demo.txt").expect("No input file");
    let start = Board::from_str(&raw).expect("Could not parse board");
    let part1 = shortest_path(start, Board::target());
    println!("Part 1: {}", part1);
}
