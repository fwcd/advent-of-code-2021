use std::collections::{BinaryHeap, HashMap};
use std::cmp::{Reverse, Ordering};
use std::str::FromStr;
use std::{fs, fmt};

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

fn target_amphipod(x: usize) -> char {
    (x + ('A' as usize)) as u8 as char
}

fn target_x(amphipod: char) -> usize {
    amphipod as usize - 'A' as usize
}

fn x_to_i(x: usize) -> usize {
    2 * x + 2
}

fn move_cost(i: usize, x: usize, y: usize, amphipod: char) -> u64 {
    (1 + y + abs_diff(x_to_i(x), i)) as u64 * cost(amphipod)
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
            // Skip any rooms that only contain target amphipods
            .filter(move |&(x, _)| !self.is_targeted_room(x))
            // Skip any rooms that are obstructed in the hallway
            .filter(move |&(x, _)| self.hallway[x_to_i(x)].is_none())
            .filter_map(|(x, r)| r.into_iter().enumerate().find_map(|(y, o)| o.map(|a| (x, y, a))))
    }

    fn enterable_rooms(self, amphipod: char) -> impl Iterator<Item=(usize, usize)> {
        let x = target_x(amphipod);
        self.rooms[x].into_iter()
            .filter(move |_| self.is_targeted_room(x))
            .enumerate()
            .filter(move |(_, o)| o.is_none())
            .last()
            .into_iter()
            .map(move |(y, _)| (x, y))
    }

    fn hallway_free_in(self, i1: usize, i2: usize) -> bool {
        (i1.min(i2)..=i1.max(i2))
            .all(|i| self.hallway[i].is_none())
    }

    fn enterable_hallway_spots(self, x: usize) -> impl Iterator<Item=usize> {
        self.hallway.into_iter()
            .enumerate()
            .filter_map(move |(i, _)| if i != x_to_i(x) && self.hallway_free_in(i, x_to_i(x)) { Some(i) } else { None })
    }

    fn leavable_hallway_spots(self) -> impl Iterator<Item=(usize, char)> {
        self.hallway.into_iter().enumerate().filter_map(|(i, o)| o.map(|a| (i, a)))
    }

    fn is_targeted_room(self, x: usize) -> bool {
        self.rooms[x].into_iter().filter_map(|o| o).all(|a| a == target_amphipod(x))
    }

    fn is_completed_room(self, x: usize) -> bool {
        self.rooms[x] == [Some(target_amphipod(x)); 2]
    }

    // Estimate distances for A* search

    fn hallway_amphipods_dists_to_targets(self) -> u64 {
        self.hallway.into_iter()
            .enumerate()
            // TODO: Provide better y estimate than 0
            .filter_map(|(i, o)| o.map(|a| move_cost(i, target_x(a), 0, a)))
            .sum()
    }

    fn room_amphipods_dists_to_targets(self) -> u64 {
        self.rooms.into_iter()
            .enumerate()
            .filter(|&(x, _)| !self.is_completed_room(x))
            .flat_map(|(x, r)| r.into_iter()
                .enumerate()
                .filter_map(move |(y, o)| o.map(|a| {
                    let i = x_to_i(x);
                    let tx = target_x(a);
                    // TODO: Provide better y estimate than 0
                    move_cost(i, x, y, a) + move_cost(abs_diff(i, tx), tx, 0, a)
                })))
            .sum()
    }

    fn amphipod_dists_to_targets(self) -> u64 {
        self.room_amphipods_dists_to_targets() + self.hallway_amphipods_dists_to_targets()
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

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let empty = '.';
        for h in self.hallway.into_iter() {
            write!(f, "{}", h.unwrap_or(empty))?;
        }
        writeln!(f)?;
        for y in 0..self.rooms[0].len() {
            write!(f, " ")?;
            for x in 0..self.rooms.len() {
                write!(f, " ")?;
                write!(f, "{}", self.rooms[x][y].unwrap_or(empty))?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl FromStr for Board {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        let mut board = Board::empty();
        let lines: Vec<_> = s.lines().collect();
        let empty = '.';
        for i in 0..11 {
            board.hallway[i] = Some(lines[1].as_bytes()[i + 1] as char).filter(|&a| a != empty);
        }
        for x in 0..4 {
            for y in 0..2 {
                board.rooms[x][y] = Some(lines[2 + y].as_bytes()[3 + 2 * x] as char).filter(|&a| a != empty);
            }
        }
        Ok(board)
    }
}

#[derive(Debug, Copy, Clone)]
struct SearchState {
    state: State,
    cost_estimate: u64,
}

impl PartialEq for SearchState {
    fn eq(&self, other: &Self) -> bool {
        self.cost_estimate == other.cost_estimate
    }
}

impl Eq for SearchState {}

impl PartialOrd for SearchState {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cost_estimate.partial_cmp(&other.cost_estimate)
    }
}

impl Ord for SearchState {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cost_estimate.cmp(&other.cost_estimate)
    }
}

fn shortest_path(start: Board, target: Board) -> u64 {
    // Use A* search
    let mut heap = BinaryHeap::<Reverse<SearchState>>::new();
    heap.push(Reverse(SearchState { state: State { board: start, energy: 0 }, cost_estimate: 0 }));
    let mut previous = HashMap::<Board, Board>::new();

    while let Some(Reverse(current)) = heap.pop() {
        if current.state.board == target {
            let mut current_board = current.state.board;
            println!("{}", current_board);
            while let Some(next_board) = previous.get(&current_board) {
                println!("{}", next_board);
                current_board = *next_board;
            }
            return current.state.energy;
        }
        for next in current.state.next_states() {
            let target_dist_estimate = next.board.amphipod_dists_to_targets();
            previous.insert(next.board, current.state.board);
            heap.push(Reverse(SearchState { state: next, cost_estimate: next.energy + target_dist_estimate }));
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
