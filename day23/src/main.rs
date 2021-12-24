use std::collections::BinaryHeap;
use std::cmp::{Reverse, Ordering};
use std::str::FromStr;
use std::{fs, fmt};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Board<const N: usize> {
    hallway: [Option<char>; 11],
    rooms: [[Option<char>; N]; 4],
}

#[derive(Debug, Copy, Clone)]
struct State<const N: usize> {
    board: Board<N>,
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

impl<const N: usize> Board<N> {
    fn empty() -> Self {
        Self { hallway: [None; 11], rooms: [[None; N]; 4] }
    }

    fn target() -> Self {
        Self {
            hallway: [None; 11],
            rooms: [[Some('A'); N], [Some('B'); N], [Some('C'); N], [Some('D'); N]],
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

    fn enterable_rooms(self, i: usize, amphipod: char) -> impl Iterator<Item=(usize, usize)> {
        let x = target_x(amphipod);
        self.rooms[x].into_iter()
            .filter(move |_| self.is_targeted_room(x) && self.hallway_free_in(i + (x_to_i(x) as i64 - i as i64).signum() as usize, x_to_i(x)))
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
        self.rooms[x] == [Some(target_amphipod(x)); N]
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

impl<const N: usize> State<N> {
    fn room_leaves(self) -> impl Iterator<Item=State<N>> {
        self.board.leavable_rooms()
            .flat_map(move |(x, y, a)| self.board.enterable_hallway_spots(x).map(move |i| {
                let mut child = self;
                child.board.hallway[i] = child.board.rooms[x][y].take();
                child.energy += move_cost(i, x, y, a);
                child
            }))
    }

    fn room_enters(self) -> impl Iterator<Item=State<N>> {
        self.board.leavable_hallway_spots()
            .flat_map(move |(i, a)| self.board.enterable_rooms(i, a).map(move |(x, y)| {
                let mut child = self;
                child.board.rooms[x][y] = child.board.hallway[i].take();
                child.energy += move_cost(i, x, y, a);
                child
            }))
    }

    fn next_states(self) -> Vec<State<N>> {
        self.room_leaves().chain(self.room_enters()).collect()
    }
}

impl<const N: usize> fmt::Display for Board<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let empty = '.';
        for h in self.hallway.into_iter() {
            write!(f, "{}", h.unwrap_or(empty))?;
        }
        writeln!(f)?;
        for y in 0..N {
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

impl<const N: usize> FromStr for Board<N> {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        let mut board = Board::empty();
        let lines: Vec<_> = s.lines().collect();
        let empty = '.';
        for i in 0..11 {
            board.hallway[i] = Some(lines[1].as_bytes()[i + 1] as char).filter(|&a| a != empty);
        }
        for x in 0..4 {
            for y in 0..N {
                board.rooms[x][y] = Some(lines[2 + y].as_bytes()[3 + 2 * x] as char).filter(|&a| a != empty);
            }
        }
        Ok(board)
    }
}

#[derive(Debug, Copy, Clone)]
struct SearchState<const N: usize> {
    state: State<N>,
    cost_estimate: u64,
}

impl<const N: usize> PartialEq for SearchState<N> {
    fn eq(&self, other: &Self) -> bool {
        self.cost_estimate == other.cost_estimate
    }
}

impl<const N: usize> Eq for SearchState<N> {}

impl<const N: usize> PartialOrd for SearchState<N> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cost_estimate.partial_cmp(&other.cost_estimate)
    }
}

impl<const N: usize> Ord for SearchState<N> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cost_estimate.cmp(&other.cost_estimate)
    }
}

fn shortest_path<const N: usize>(start: Board<N>, target: Board<N>) -> u64 {
    // Use A* search
    let mut heap = BinaryHeap::<Reverse<SearchState<N>>>::new();
    heap.push(Reverse(SearchState { state: State { board: start, energy: 0 }, cost_estimate: 0 }));

    while let Some(Reverse(current)) = heap.pop() {
        if current.state.board == target {
            return current.state.energy;
        }
        for next in current.state.next_states() {
            let target_dist_estimate = next.board.amphipod_dists_to_targets();
            heap.push(Reverse(SearchState { state: next, cost_estimate: next.energy + target_dist_estimate }));
        }
    }

    panic!("No shortest path found!");
}

fn main() {
    let raw = fs::read_to_string("resources/demo.txt").expect("No input file");
    let part1_start = Board::<2>::from_str(&raw).expect("Could not parse board");

    let part1 = shortest_path(part1_start, Board::target());
    println!("Part 1: {}", part1);

    let middle_lines = [
        [Some('D'), Some('D')],
        [Some('C'), Some('B')],
        [Some('B'), Some('A')],
        [Some('A'), Some('C')],
    ];
    let mut part2_start = Board::<4>::empty();
    
    for (x, m) in middle_lines.into_iter().enumerate() {
        part2_start.rooms[x][0] = part1_start.rooms[x][0];
        part2_start.rooms[x][3] = part1_start.rooms[x][1];
        for y in 0..2 {
            part2_start.rooms[x][y + 1] = m[y];
        }
    }

    let part2 = shortest_path(part2_start, Board::target());
    println!("Part 2: {}", part2);
}
