open System.IO

type Player =
  { pos: int
    score: int }

type State =
  { p1: Player
    p2: Player
    turn: bool }

type Part1State =
  { state: State
    die: int }

type Part2State =
  { states: Map<State, int> // maps state to count
    p1Won: int              // number of games won by player 1
    p2Won: int }            // number of games won by player 2

let rec iterateUntil p f x =
  if p x then x
  else iterateUntil p f (f x)

let chop x = ((x - 1) % 10) + 1
let nextPos pos die = chop (pos + die)
let step s die =
  let turn' = not s.turn
  if s.turn then
    let pos' = nextPos s.p1.pos die
    { p1 = { pos = pos'
             score = s.p1.score + pos' }
      p2 = s.p2
      turn = turn' }
  else
    let pos' = nextPos s.p2.pos die
    { p1 = s.p1
      p2 = { pos = pos'
             score = s.p2.score + pos' }
      turn = turn' }

let part1Threshold = 1000
let part1Step s = { state = step s.state (3 * s.die + 3); die = s.die + 3 }
let part1Play = iterateUntil (fun s -> (max s.state.p1.score s.state.p2.score) >= part1Threshold) part1Step
let part1Loser s = if s.p1.score >= part1Threshold then s.p2 else s.p1

let part2Threshold = 21
let part2Play s = s
// TODO

// Main program

let input =
  File.ReadAllText("resources/input.txt").Split("\n")
    |> Seq.filter (fun l -> String.length l > 0)
    |> Seq.map (fun l -> (l.Split(":")[1]).Trim() |> int)
    |> Seq.toList

let initialState =
  { p1 = { pos = input[0]
           score = 0 }
    p2 = { pos = input[1]
           score = 0 }
    turn = true }

let part1State = part1Play { state = initialState; die = 1 }
let part1 = (part1State.die - 1) * (part1Loser part1State.state).score

printfn "Part 1: %d" part1

let part2State = part2Play { states = Map [(initialState, 1)]; p1Won = 0; p2Won = 0 }
let part2 = max part2State.p1Won part2State.p2Won

printfn "Part 2: %d" part2
