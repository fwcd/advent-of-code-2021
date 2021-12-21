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

let threshold = 1000
let part1Step s = { state = step s.state (3 * s.die + 3); die = s.die + 3 }
let part1Play = iterateUntil (fun s -> (max s.state.p1.score s.state.p2.score) >= threshold) part1Step
let loser s = if s.p1.score >= threshold then s.p2 else s.p1

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
let part1 = (part1State.die - 1) * (loser part1State.state).score

printfn "Part 1: %d" part1
