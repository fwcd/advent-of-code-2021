open System.IO

type Player =
  { pos: int
    score: int }

type State =
  { p1: Player
    p2: Player
    turn: bool
    die: int }

let rec iterateUntil p f x =
  if p x then x
  else iterateUntil p f (f x)

let chop x = ((x - 1) % 10) + 1
let nextPos pos die = chop (pos + 3 * die + 3)
let nextDie die = die + 3

let step s =
  let turn' = not s.turn
  if s.turn then
    let pos' = nextPos s.p1.pos s.die
    { p1 = { pos = pos'
             score = s.p1.score + pos' }
      p2 = s.p2
      die = nextDie s.die
      turn = turn' }
  else
    let pos' = nextPos s.p2.pos s.die
    { p1 = s.p1
      p2 = { pos = pos'
             score = s.p2.score + pos' }
      die = nextDie s.die
      turn = turn' }

let threshold = 1000
let play = iterateUntil (fun s -> (max s.p1.score s.p2.score) >= threshold) step
let loser s = if s.p1.score >= 1000 then s.p2 else s.p1

// Main program

let input = File.ReadAllText("resources/input.txt").Split("\n")
              |> Seq.filter (fun l -> String.length l > 0)
              |> Seq.map (fun l -> (l.Split(":")[1]).Trim() |> int)
              |> Seq.toList

let initialState =
  { p1 = { pos = input[0]
           score = 0 }
    p2 = { pos = input[1]
           score = 0 }
    die = 1
    turn = true
  }
let finalState = play initialState
let part1 = (finalState.die - 1) * (loser finalState).score

printfn "Part 1: %d" part1
