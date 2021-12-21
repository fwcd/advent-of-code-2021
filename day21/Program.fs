open System.IO

type Player =
  { pos: int
    score: int }

type State =
  { p1: Player
    p2: Player
    die: int }

// TODO: Consider case where first player wins
let chop x = ((x - 1) % 10) + 1
let step s =
  let pos1' = chop (s.p1.pos + 3 * s.die + 3)
  let pos2' = chop (s.p2.pos + 3 * s.die + 12)
  let die'  = s.die + 6
  { p1 = { pos = pos1'
           score = s.p1.score + pos1' }
    p2 = { pos = pos2'
           score = s.p2.score + pos2' }
    die = die' }

let rec iterateUntil p f x =
  if p x then x
  else iterateUntil p f (f x)

let input = File.ReadAllText("resources/demo.txt").Split("\n")
              |> Seq.filter (fun l -> String.length l > 0)
              |> Seq.map (fun l -> (l.Split(":")[1]).Trim() |> int)
              |> Seq.toList

let initialState =
  { p1 = { pos = input[0]
           score = 0 }
    p2 = { pos = input[1]
           score = 0 }
    die = 1
  }
let finalState = iterateUntil (fun s -> (max s.p1.score s.p2.score) >= 1000) step initialState

printfn "Part 1: %d" (finalState.die)
