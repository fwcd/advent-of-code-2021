open System.IO

type Player =
  { pos: int
    score: int }

type State =
  { p1: Player
    p2: Player
    die: int }

let chop x = ((x - 1) % 10) + 1
let step s =
  let pos1' = chop (s.p1.pos + 3 * s.die + 3)
  let pos2' = chop (s.p2.pos + 3 * s.die + 12)
  let die'  = chop (s.die + 6)
  { p1  = { pos = pos1'
            score = s.p1.score + pos1' }
    p2  = { pos = pos2'
            score = s.p2.score + pos2' }
    die = die' }

let input = File.ReadAllText("resources/demo.txt").Split("\n")
              |> Seq.filter (fun l -> String.length l > 0)
              |> Seq.map (fun l -> (l.Split(":")[1]).Trim() |> int)

printfn "%s" (input |> Seq.map string |> String.concat "+")
