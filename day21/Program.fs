﻿open System.IO

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
  { states: Map<State, uint64> // maps state to count
    p1Won: uint64              // number of games won by player 1
    p2Won: uint64 }            // number of games won by player 2

let rec iterateUntil (p: 'a -> bool) (f: 'a -> 'a) (x: 'a) =
  if p x then x
  else iterateUntil p f (f x)

let chop x = ((x - 1) % 10) + 1
let nextPos pos die = chop (pos + die)
let step die s =
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

let part1Won p = p.score >= 1000
let part1Step ps = { state = step (3 * ps.die + 3) ps.state; die = ps.die + 3 }
let part1Play = iterateUntil (fun s -> part1Won s.state.p1 || part1Won s.state.p2) part1Step
let part1Loser s = if part1Won s.p1 then s.p2 else s.p1

let part2Won p = p.score >= 21
let part2WonStates pf = Map.fold (fun x s c -> x + (if part2Won (pf s) then c else 0UL)) 0UL
let part2RollDie sts sts' die =
  Map.keys sts
    |> Seq.fold (fun sts'' s ->
      let c = Map.find s sts
      sts''
        |> Map.change s (fun x -> Some ((defaultArg x 0UL) - c) |> Option.filter (fun x -> x > 0UL))
        |> Map.change (step die s) (fun x -> Some ((defaultArg x 0UL) + c))) sts'
let part2Step ps =
  printfn "%A" ps
  let states' = (seq { 1..3 }) |> Seq.fold (part2RollDie ps.states) ps.states
  { states = Map.filter (fun s _ -> not (part2Won s.p1 || part2Won s.p2)) states'
    p1Won = ps.p1Won + part2WonStates (fun s -> s.p1) states'
    p2Won = ps.p2Won + part2WonStates (fun s -> s.p2) states'
  }
let part2Play = iterateUntil (fun s -> Map.isEmpty s.states) part2Step

// Main program

let input =
  File.ReadAllText("resources/demo.txt").Split("\n")
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

let part2State = part2Play { states = Map [(initialState, 1UL)]; p1Won = 0UL; p2Won = 0UL }
let part2 = max part2State.p1Won part2State.p2Won

printfn "Part 2: %d" part2
