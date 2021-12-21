open System.IO

let input = File.ReadAllText("resources/demo.txt").Split("\n")
              |> Seq.filter (fun l -> String.length l > 0)
              |> Seq.map (fun l -> (l.Split(":")[1]).Trim() |> int)

printfn "%s" (input |> Seq.map string |> String.concat "+")
