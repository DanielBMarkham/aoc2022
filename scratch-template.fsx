
let linesExample = System.IO.File.ReadLines("dayX/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("dayX/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("dayX/problem2.txt")   |> Seq.toList;;

let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    //if f x then incr i
    if f x then (i.Value<-i.Value+1)
    i.Value)
  |> Seq.map snd

