
let linesExample = System.IO.File.ReadLines("day1/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day1/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day1/problem2.txt")   |> Seq.toList;;

let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    //if f x then incr i
    if f x then (i.Value<-i.Value+1)
    i.Value)
  |> Seq.map snd

let chunksExample=linesExample |> splitBy(fun x->x.Length=0);;
let elfSnacksExample=chunksExample|>Seq.map(fun x->x|>Seq.filter(fun x->x.Length<>0) |>Seq.sumBy(fun x->int x));;

let chunksProblem1=linesProblem1 |> splitBy(fun x->x.Length=0);;
let elfSnacksProblem1=chunksProblem1|>Seq.map(fun x->x|>Seq.filter(fun x->x.Length<>0) |>Seq.sumBy(fun x->int x));;

let topThreeElves=elfSnacksProblem1|>Seq.sort;;
//topThreeElves|>Seq.rev|>Seq.take 3