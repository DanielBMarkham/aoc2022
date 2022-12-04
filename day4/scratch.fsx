
let linesExample = System.IO.File.ReadLines("day4/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day4/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day4/problem1.txt")   |> Seq.toList;;

let makeIntoListOfSets (s:string) = 
  try
    s.Split([|','|])|>List.ofArray|>List.map(fun x->x.Split([|'-'|])|>List.ofArray)|>List.map(fun x->[int x[0]..int x[1]])|>List.map(fun x->Set.ofList x)
  with _->[]
let isAnySetInListASubsetOfAnother (setList:Set<'a> list) = 
  let smallestSetLength:int=setList|>List.map(fun x->x.Count)|>List.min
  let intersection=setList |> List.reduce Set.intersect
  smallestSetLength=intersection.Count

let example1=linesExample|>List.map(fun x->(makeIntoListOfSets x)|>isAnySetInListASubsetOfAnother)|>List.countBy(fun x->x)

let prob1=linesProblem1|>List.map(fun x->(makeIntoListOfSets x)|>isAnySetInListASubsetOfAnother)|>List.countBy(fun x->x)

let prob2b=linesProblem2|>List.map(fun x->(makeIntoListOfSets x))|>List.map(fun x->x|>List.reduce Set.intersect)|>List.countBy(fun x->x.Count>0)
