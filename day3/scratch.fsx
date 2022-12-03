
let linesExample = System.IO.File.ReadLines("day3/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day3/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day3/problem2.txt")   |> Seq.toList;;

let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    //if f x then incr i
    if f x then (i.Value<-i.Value+1)
    i.Value)
  |> Seq.map snd

let splitLine (line:string) = line.ToCharArray()|>Array.splitAt(line.Length/2)
let splitLines (lines: string List)=lines|>List.map(fun x->splitLine x)


let makeIntoSetPairs (linePairs:(char array * char array) list) = linePairs|>List.map(fun x-> [fst x|>List.ofSeq; snd x|>List.ofSeq]|>Seq.map Set.ofList);;
let findIntersections setPair=setPair|>List.ofSeq|>List.map(fun x->x|>Seq.reduce Set.intersect)

let exampleSameLetters= linesExample|>splitLines|>makeIntoSetPairs|>findIntersections
let toAscii myChar = int(System.Text.Encoding.ASCII.GetBytes(string myChar)[0])
let toElfCode (mychar:char) =
  if System.Char.IsUpper(mychar) then (toAscii mychar)-38 else (toAscii mychar)-96


let exampleMatchingLetters = exampleSameLetters|>List.map(fun x->(Array.ofSeq x)[0])
let problem1MatchingLetters = linesProblem1|>List.map(fun x->(Array.ofSeq x)[0])

//|>List.map(fun x->toElfCode x)