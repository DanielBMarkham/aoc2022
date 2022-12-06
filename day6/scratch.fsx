
let linesExample = System.IO.File.ReadLines("day6/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day6/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day6/problem1.txt")   |> Seq.toList;;

let lastNCharactersDifferent (n:int) (s:string):bool =
  try s.Substring(s.Length-n).ToCharArray()|>Array.distinct|>Array.length=n with _->false
let progressivelyBiggerChunksOfString (s:string) =
  s.ToCharArray()|>Array.mapi(fun i x->s.Substring(0,i+1))

let prob1 x=x|>progressivelyBiggerChunksOfString|>Array.mapi(fun i x->(i,x,lastNCharactersDifferent 4 x))|>Array.find(fun (a,b,c)->c)

let prob2 x=x|>progressivelyBiggerChunksOfString|>Array.mapi(fun i x->(i,x,lastNCharactersDifferent 14 x))|>Array.find(fun (a,b,c)->c)


// One liner using windowed views
let findFirstDistinct (windowSize:int) (s:string) = (s.ToCharArray()|>Array.windowed windowSize |>Array.find(fun x->x|>Array.distinct|>Array.length=windowSize)|>System.String|>s.IndexOf) + windowSize
linesExample|>List.map(fun x->findFirstDistinct 4 x)
