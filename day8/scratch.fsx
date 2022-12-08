
let linesExample = System.IO.File.ReadLines("day8/example.txt")   |> Seq.toList |> List.filter(fun x->x.Length>0)
let linesProblem1 = System.IO.File.ReadLines("day8/problem1.txt")   |> Seq.toList |> List.filter(fun x->x.Length>0)
let linesProblem2 = System.IO.File.ReadLines("day8/problem2.txt")   |> Seq.toList |> List.filter(fun x->x.Length>0)

let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    //if f x then incr i
    if f x then (i.Value<-i.Value+1)
    i.Value)
  |> Seq.map snd

let getNeighbor<'a> (arr:'a [,]) x y :'a option  =
  try Some arr[x,y] with _->None
let getNeighbors<'a> (myWorld:'a [,]) (myPoint:int*int) (neighborsToGet:(int*int) list) =
  let rootX,rootY = myPoint
  neighborsToGet|>List.map(fun x->
    let newX = rootX + fst x
    let newY = rootY + snd x
    fun x->getNeighbor<'a> myWorld newX  newY
  )
let lookLeft (arr:'a [,]) x y = arr[0..x-1,y]
let lookRight (arr:'a [,]) x y = arr[x+1..,y]
let LookUp (arr:'a [,]) x y = arr[x,0..y-1]
let LookDown (arr:'a [,]) x y = arr[x,y+1..]
let rowsAndColumns (arr:'a [,]) x y =
  [(lookLeft arr x y); (lookRight arr x y); (LookUp arr x y); (LookDown arr x y)];
let example1IncomingData=array2D (linesExample|>List.map(fun x->x|>List.ofSeq|>List.map(fun y-> int (string y)))) 
let problem11IncomingData=array2D (linesProblem1|>List.map(fun x->x|>List.ofSeq|>List.map(fun y-> int (string y)))) 
//getNeighbor<int> foo 1 1;;

let insideGrid (grid:'a [,]) = 
  let rightSide=grid|>Array2D.length1
  let bottomSide=grid|>Array2D.length2
  grid[1..rightSide-2,1..bottomSide-2]

let borderCount (grid:'a [,])=
  let width=grid|>Array2D.length1
  let heigth=grid|>Array2D.length2
  width*2 + (heigth-2)*2

let canBeSeenFromOutside (grid:int [,]) x y =
  let valToCompare= grid[x,y]
  let highestToLeft = try valToCompare > ((lookLeft grid x y) |> List.ofArray |> List.max) with _->true
  let highestToRight = try valToCompare > ((lookRight grid x y) |> List.ofArray |> List.max) with _->true
  let highestToTop = try valToCompare > ((LookUp grid x y) |> List.ofArray |> List.max) with _->true
  let highestToBottom = try valToCompare > ((LookDown grid x y) |> List.ofArray |> List.max) with _->true
  highestToLeft || highestToRight || highestToTop || highestToBottom

let example1TruthGrid =example1IncomingData |> Array2D.mapi(fun i j x-> (canBeSeenFromOutside example1IncomingData i  j) ) |>Seq.cast<bool> |>Seq.map(fun x->x)
let problem1TruthGrid =problem11IncomingData |> Array2D.mapi(fun i j x-> (canBeSeenFromOutside problem11IncomingData i  j) ) |>Seq.cast<bool> |>Seq.map(fun x->x)


