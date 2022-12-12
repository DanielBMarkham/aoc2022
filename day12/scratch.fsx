// HOUSEKEEPING/COMMON
let linesExample = System.IO.File.ReadLines("day12/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day12/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day12/problem2.txt")   |> Seq.toList;;

let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    //if f x then incr i
    if f x then (i.Value<-i.Value+1)
    i.Value)
  |> Seq.map snd

let stringListToCharArray (lines:string list)=lines|>List.filter(fun x->x.Length>0)|>List.map(fun x->x.ToCharArray()|>List.ofArray)|>array2D;
let getNeighbor<'a> (arr:'a [,]) x y :'a option  =
  try Some arr[x,y] with _->None
let getNeighborAndCoordinate<'a> (arr:'a [,]) x y :('a*(int*int)) option  =
  try Some (arr[x,y],(x,y)) with _->None
let getNeighbors<'a> (myWorld:'a [,]) (myPoint:int*int) (neighborsToGet:(int*int) list):'a list =
  let rootX,rootY = myPoint
  neighborsToGet|>List.map(fun x->
    let newX = rootX + fst x
    let newY = rootY + snd x
    getNeighbor<'a> myWorld newX  newY
  ) |>List.choose id
let getNeighborsAndCoordinates<'a> (myWorld:'a [,]) (myPoint:int*int) (neighborsToGet:(int*int) list):('a*(int*int)) list =
  let rootX,rootY = myPoint
  neighborsToGet|>List.map(fun x->
    let newX = rootX + fst x
    let newY = rootY + snd x
    getNeighborAndCoordinate<'a> myWorld newX  newY
  ) |>List.choose id

let getPointsSurroundingAPoint<'a> (myWorld:'a [,]) (myPoint:int*int) =
  getNeighbors<'a> myWorld myPoint [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(0,1);(1,1)]
let getPointsAndCoordinatesSurroundingAPoint<'a> (myWorld:'a [,]) (myPoint:int*int) =
  getNeighborsAndCoordinates<'a> myWorld myPoint [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(0,1);(1,1)]
let getRowColumnPointsAndCoordinatesSurroundingAPoint<'a> (myWorld:'a [,]) (myPoint:int*int) =
  getNeighborsAndCoordinates<'a> myWorld myPoint [(-1,0);(0,-1);(0,1);(1,0)]


let find2D needle (arr: 'a [,]) = 
    let rec go x y =
          if   y >= arr.GetLength 1 then None
          elif x >= arr.GetLength 0 then go 0 (y+1)
          elif arr.[x,y] = needle   then Some (x,y)
          else go (x+1) y
    go 0 0

let foo=linesExample |> stringListToCharArray |> Array2D.map(fun x->if x='S'then 'a' else x) |> Array2D.map(fun x->if x='E' then 'z' else x)

let pointsICanGoTo (myWorld:char [,]) (myPoint:int*int) = 
  //printfn "I am looking at point %A. It is a %A" myPoint myWorld[fst myPoint,snd myPoint]
  let pointsAndCoordinatesAroundMe=getRowColumnPointsAndCoordinatesSurroundingAPoint<char> myWorld myPoint
  printfn "point coords around me %A" pointsAndCoordinatesAroundMe
  let ret= if pointsAndCoordinatesAroundMe.Length>0 then   pointsAndCoordinatesAroundMe|>List.filter(fun x->int (fst x)<=(int (fst x)+1)) else []
  //printfn "Points that match the height criteria %A" ret
  ret

let isPointWinner charA = charA='z'

let pointToPoints (myWorld:char [,]) (myPoint:int*int) = 
  if isPointWinner (myWorld[fst myPoint,snd myPoint])
  then []
  else 
    let temp=pointsICanGoTo myWorld myPoint
    let ret =pointsICanGoTo myWorld myPoint
    ret |>List.map(fun x->snd x)

