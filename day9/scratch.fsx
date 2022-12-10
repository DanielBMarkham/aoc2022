// HOUSEKEEPING/COMMON
let linesExample = System.IO.File.ReadLines("day9/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day9/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day9/problem2.txt")   |> Seq.toList;;

let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    if f x then (i.Value<-i.Value+1)
    i.Value)
  |> Seq.map snd

let getNeighbor<'a> (arr:'a [,]) x y :'a option  =
  try Some arr[x,y] with _->None
let getNeighbors<'a> (myWorld:'a [,]) (myPoint:int*int) (neighborsToGet:(int*int) list):'a list =
  let rootX,rootY = myPoint
  neighborsToGet|>List.map(fun x->
    let newX = rootX + fst x
    let newY = rootY + snd x
    getNeighbor<'a> myWorld newX  newY
  ) |>List.choose id
let find2D needle (arr: 'a [,]) = 
    let rec go x y =
          if   y >= arr.GetLength 1 then None
          elif x >= arr.GetLength 0 then go 0 (y+1)
          elif arr.[x,y] = needle   then Some (x,y)
          else go (x+1) y
    go 0 0

let splitStateFromInstructions (incomingData:list<string>) = incomingData |> splitBy(fun x->x.Length=0) |>List.ofSeq
let getInitialStateStrings (incomingData:list<string>) = (splitStateFromInstructions incomingData)[0] |> List.ofSeq |> List.filter(fun x->x.Length<>0)
let getInitialMovesString (incomingData:list<string>) = (splitStateFromInstructions incomingData)[1] |> List.ofSeq|> List.filter(fun x->x.Length<>0)

type GridCell= |Empty|Head|Tail|Both 
let getInitialState (incomingData:list<string>) =
  let initialStateStrings=getInitialStateStrings (incomingData:list<string>)
  let initialStateCharacterGrid = array2D (initialStateStrings|>List.map(fun x->x|>List.ofSeq)) //|>List.map(fun y-> int (string y)))) 
  initialStateCharacterGrid 
    |>Array2D.map(fun x->
      match x with 
        | '.'->Empty
        | 'H' ->Head
        | 'T' ->Tail 
        |_-> Both
    ) |> Array2D.map(fun x->if x=Head then Both else x) // start at both

type MoveDirection = |Left|Right|Up|Down
let moveDirectionToDeltaPoint (move:MoveDirection) =
  match move with |Up->(-1,0) |Right->(0,1) |Left->(0,-1) |Down->(1,0)
let addPoints (ptA:int*int) (ptB:int*int) =
  (fst ptA+fst ptB, snd ptA + snd ptB)
let moveInDirectionFromPoint (point:int*int)   (move:MoveDirection) =
  let delta =moveDirectionToDeltaPoint move 
  addPoints delta point
let getInitialInstructions (incomingData:list<string>) =
  let initialInstructionsStrings=getInitialMovesString incomingData
  initialInstructionsStrings |> List.map(fun x->
    let moveSplit=x.Split([|' '|])
    let moveCount = int moveSplit[1]
    let moveType =
      match moveSplit[0] with 
        |"U"->Up 
        |"D"->Down
        |"L"->Left 
        |"R"->Right
        |_->Up
    (moveType,moveCount)
    )
  |>List.map(fun x->List.init (snd x) (fun y-> (fst x)) )|>List.concat

let setGridCellToToken (grid:GridCell [,]) (tokenToSet:GridCell) (targetLocation:int*int) = 
  grid|>Array2D.mapi(fun i j x->if (i,j) = targetLocation then tokenToSet else x)
let newLocation (oldLocation:int*int) (moveDir:MoveDirection):int*int = 
  match moveDir with 
    |Left->(fst oldLocation, snd oldLocation-1)
    |Right->(fst oldLocation, snd oldLocation+1)
    |Up->(fst oldLocation-1, snd oldLocation)
    |Down->(fst oldLocation+1, snd oldLocation)


type RelationshipHeadtoTail = Overlapping|AdjacentInRowOrColumn|AdjacentInDiagonal|TwoSpacesAwayInRowOrColumn|Other
let getNewTailLocation (pointTail:int*int) (pointHead:int*int):(int*int) =
  let delta= (fst pointHead - fst pointTail, snd pointHead - snd pointTail)
  let headTailRelationship =
    match delta with 
    | (0,0)->Overlapping
    |x when x=(-1,-1) || x=(-1,1) || x=(1,-1) || x=(1,1) ->AdjacentInDiagonal
    |x when x=(0,-1) || x=(0,1) || x=(1,0) || x=(-1,0)->AdjacentInRowOrColumn
    |x when x=(0,-2) || x=(0,2) || x=(2,0) || x=(-2,0)->TwoSpacesAwayInRowOrColumn
    |_->Other
  match headTailRelationship with 
    |Overlapping->pointTail // no move
    |AdjacentInRowOrColumn->pointTail //no move
    |AdjacentInDiagonal->pointTail // diagonal is okay 
    |TwoSpacesAwayInRowOrColumn->
      let wayToMove=((fst delta)/2, (snd delta)/2)
      addPoints pointTail wayToMove
    |Other->
      let generalDirection:(int*int) =
        (System.Math.Sign (fst delta) , System.Math.Sign (snd delta))
      addPoints pointTail generalDirection

let foo=getInitialState linesExample
let bar=getInitialInstructions linesExample 
let prob1Instruction=getInitialInstructions linesProblem1

let listDelts=prob1Instruction |> List.map(fun x->moveDirectionToDeltaPoint x)
let headHist n =
  listDelts |> List.take n
  |>List.fold(fun (acc:List<int*int>) (x:int*int)->
    let pointToProcess=(acc|>List.head)
    printfn "current point to process %A" pointToProcess
    let newPoint = (addPoints x pointToProcess)
    printfn "Add to %A result %A " x newPoint
    List.append [newPoint] acc ) [(0,0)]
  |>List.rev


let procStep n =
  headHist n
  |>List.fold(fun (acc:((int*int)*(int*int)) list) (x:int*int)->
      let head = x 
      let oldTail = snd (acc|>List.rev|>List.head)
      let newTail = getNewTailLocation oldTail head
      let newPair=( head, newTail)
      let newAcc=List.append acc [newPair]
      newAcc
      ) [((0,0),(0,0))]