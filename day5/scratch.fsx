
let linesExample = System.IO.File.ReadLines("day5/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day5/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day5/problem1.txt")   |> Seq.toList;;
let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    //if f x then incr i
    if f x then (i.Value<-i.Value+1)
    i.Value)
  |> Seq.map snd
let truncateOne (incomingList:List<'a>) = incomingList|>List.truncate (incomingList.Length-1)

// COMMON
let splitInput (lines:List<string>)=lines|>splitBy(fun x->x.Length=0)|>List.ofSeq
let initialStacks lines=(splitInput lines)[0]|>List.ofSeq|>truncateOne
let moveInstructions lines=(splitInput lines)[1]|>Seq.tail|>List.ofSeq
let setupInitialStackSparseArray (initialStackList:string list)=
  initialStackList
  |>List.rev |> List.map(fun x->x.ToCharArray())
  |>List.mapi(fun i x->x|>Array.mapi(fun j y->if System.Char.IsAsciiLetter(y) then Some (i,j,y) else None)|>Array.choose id |> List.ofArray)
  |>List.map(fun x->x|>List.map(fun (a,b,c)->(a,((b-1)/4)),c ))
  |>List.concat

let setupInitialStackLists (initSparseArray:(((int*int)*char) list)) = 
  let stackCount=initSparseArray|>List.map(fun ((a,b),c)->b) |> List.max
  initSparseArray
  |>List.sortBy(fun ((a,b),c)->a)
  |>List.sortBy(fun ((a,b),c)->b)
  |>List.groupBy(fun ((a,b),c)->b)
  |>List.map(fun (a,b)->
    b) |>List.map(fun x->x|>List.map(fun (a,b)->b))

let initialStackList (lines:List<string>)=setupInitialStackLists (setupInitialStackSparseArray (initialStacks lines))

let translateMoveInstruction (sInstruction:string) = 
  let split = sInstruction.Split([|' '|])
  let numberToMove= int split[1]
  let fromStack = int split[3]
  let toStack = int split[5]
  (numberToMove,fromStack,toStack)

let initialInstructions (lines:List<string>) =
  lines
    |>List.map(fun x->translateMoveInstruction x)
    |>List.map(fun (a,b,c)-> Seq.init a (fun x->(b,c))) |>Seq.concat|>List.ofSeq

// EXAMPLE
let moveCrate (crates:char list list) (englishStackNumberFrom:int) (englishStackNumberto:int) =
  let stackFrom=englishStackNumberFrom-1
  let stackTo=englishStackNumberto-1
  let itemToMove=crates[stackFrom]|>List.rev|>List.head
  let newFromStack=crates[stackFrom]|>truncateOne
  let newToStack=List.append  crates[stackTo] [itemToMove]
  crates|>List.mapi(fun i x->
    match i with 
      |n when n=stackFrom->newFromStack
      |n when n=stackTo->newToStack
      |_->x
    )
let executeMoveInstruction crates (instruction:int*int) =
  moveCrate crates (fst instruction) (snd instruction)
let example1Instructions=initialInstructions (moveInstructions linesExample)
let example1Crates = initialStackList linesExample
let playGameExample1=
  example1Instructions
  |> List.fold(fun acc x->executeMoveInstruction acc x) example1Crates
// Get the answer
playGameExample1 |> List.map(fun x->x|>List.rev|>List.head)

// PROBLEM 1
let problem11Instructions=initialInstructions (moveInstructions linesProblem1)
let problem1Crates = initialStackList linesProblem1
let playGameProblem1=
  problem11Instructions
  |> List.fold(fun acc x->executeMoveInstruction acc x) problem1Crates
// Get the answer
playGameProblem1|>List.map(fun x->x|>List.rev|>List.head);;

// PROBLEM 2
let moveCrateExample2 (crates:char list list) (iCount:int) (englishStackNumberFrom:int) (englishStackNumberto:int) =
  let stackFrom=englishStackNumberFrom-1
  let stackTo=englishStackNumberto-1
  let itemsToMove=crates[stackFrom]|>List.rev|>List.take iCount
  let newFromStack=crates[stackFrom]|>List.rev|>List.skip iCount |>List.rev
  let newToStack=List.append  crates[stackTo] (itemsToMove|>List.rev)
  crates|>List.mapi(fun i x->
    match i with 
      |n when n=stackFrom->newFromStack
      |n when n=stackTo->newToStack
      |_->x
    )
let initialInstructionsExample2 (lines:List<string>) =
  lines
    |>List.map(fun x->translateMoveInstruction x)

let instructionsExampleForProblem2=initialInstructionsExample2 (moveInstructions linesExample)
let problem2Instructions = initialInstructionsExample2 (moveInstructions linesProblem2)
let problem2Crates = initialStackList linesProblem2
let executeMoveInstructionProblem2 crates ((a,b,c):int*int*int) =
  moveCrateExample2 crates a b c
let playGameProblem2=
  problem2Instructions
  |> List.fold(fun acc x->executeMoveInstructionProblem2 acc x) problem2Crates
// Get the answer
playGameProblem2|>List.map(fun x->x|>List.rev|>List.head);;


// test walk-thru function
let pop n=
  (instructionsExampleForProblem2 |> List.take n)
  |> List.fold(fun acc x->executeMoveInstructionProblem2 acc x) example1Crates
