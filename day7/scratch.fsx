
let linesExample = System.IO.File.ReadLines("day7/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day7/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day7/problem2.txt")   |> Seq.toList;;

type FileData= string*int
type LineType = |CommandCD of string|CommandLS|Directory of string|File of FileData
let getDesiredDirectory (Directory x) = x
let getDesiredFileInfo (File x) =x
let getFileName (File dat)= (fst dat)
let getFileSize (File dat) = (snd dat)
type TerminalSession = List<LineType>

let processIntoTerminalHistory (linesInput: string list):TerminalSession =
  linesInput 
  |> List.map(fun x->
  match x[0] with
    |'$'->
      let removePrompt=x[2..]
      let splitLine=removePrompt.Split([|' '|])
      if splitLine[0]="ls" then CommandLS else CommandCD(splitLine[1])
    |_->
      if x[0..2]="dir"
        then Directory(x[4..])
        else 
          let splitLine=x.Split([|' '|])
          File(splitLine[1],int splitLine[0])
  )


let example1TerminalSession = processIntoTerminalHistory linesExample
let problem1TerminalSession = processIntoTerminalHistory linesProblem1

let addWorkingDirectoryToTerminalHistory (incoming:TerminalSession) = 
  incoming
  |>List.map(fun x->
    match x with 
      |CommandCD cd->(cd,x)
      |_->("",x)
    )
  |>List.mapFold(fun acc (x:string*LineType)->
    match snd x  with 
      | CommandCD cdc  ->
        match cdc with 
          |"/"->
            printfn "Back to root : %A" cdc 
            (x,"/")
          |".."->
            let lastSlashIndex=acc.LastIndexOf("/")
            let newDir=acc.Substring(0,lastSlashIndex-1)
            printfn "up a directory : %A" newDir
            (x,newDir)
          |_->
            let newDir= acc + cdc + "/" 
            printfn "into a directory %A %A" acc newDir
            (x, newDir)
      |_->
        printfn "No directory change, current dir  %A" acc
        ((acc, snd x),acc)
    ) ""
  |> fst |> List.map(fun (lineToProc:(string*LineType))->
    let currentDirectory,currentTerminalLine=lineToProc
    match currentTerminalLine with 
      |File f->Some (currentDirectory,f)
      |_->None
    )
  |> List.choose id


// let foo=addWorkingDirectoryToTerminalHistory example1TerminalSession|> List.sortBy(fun x-> fst x) |> List.groupBy fst
// let bar (baz:(string * (string*FileData)) list) = (snd baz[0], fst baz[0])
// let grom1=snd foo[0]|>List.map(fun x->snd x)
// let gromN (n:int)=snd foo[n]|>List.map(fun x->snd x)
// let grom2=snd foo[0]|>List.map(fun x->snd x)|>List.sumBy(fun x->snd x)
// let grom2N (n:int)=snd foo[n]|>List.map(fun x->snd x)|>List.sumBy(fun x->snd x)

// let dirAndFileSizes=foo|>List.mapi(fun i x->((fst x),(grom2N i)))
// let directorySizes=dirAndFileSizes|>List.map(fun (dirName,dirSize)->(dirName,dirAndFileSizes|>List.filter(fun (a,b)->a.Contains dirName)|>List.sumBy(fun x->snd x)))
// let example1Answer=directorySizes|>List.filter(fun (dir,size)->size<100000)|>List.sumBy(fun x->snd x)

let fooProb1=addWorkingDirectoryToTerminalHistory problem1TerminalSession|> List.sortBy(fun x-> fst x) |> List.groupBy fst
let fooDebugProb1 (n:int) = addWorkingDirectoryToTerminalHistory (problem1TerminalSession |> List.take n) |> List.sortBy(fun x-> fst x) |> List.groupBy fst
//let barProb1 (baz:(string * (string*FileData)) list) = (snd baz[0], fst baz[0])
//let grom2Prob1=snd fooProb1[0]|>List.map(fun x->snd x)|>List.sumBy(fun x->snd x)
let sumUpFileSizesForADirectoryIndex (n:int)=snd fooProb1[n]|>List.map(fun x->snd x)|>List.sumBy(fun x->snd x)

let dirAndFileSizesProb1=fooProb1|>List.mapi(fun i x->((fst x),(sumUpFileSizesForADirectoryIndex i)))
let directorySizesProb1=dirAndFileSizesProb1|>List.map(fun (dirName,dirSize)->(dirName,dirAndFileSizesProb1|>List.filter(fun (a,b)->a.Contains dirName)|>List.sumBy(fun x->snd x)))
let answerProb1=directorySizesProb1|>List.filter(fun (dir,size)->size<100000)|>List.sumBy(fun x->snd x)


let condenseToTuplesForADirectoryIndex (n:int)=snd fooProb1[n]|>List.map(fun x->snd x)
let makeIntoDirectoryFileSizeList incoming = 
  (processIntoTerminalHistory incoming) 
  |> addWorkingDirectoryToTerminalHistory 
  |> List.groupBy fst
  |> List.map snd
  |>List.map(fun x->x|>List.map(fun (a,b)->a,getFileName (File b),getFileSize (File b)))
  |> List.concat

let directoriesAndTotalSizes incoming =
  let tempList=makeIntoDirectoryFileSizeList incoming
  tempList
    |>List.map(fun (a,b,c)->(a,tempList|>List.filter(fun (x,y,z)->x.Contains a)|>List.sumBy(fun (x,y,z)->z)))
    |>List.distinct


let example1 = directoriesAndTotalSizes linesExample|>List.filter(fun x->snd x<=100000)|>List.sumBy(fun x->snd x)
let foo=directoriesAndTotalSizes linesProblem1
let printTestDirs=foo|>List.map(fun x->fst x)
let printSubDirs (dir:string)=printTestDirs|>(List.filter(fun x->x.Contains dir))|>List.append [dir]

//printSubDirs "/cmvqf/dcgbjvddnclwtgccnrqwzphrvqwnjssmwcn/cqdzdnq/"