let linesExample = System.IO.File.ReadLines("warmup/example.tsv")   |> Seq.toList;;
let fieldsExample =linesExample|>List.map(fun x->x.Split [|'\t'|])

type MyCat =
  val Name:string
  new(?cName:string)=match cName with |Some name->{Name=name} | None->{Name="Joe"}

let sendCatToVet (theCat:MyCat) = printfn "I have sent %s to the vet" theCat.Name

type MyDog<'a> =
  Name=""
  new(newName:'a) =
    match typeof<'a> with 
      | "string"->{Name=newName}
      |_->{Name="Bozo"  }

type Change = {Value:string;EventDateTime:System.DateTime}
type TIAVDEntry =
  {
    Type: string
    Instance: string
    Attribute:string
    Initial:Change
  }
type TIAVDHistoryItem = 
  {
    Type: string
    Instance: string
    Attribute:string
    Changes: Change list
  }
type TIAVDHistoryList = TIAVDHistoryItem list
let processedLines = 
  List.choose id (fieldsExample |> List.map(fun eachLine->
    try 
      Some {Type=eachLine[0];Instance=eachLine[1];Attribute=eachLine[2];Initial={Value=eachLine[3];EventDateTime=System.DateTime.Parse eachLine[4]}}
      with _-> None
  ))
let sortedIncoming=processedLines |> List.sortBy(fun x->x.Type+x.Instance+x.Attribute)

let groupByTrackedItem = sortedIncoming|>List.groupBy(fun x->x.Type+x.Instance+x.Attribute)
let changeHistories=
  groupByTrackedItem|>List.map(fun x->
    let firstItemInGroup=(snd x)[0]
    let changeList=(snd x) |> List.map(fun y->{Value=y.Initial.Value;EventDateTime=y.Initial.EventDateTime}) |> List.sortBy(fun z->z.EventDateTime)
    {
      Type= firstItemInGroup.Type
      Instance= firstItemInGroup.Instance
      Attribute=firstItemInGroup.Attribute
      Changes=changeList
    }
    )
let historyOfAType (xtype0:string option) =
  match xtype0 with
    | Some sType->changeHistories |> List.filter(fun x->x.Type=sType)
    | None->changeHistories
 



// let historyOfATypeAndInstance (xInstance:string) (?b : string) =
//   let records= 
//     match b with 
//       |Some (Value = typeName: string;) -> historyOfAType typeName
//       |None->changeHistories
//   records |> List.filter(fun x->x.Attribute=xInstance)
// EAV or EAV/CR
// Decided that Type Instance Attribute Value datetime was all that was required
// It's a logging question!