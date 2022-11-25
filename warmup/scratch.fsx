let linesExample = System.IO.File.ReadLines("warmup/example.tsv")   |> Seq.toList;;
let fieldsExample =linesExample|>List.map(fun x->x.Split [|'\t'|])
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

let allEntities=changeHistories
let entitiesByName name = changeHistories |> List.filter(fun x->x.Type=name)
let specificEntity name identifier = entitiesByName name |> List.filter(fun x->x.Instance=identifier)
let valueChangedBetween (dtFrom:System.DateTime) (dtTo:System.DateTime)=
  let hasChangeInsideDateRange=allEntities|>List.filter(fun x->x.Changes|>List.exists(fun y->y.EventDateTime>dtFrom && y.EventDateTime<dtTo))
  let getMinChangeDateForAnItem (item:TIAVDHistoryItem) = (item.Changes |> List.minBy(fun k->k.EventDateTime)).EventDateTime
  let ret=hasChangeInsideDateRange |> List.sortBy (fun z->getMinChangeDateForAnItem z)
  ret
// EAV or EAV/CR
// Decided that Type Instance Attribute Value datetime was all that was required
// It's a logging question!
