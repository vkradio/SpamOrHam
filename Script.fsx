open System.IO

type DocType =
    | Ham
    | Spam

let parseDocType (label:string) =
    match label with
    | "ham"  -> Ham
    | "spam" -> Spam
    | _      -> failwith "Unknown label"

let parseLine (line:string) =
    let split = line.Split('\t')
    let label = split.[0] |> parseDocType
    let message = split.[1]
    (label, message)

let fileName = "SMSSpamCollection.txt"
let path = Path.Combine(__SOURCE_DIRECTORY__, "Data", fileName)

let dataset =
    File.ReadAllLines path
    |> Array.map parseLine

let spamWithFREE =
    dataset
    |> Array.filter (fun (docType, _) -> docType = Spam)
    |> Array.filter (fun (_, sms) -> sms.Contains "FREE")
    |> Array.length

let hamWithFREE =
    dataset
    |> Array.filter (fun (docType, _) -> docType = Ham)
    |> Array.filter (fun (_, sms) -> sms.Contains "FREE")
    |> Array.length

dataset
|> Array.filter (fun (docType, _) -> docType = Ham)
|> Array.filter (fun (_, sms) -> sms.Contains "FREE")
|> Array.map (fun (_, sms) -> sms)
|> Array.iter (printfn "%s")

let primitiveClassifier (sms:string) =
    if (sms.Contains "FREE")
    then Spam
    else Ham
