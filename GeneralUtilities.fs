namespace BitBlue.Utilities


open System


[<AutoOpen>]
module GeneralUtilities =
  let expand template (data:Map<string, string>) =
    let pattern = "\\${(.*?)}"
    let f (m:Regex.RegexMatch) =
      let key = m.Groups.[1].Value
      match Map.tryFind key data with
      | Some x -> x
      | _ -> m.Groups.[0].Value
    Regex.replaceWithFunction pattern f template

  ///Expands the template multiple times. This is to handle instances where the data values are templates
  let nExpand n template data =
    let rec loop (template:string) data n =
      if n=0 || not (template.Contains "${") then template
      else loop (expand template data) data (n-1)
    loop template data n

  let (|Contains|_|) subString x =
    if String.contains subString x then Some ()
    else None

  let limitText cnt text =
    match String.length text with
    | x when x <= cnt -> text
    | x when cnt > 3 -> text.[0 .. cnt - 3 - 1] + "..."
    | _ -> text.[0 .. cnt - 1]

