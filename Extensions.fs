namespace BitBlue.Utilities


open System
open System.Text.RegularExpressions

[<AutoOpen>]
module Extensions =
  type String with
    ///Returns ifNull is value isNullOrEmpty else value
    static member orElse ifNull value =
      if String.IsNullOrEmpty value then ifNull
      else value

    static member replace pattern (replacement:string) caseSensitive original =
      match caseSensitive with
      | false ->
        //optimized algorithm at copied from https://www.codeproject.com/Articles/10890/Fastest-C-Case-Insenstive-String-Replace
        Regex.Replace (original, Regex.Escape pattern, replacement, RegexOptions.IgnoreCase ||| RegexOptions.Multiline)
      | true -> original.Replace (pattern, replacement)
    
    static member toTitleCase =
      let regex = new Regex("[A-Z](?=[^A-Z])|(?<=[^A-Z])[A-Z]", RegexOptions.Compiled)
      fun x ->
        match x with
        | "" -> ""
        | _ ->
            let x = regex.Replace(x, " $0")
            sprintf "%c%s" (Char.ToUpper x.[0]) x.[1..]

  type Exception with
    member x.innerMost =
      let rec f (x: exn) = match isNull x.InnerException with false -> f x.InnerException | _ -> x
      f x

  module List =
    let safeSkip n xs =
      xs
      |> List.indexed //.mapi (fun i x -> i, x)
      |> List.choose (fun (i, x) -> if i >= n then Some x else None)
    
    ///Can this be replaced with List.truncate?
    let safeTake n xs =
      xs
      |> List.indexed //mapi (fun i x -> i, x)
      |> List.choose (fun (i, x) -> if i < n then Some x else None)

    let rec intersperse sep = function
      | [] -> []
      | [x] -> [x]
      | x :: xs -> x :: sep :: intersperse sep xs

  module Seq =
    let all f xs = not <| Seq.exists (not << f) xs