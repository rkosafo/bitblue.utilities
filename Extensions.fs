namespace App.Utils


open System

[<AutoOpen>]
module Extensions =
  type String with
    ///Returns ifNull is value isNullOrEmpty else value
    static member orElse ifNull value =
      if String.IsNullOrEmpty value then ifNull
      else value

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