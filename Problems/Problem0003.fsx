// Leetcode Problem 3 Longest Substring Without Repeating Characters

let uniqueMaxSubstr (s: string) =
    let rec uniqueHead (s:string) (ls: string list) = 
        let l = s |> Seq.mapi(fun i c -> s.IndexOf(c) = i) |> Seq.takeWhile(id) |> Seq.length
        if l = s.Length && l = 0 then ls
        elif l = s.Length then s::ls
        else uniqueHead s.[s.IndexOf(s.[(l)]) + 1 .. ] (s.[.. (l - 1)]::ls)
    match s with
    | "" -> ("",0)
    | _ -> uniqueHead s [] |> List.maxBy (fun s -> s.Length) |> fun s -> (s, s.Length)

uniqueMaxSubstr "pwwkew"
uniqueMaxSubstr "bbbbb"
uniqueMaxSubstr " "
uniqueMaxSubstr "dvdf"
uniqueMaxSubstr "tmmzuxt"
uniqueMaxSubstr "dvd"
uniqueMaxSubstr ""
