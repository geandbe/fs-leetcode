// Leetcode Problem 3 Longest Substring Without Repeating Characters

let uniqueSubstr s =
    let rec uniqueHead (s:string) (ls: string list) = 
        let l = s |> Seq.mapi(fun i c -> s.IndexOf(c) = i) |> Seq.takeWhile(id) |> Seq.length
        if l > 1 then uniqueHead s.[l..] (s.[..l-1]::ls) else s.[..l-1]::ls
    uniqueHead s [] |> List.maxBy (fun s -> s.Length) |> fun s -> (s, s.Length)

uniqueSubstr "abcabcbb"
uniqueSubstr "bbbbb"

uniqueSubstr "pwwkew"