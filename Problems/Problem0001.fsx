// Leetcode Problem 1 Two Sum
//  Makes sense to note
//      a) the array cannot have duplicate elements, otherwise exactly one solution assumption fails
//         hence, a reverse index lookup dictionary can be built
//      b) a care should be taken to forbid the use of the same element twice,
//         although strictly speaking such target value violates the condition of existing single solution
let twoSum source target =
    let revIdx = source |> Seq.mapi (fun i v -> (v,i)) |> dict
    revIdx.Keys
    |> Seq.tryPick (fun k -> (revIdx.ContainsKey(target - k) && (k + k <> target)) |> function 
                                                                                      |true -> Some(revIdx.[k],revIdx.[target - k])
                                                                                      | _ -> None)

twoSum [|3;6;8;0|] 6