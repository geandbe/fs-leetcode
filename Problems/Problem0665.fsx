let isNondec ls =
    let rec checkOrder ls isFlipped p =
        match ls with
        | [x] -> true
        | [] -> true
        | x::y::t when isFlipped -> if x > y then false
                                    else checkOrder (y::t) true x
        | x::y::t -> if x <= y then checkOrder (y::t) false x
                     else checkOrder ((max p y)::y::t) true p
                     
    checkOrder ls false System.Int32.MinValue


isNondec [1;2;3]
isNondec [2;1]
isNondec [1;2]
isNondec [4;2;3]
isNondec [4;2;1]
isNondec [-1;4;2;3]
isNondec [3;4;2;3]
isNondec [3;3;2;2]

