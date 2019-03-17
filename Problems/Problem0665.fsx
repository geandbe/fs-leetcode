let isNondec ls =
    let rec checkOrder ls isFlipped =
        match ls with
        | [x] -> true
        | [] -> true
        | x::y::t -> if x <= y then checkOrder (y::t) isFlipped elif isFlipped then false else checkOrder (y::t) true
    checkOrder ls false


isNondec [1;2;3]
isNondec [2;1]
isNondec [1;2]
isNondec [4;2;3]
isNondec [4;2;1]
isNondec [-1;4;2;3]
