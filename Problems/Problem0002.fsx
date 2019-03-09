// Leetcode Problem 2 Add Two Numbers

let sum s1 s2 =
    let rec sum' s1 s2 carry r =
        match s1,s2,carry with
        |[],[],p -> (if p then '1'::r else r) |> List.rev
        | h1::t1,[],false -> sum' t1 [] false (h1::r)
        | h1::t1,[],true when h1 = '9' -> sum' t1 [] true ('0'::r)
        | h1::t1,[],true -> sum' t1 [] false (char((int h1) + 1)::r)
        | [],h2::t2,false -> sum' [] t2 false (h2::r)
        | [],h2::t2,true when h2 = '9' -> sum' t2 [] true ('0'::r)
        | [],h2::t2,true -> sum' t2 [] false (char((int h2) + 1)::r)
        | h1::t1,h2::t2,p -> let psum = int(h1) + int(h2) - 2*int('0') + (if p then 1 else 0)
                             if psum >=10 then sum' t1 t2 true (char(psum - 10 + int('0'))::r) else sum' t1 t2 false (char(psum + int('0'))::r)
    sum' s1 s2 false []


sum ['0';'9'] ['9';'9'] 

sum [] ['1';'9']

sum ['1'] ['9';'9';'9']

sum [] []