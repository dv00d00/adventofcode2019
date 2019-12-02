let (|Add|Multiply|Halt|) (intptr, (xs:int[])) =
    match xs.[intptr] with
    | 1 -> Add      (xs.[intptr + 1], xs.[intptr + 2], xs.[intptr + 3])
    | 2 -> Multiply (xs.[intptr + 1], xs.[intptr + 2], xs.[intptr + 3])
    | 99 -> Halt

let solve code =
    let code = Array.copy code
    
    let rec run intptr code =
    
        match intptr, code with
        | Add (a,b,c) ->
            code.[c] <- code.[a] + code.[b]
            run (intptr + 4) code
        | Multiply (a,b,c) ->
            code.[c] <- code.[a] * code.[b]
            run (intptr + 4) code 
        | Halt ->
            code.[0]
    
    run 0 code

let test1 = solve [|1;9;10;3;2;3;11;0;99;30;40;50|]
let test2 = solve [|1;0;0;0;99|]

let input = [|1;12;2;3;1;1;2;3;1;3;4;3;1;5;0;3;2;1;10;19;1;6;19;23;2;23;6;27;2;6;27;31;2;13;31;35;1;10;35;39;2;39;13;43;
              1;43;13;47;1;6;47;51;1;10;51;55;2;55;6;59;1;5;59;63;2;9;63;67;1;6;67;71;2;9;71;75;1;6;75;79;2;79;13;83;1;
              83;10;87;1;13;87;91;1;91;10;95;2;9;95;99;1;5;99;103;2;10;103;107;1;107;2;111;1;111;5;0;99;2;14;0;0|]

let answer1 = solve input

let answer2 =
    
    let domain = Seq.allPairs [0..99] [0..99]
    let input = Array.copy input

    domain
    |> Seq.map (fun (noun, verb) ->
        input.[1] <- noun
        input.[2] <- verb
        (noun, verb), solve input
    )
    |> Seq.find (fun (_, r) -> r = 19690720)
    |> fun ((n,v), _) -> 100 * n + v        

