type ByRef = ByRef of int

type Param =
    | Ref of int
    | Val of int
    
type Instruction =
    | Add  of Param * Param * ByRef
    | Mul  of Param * Param * ByRef
    | Move of ByRef 
    | Out  of Param
    | JumpNonZero of Param * Param
    | JumpZero  of Param * Param
    | LessThen of Param * Param * ByRef
    | Equals of Param * Param * ByRef
    | Halt
    
type IntPtr = IntPtr of int

type Memory = Map<int, int>

let inline instruction (x:int) =
    (string x).PadLeft(5, '0').ToCharArray()
    
let inline parameter mode value =
    if mode = '0' then Ref value
    else Val value

let parse (IntPtr intptr) (xs:Memory) =
    let inline input i mode = parameter mode xs.[intptr + i]
    let inline output i = ByRef xs.[(intptr + i)]
    
    match instruction (xs.[intptr]) with
    | [|'0';m2;m1;'0';'1'|] -> Add  (input 1 m1, input 2 m2, output 3)
    | [|'0';m2;m1;'0';'2'|] -> Mul  (input 1 m1, input 2 m2, output 3)
    | [|_ ;_ ;'0';'0';'3'|] -> Move (output 1)
    | [|_ ;_  ;m1;'0';'4'|] -> Out  (input 1 m1)
    | [|_ ;m2;m1;'0';'5' |] -> JumpNonZero  (input 1 m1, input 2 m2)
    | [|_ ;m2;m1;'0';'6' |] -> JumpZero  (input 1 m1, input 2 m2)
    | [|'0';m2;m1;'0';'7'|] -> LessThen  (input 1 m1, input 2 m2, output 3)
    | [|'0';m2;m1;'0';'8'|] -> Equals  (input 1 m1, input 2 m2, output 3)
    | [|_ ;_ ;_ ;'9';'9' |] -> Halt
    | input -> failwithf "malformed input: %A" input 

type State = {
    IntPtr: IntPtr;
    Memory: Memory;
    Inputs: int[];
    InputIdx: int;
    Outputs: int list
}

module Eff = 

    let set_mem (ByRef address) value state =
        let next = state.Memory |> Map.add address value
        { state with Memory = next } 

    let inc_intptr offset (state:State)  =
        let inline (+) (IntPtr a) (b:int) = IntPtr (a + b)
        { state with IntPtr = state.IntPtr + offset }
        
    let set_intptr value (state:State) = { state with IntPtr = IntPtr value }

    let inc_inputptr (state:State) = {state with InputIdx = state.InputIdx + 1}

    let print value state =
        { state with Outputs = value :: state.Outputs }

type Mode = Running | Interrupted | Halted

let step (state:State) : State * Mode =
    
    let instruction = parse state.IntPtr state.Memory
    
    let inline getValue parameter =
        match parameter with
        | Ref x -> state.Memory.[x]
        | Val x -> x

    match instruction with
    | Add (p1, p2, addr) ->
        let arg1 = getValue p1
        let arg2 = getValue p2
        state |> Eff.set_mem addr (arg1 + arg2) |> Eff.inc_intptr 4 , Running
        
    | Mul (p1, p2, addr) ->
        let arg1 = getValue p1
        let arg2 = getValue p2
        state |> Eff.set_mem addr (arg1 * arg2) |> Eff.inc_intptr 4 , Running
        
    | Move (addr) ->

        if (state.InputIdx < state.Inputs.Length) then
            state 
            |> Eff.set_mem addr (state.Inputs.[state.InputIdx])
            |> Eff.inc_intptr 2 
            |> Eff.inc_inputptr , Running

        else 
            state, Interrupted
        
    | Out p ->
        state |> Eff.print (getValue p) |> Eff.inc_intptr 2 , Running
        
    | JumpNonZero (test, destination) ->
        if getValue test <> 0 then
            state |> Eff.set_intptr (getValue destination) , Running
        else
            state |> Eff.inc_intptr 3 , Running
    
    | JumpZero (test, destination) ->
        if getValue test = 0 then
            state |> Eff.set_intptr (getValue destination) , Running
         else
            state |> Eff.inc_intptr 3 , Running
        
    | LessThen (a, b, dest) ->
        let value = 
            if getValue a < getValue b then 1
            else 0
            
        state |> Eff.set_mem dest value |> Eff.inc_intptr 4 , Running
        
    | Equals (p1, p2, dest) ->
        let value =
            if getValue p1 = getValue p2 then 1
            else 0
        
        state |> Eff.set_mem dest value |> Eff.inc_intptr 4 , Running
        
    | Halt ->
        state , Halted

let day7_input = [|3;8;1001;8;10;8;105;1;0;0;21;38;55;80;97;118;199;280;361;442;99999;3;9;101;2;9;9;1002;9;5;9;1001;9;4;9;4;9;99;3;9;
101;5;9;9;102;2;9;9;1001;9;5;9;4;9;99;3;9;1001;9;4;9;102;5;9;9;101;4;9;9;102;4;9;9;1001;9;4;9;4;9;99;3;9;1001;9;3;9;1002;9;2;9;
101;3;9;9;4;9;99;3;9;101;5;9;9;1002;9;2;9;101;3;9;9;1002;9;5;9;4;9;99;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;3;
9;101;1;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;
99;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;101;2;9;9;4;
9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;102;2;9;9;
4;9;3;9;1001;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;101;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;99;3;9;1002;9;2;
9;4;9;3;9;101;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;2;9;9;4;9;3;9;102;2;9;9;
4;9;3;9;101;2;9;9;4;9;3;9;1001;9;1;9;4;9;99;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;2;
9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;1;9;4;9;3;9;102;2;9;9;4;9;99|]

let rec insertions x = function
| []             -> [[x]]
| (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
| []      -> seq [ [] ]
| x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

module Part1 = 

    let runOne memory phase input = 
        let state = {
            IntPtr = IntPtr 0;
            Memory = memory |> Array.indexed |> Map.ofArray
            Outputs = [];
            Inputs = [|phase; input|];
            InputIdx = 0;
        }
    
        let outputs =
            state |> Seq.unfold (fun state ->
                let (state,mode) = step state 
                match mode with
                | Halted | Interrupted -> None
                | Running -> Some (state.Outputs, state)

            )
            |> Seq.last

        outputs |> Seq.head  

    let runChain program (settings:int[]) = 
        settings
        |> Array.fold (fun output phase ->
            runOne program phase output
        ) 0

    let possibleInputs = permutations [0;1;2;3;4;] |> Seq.map (Array.ofList)

    let solve program = 
        possibleInputs 
        |> Seq.map (fun input -> input, runChain program input)
        |> Seq.maxBy snd

    let solution1 = solve day7_input

module Part2 = 

    let inputs = permutations [5;6;7;8;9] |> Seq.map (Array.ofList)

    let mkState memory phase inputs = 
        {
            IntPtr = IntPtr 0;
            Memory = memory |> Array.indexed |> Map.ofArray
            Outputs = [];
            Inputs = Array.concat [ [|phase;|] ; inputs ];
            InputIdx = 0;
        }

    let runOne state = 
        state |> Seq.unfold (fun state ->
            let (state,mode) = step state 
            Some ((mode, state), state)
        )
        |> Seq.skipWhile (fun (mode,state) -> mode = Running)
        |> Seq.head

    let seed program (settings:int[]) = 
        let (_, state1) = runOne (mkState program settings.[0] [|0|])
        let (_, state2) = runOne (mkState program settings.[1] (state1.Outputs |> List.rev |> Array.ofList))
        let (_, state3) = runOne (mkState program settings.[2] (state2.Outputs |> List.rev |> Array.ofList))
        let (_, state4) = runOne (mkState program settings.[3] (state3.Outputs |> List.rev |> Array.ofList))
        let (mode, state5) = runOne (mkState program settings.[4] (state4.Outputs |> List.rev |> Array.ofList))
        let nextStates = [| state1; state2; state3; state4; state5; |]
    
        mode , nextStates

    let runRound (states:State[]) = 
        let link (recepient: State) (source: State) =
            let inline (++) a b = Array.concat [ a; b]
            let recepient' = {
                recepient with 
                    Inputs = recepient.Inputs ++ (source.Outputs |> List.rev |> Array.ofList)
                    Outputs = []
            }
            recepient'

        let _,s0 = runOne (link states.[0] states.[4])
        let _,s1 = runOne (link states.[1] s0)
        let _,s2 = runOne (link states.[2] s1)
        let _,s3 = runOne (link states.[3] s2)
        let mode,s4 = runOne (link states.[4] s3)
        let nextStates = [| s0; s1; s2; s3; s4; |]
        mode , nextStates

    let solve code settings = 
        let (mode, next) = seed code settings
        if mode = Halted then 
            next.[4].Outputs |> Seq.last
        else
            let rec go states =
                let (mode, next) = runRound states
                if mode = Halted then 
                    (mode, next)
                else
                    go next
            
            let _, st = go next
            st.[4].Outputs |> Seq.last

    let solution2 = 
        inputs
        |> Seq.map (fun settings -> solve day7_input settings)
        |> Seq.max