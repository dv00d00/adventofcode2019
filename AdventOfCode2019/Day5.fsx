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
    Input: int;
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

    let print value state =
        { state with Outputs = value :: state.Outputs }

let step (state:State) : Option<State> =
    
    let instruction = parse state.IntPtr state.Memory
    
    let inline getValue parameter =
        match parameter with
        | Ref x -> state.Memory.[x]
        | Val x -> x

    match instruction with
    | Add (p1, p2, addr) ->
        let arg1 = getValue p1
        let arg2 = getValue p2
        state |> Eff.set_mem addr (arg1 + arg2) |> Eff.inc_intptr 4 |> Some
        
    | Mul (p1, p2, addr) ->
        let arg1 = getValue p1
        let arg2 = getValue p2
        state |> Eff.set_mem addr (arg1 * arg2) |> Eff.inc_intptr 4 |> Some
        
    | Move (addr) ->
        state |> Eff.set_mem addr state.Input |> Eff.inc_intptr 2 |> Some
        
    | Out p ->
        state |> Eff.print (getValue p) |> Eff.inc_intptr 2 |> Some
        
    | JumpNonZero (test, destination) ->
        if getValue test <> 0 then
            state |> Eff.set_intptr (getValue destination) |> Some
        else
            state |> Eff.inc_intptr 3 |> Some
    
    | JumpZero (test, destination) ->
        if getValue test = 0 then
            state |> Eff.set_intptr (getValue destination) |> Some
         else
            state |> Eff.inc_intptr 3 |> Some
        
    | LessThen (a, b, dest) ->
        let value = 
            if getValue a < getValue b then 1
            else 0
            
        state |> Eff.set_mem dest value |> Eff.inc_intptr 4 |> Some
        
    | Equals (p1, p2, dest) ->
        let value =
            if getValue p1 = getValue p2 then 1
            else 0
        
        state |> Eff.set_mem dest value |> Eff.inc_intptr 4 |> Some
        
    | Halt ->
        None
    
let memory = [|3;225;1;225;6;6;1100;1;238;225;104;0;1002;148;28;224;1001;224;-672;224;4;224;1002;223;8;223;101;3;224;
               224;1;224;223;223;1102;8;21;225;1102;13;10;225;1102;21;10;225;1102;6;14;225;1102;94;17;225;1;40;173;
               224;1001;224;-90;224;4;224;102;8;223;223;1001;224;4;224;1;224;223;223;2;35;44;224;101;-80;224;224;4;
               224;102;8;223;223;101;6;224;224;1;223;224;223;1101;26;94;224;101;-120;224;224;4;224;102;8;223;223;1001;
               224;7;224;1;224;223;223;1001;52;70;224;101;-87;224;224;4;224;1002;223;8;223;1001;224;2;224;1;223;224;
               223;1101;16;92;225;1101;59;24;225;102;83;48;224;101;-1162;224;224;4;224;102;8;223;223;101;4;224;224;1;
               223;224;223;1101;80;10;225;101;5;143;224;1001;224;-21;224;4;224;1002;223;8;223;1001;224;6;224;1;223;224;
               223;1102;94;67;224;101;-6298;224;224;4;224;102;8;223;223;1001;224;3;224;1;224;223;223;4;223;99;0;0;0;
               677;0;0;0;0;0;0;0;0;0;0;0;1105;0;99999;1105;227;247;1105;1;99999;1005;227;99999;1005;0;256;1105;1;99999;
               1106;227;99999;1106;0;265;1105;1;99999;1006;0;99999;1006;227;274;1105;1;99999;1105;1;280;1105;1;99999;
               1;225;225;225;1101;294;0;0;105;1;0;1105;1;99999;1106;0;300;1105;1;99999;1;225;225;225;1101;314;0;0;106;
               0;0;1105;1;99999;108;677;677;224;102;2;223;223;1005;224;329;101;1;223;223;1107;677;226;224;102;2;223;
               223;1006;224;344;101;1;223;223;1107;226;226;224;102;2;223;223;1006;224;359;101;1;223;223;1108;677;677;
               224;102;2;223;223;1005;224;374;101;1;223;223;8;677;226;224;1002;223;2;223;1005;224;389;101;1;223;223;
               108;226;677;224;1002;223;2;223;1006;224;404;1001;223;1;223;107;677;677;224;102;2;223;223;1006;224;419;
               101;1;223;223;1007;226;226;224;102;2;223;223;1005;224;434;101;1;223;223;1007;677;677;224;102;2;223;223;
               1005;224;449;1001;223;1;223;8;677;677;224;1002;223;2;223;1006;224;464;101;1;223;223;1108;677;226;224;
               1002;223;2;223;1005;224;479;101;1;223;223;7;677;226;224;1002;223;2;223;1005;224;494;101;1;223;223;1008;
               677;677;224;1002;223;2;223;1006;224;509;1001;223;1;223;1007;226;677;224;1002;223;2;223;1006;224;524;
               1001;223;1;223;107;226;226;224;1002;223;2;223;1006;224;539;1001;223;1;223;1107;226;677;224;102;2;223;
               223;1005;224;554;101;1;223;223;1108;226;677;224;102;2;223;223;1006;224;569;101;1;223;223;108;226;226;
               224;1002;223;2;223;1006;224;584;1001;223;1;223;7;226;226;224;1002;223;2;223;1006;224;599;101;1;223;223;
               8;226;677;224;102;2;223;223;1005;224;614;101;1;223;223;7;226;677;224;1002;223;2;223;1005;224;629;101;1;
               223;223;1008;226;677;224;1002;223;2;223;1006;224;644;101;1;223;223;107;226;677;224;1002;223;2;223;1005;
               224;659;1001;223;1;223;1008;226;226;224;1002;223;2;223;1006;224;674;1001;223;1;223;4;223;99;226|]

let run input memory =
    
    let state = {
        IntPtr = IntPtr 0;
        Memory = memory |> Array.indexed |> Map.ofArray
        Outputs = [];
        Input = input;
    }
    
    let outputs =
        state |> Seq.unfold (fun state ->
            step state |> Option.map (fun state -> state.Outputs, state)
        )
        |> Seq.last

    outputs |> Seq.head    
    
let test = [|
    3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;
    1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;
    999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99
|]

let test_run1 = run 1 test
let test_run2 = run 8 test
let test_run3 = run 100 test
    
let answer1 = run 1 memory
let answer2 = run 5 memory
