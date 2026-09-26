(*
    0x000 - 0x1ff mostly unused, builtin font here
    0x200 - 0xfff program and ram
*)

open Unsigned

module Timer = struct
    type t = UInt8.t
end

module type Address = sig
    type t = UInt16.t
    val of_int : int -> t
    val add : t -> t
    val sub : t -> t
    val mul : t -> t
end

module Address : Address = struct
    type t = UInt16.t
    let of_int = failwith "TODO"
    let add = failwith "TODO"
    let sub = failwith "TODO"
    let mul = failwith "TODO"
end


module type Register = sig
    type t
    val range : int
    val in_range : t -> bool
    val of_int : int -> t
    val compare : t -> t -> int
end

module Register : Register = struct
    type t = int
    let range = 0xf
    let in_range n : bool = n >= 0 && n <= range
    let of_int n =
        assert (in_range n);
        n
    let compare = Int.compare
end

module RegisterMap = Map.Make(Register)

module Registers = struct
    open Register
    type t = UInt8.t RegisterMap.t
    let create () =
        let nullbyte = UInt8.of_int 0 in
        RegisterMap.of_list @@ List.init range (fun x -> (of_int x, nullbyte))
    let find n (registers : t) =
        assert (in_range n);
        registers |> RegisterMap.find n
    let update n value (registers : t) : t =
        assert (in_range n);
        registers |> RegisterMap.update n (Option.map (fun _ -> value))
end

type opcode =
    | Scd of int
    | Scu of int
    | LdReg of Register.t * Register.t
    | LdImmediate of Register.t * UInt8.t
    | LdI of Address.t
    | LdMemory of Register.t
    | LdFromMemory of Register.t
    | LdDelayTimer of Register.t
    | LdFromDelayTimer of Register.t
    | LdKey of Register.t
    | LdSoundTimer of Register.t
    | LdSprite of Register.t
    | LdBCD of Register.t
    | Cls
    | Ret
    | Call of Address.t
    | Scr
    | Scl
    | Exit
    | Low
    | High
    | Dw of Address.t
    | Or of Register.t * Register.t  (**)
    | And of Register.t * Register.t  (* all these set VF = 0 *)
    | Xor of Register.t * Register.t (**)
    | Se of Register.t * UInt8.t
    | SeReg of Register.t * Register.t
    | Sne of Register.t * UInt8.t
    | SneReg of Register.t * Register.t
    | Jump of Address.t
    | Jump0 of Address.t
    | Rnd of Register.t * UInt8.t
    | AddI of Register.t
    | AddImmediate of Register.t * UInt8.t (* do not set OF *)
    | Add of Register.t * Register.t (* should set OF *)
    | Sub of Register.t * Register.t
    | Subn of Register.t * Register.t
    | Shr of Register.t * Register.t
    | Shl of Register.t * Register.t
    | Skp of Register.t
    | Sknp of Register.t
    | Draw of {xpos : Register.t ; ypos : Register.t ; height : int}

module type Stack = sig
    type 'a t
    exception Empty
    val create : unit -> 'a t
    val push : 'a -> 'a t -> 'a t
    val pop : 'a t -> 'a * 'a t
end

module Stack : Stack = struct
    type 'a t = StackContents of 'a list
    exception Empty
    let create () = StackContents []
    let push x (StackContents s) = StackContents (x::s)
    let pop (StackContents s) =
        match s with
        | [] -> raise Empty
        | h::t -> h, StackContents t
end


type cpu = {
    pc : UInt16.t;
    i : UInt16.t;
    dt : Timer.t;
    st : Timer.t;
    vr : Registers.t
}

let fetch cpu memory : bytes * cpu =
    let instr_bytes = Bytes.sub memory (UInt16.to_int cpu.pc) 2 in
    let pc = UInt16.add cpu.pc (UInt16.of_int 2) in
    let cpu' = {cpu with pc = pc} in
    instr_bytes,cpu'

let decode intsr : opcode =
    failwith "TODO"

let execute cpu memory stack = function
    | Cls -> failwith "TODO"
    | Jump addr -> {cpu with pc = addr},stack
    | LdReg (d,s) -> failwith "TODO"
    | LdI addr -> failwith "TODO"
    | AddImmediate (r,v) ->
            let newv = UInt8.add (cpu.vr |> Registers.find r) v in
            {cpu with vr = cpu.vr |> Registers.update r newv},stack
    | Draw x -> failwith "TODO"
    | _ -> failwith "TODO"

let rec loop cpu memory stack t =
    (* handle inputs *)
    (* decrement timers *)

    let rec fde c s = function
        | 0 -> c, s
        | i ->
            let (instr,cpu') = fetch cpu memory in
            let opcode = decode instr in
            let (cpu'', stack') = execute cpu' memory stack opcode in
            fde cpu'' stack' (i - 1)
    in
    let (cpu', stack') = fde cpu stack 10 in
    (* update screen *)
    let timedelta = Unix.gettimeofday () -. t in
    let _ = Unix.sleepf @@ 16.667 -. timedelta in
    let t' = Unix.gettimeofday () in
    loop cpu' memory stack' t'

let () =
    let nullbyte = UInt8.of_int 0 in
    let memory = Array.make 4096 nullbyte in
    let stack = Stack.create () in
    let cpu = {
        pc = UInt16.of_int 0x200;
        i = UInt16.of_int 0;
        dt = nullbyte;
        st = nullbyte;
        vr = Registers.create ()
    } in
    loop cpu memory stack (Unix.gettimeofday ())

