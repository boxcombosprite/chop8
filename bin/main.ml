(*
    0x000 - 0x1ff mostly unused, builtin font here
    0x200 - 0xfff program and ram
*)

open Unsigned

let width = 64
let height = 32

let font = [
    0xF0; 0x90; 0x90; 0x90; 0xF0;  (* 0 *)
    0x60; 0x20; 0x20; 0x20; 0x70;  (* 1 *)
    0xF0; 0x10; 0xF0; 0x80; 0xF0;  (* 2 *)
    0xF0; 0x10; 0xF0; 0x10; 0xF0;  (* 3 *)
    0xA0; 0xA0; 0xF0; 0x20; 0x20;  (* 4 *)
    0xF0; 0x80; 0xF0; 0x10; 0xF0;  (* 5 *)
    0xF0; 0x80; 0xF0; 0x90; 0xF0;  (* 6 *)
    0xF0; 0x10; 0x10; 0x10; 0x10;  (* 7 *)
    0xF0; 0x90; 0xF0; 0x90; 0xF0;  (* 8 *)
    0xF0; 0x90; 0xF0; 0x10; 0xF0;  (* 9 *)
    0xF0; 0x90; 0xF0; 0x90; 0x90;  (* A *)
    0xF0; 0x50; 0x70; 0x50; 0xF0;  (* B *)
    0xF0; 0x80; 0x80; 0x80; 0xF0;  (* C *)
    0xF0; 0x50; 0x50; 0x50; 0xF0;  (* D *)
    0xF0; 0x80; 0xF0; 0x80; 0xF0;  (* E *)
    0xF0; 0x80; 0xF0; 0x80; 0x80;  (* F *)
] |> List.map UInt8.of_int


let ( |:> ) x f = snd @@ f x
let ( |.> ) x f = fst @@ f x
let ( >> ) f g x = g @@ f x
let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j


module Timer = struct
    type t = UInt8.t
    let dec timer =
        if timer > UInt8.zero then UInt8.pred timer else UInt8.zero
end

module Address : sig
    type t = UInt16.t
    val of_int : int -> t
    val add : t -> int -> t
    val sub : t -> int -> t
end =
struct
    open UInt16
    type t = UInt16.t
    let of_int = failwith "TODO"
    let add = failwith "TODO"
    let sub = failwith "TODO"
    let mul = failwith "TODO"
end


module Register : sig
    type t
    val range : int
    val in_range : t -> bool
    val of_int : int -> t
    val compare : t -> t -> int
end =
struct
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


module Stack : sig
    type 'a t
    exception Empty
    val create : unit -> 'a t
    val push : 'a -> 'a t -> 'a t
    val pop : 'a t -> 'a * 'a t
end =
struct
    type 'a t = StackContents of 'a list
    exception Empty
    let create () = StackContents []
    let push x (StackContents s) = StackContents (x::s)
    let pop (StackContents s) =
        match s with
        | [] -> raise Empty
        | h::t -> h, StackContents t
end

module Memory : sig
    type t = UInt8.t Array.t
end

module Cpu : sig
    type t = {
        pc : UInt16.t;
        i : UInt16.t;
        dt : Timer.t;
        st : Timer.t;
        vr : Registers.t
    }
    val tick_timers : t -> t
    val register_operation : (UInt8.t -> UInt8.t -> UInt8.t) -> Register.t -> Register.t -> t -> UInt8.t * t
    val register_value : Register.t -> t -> UInt8.t
    val update_register : Register.t -> UInt8.t -> t -> t
end =
struct
    include Register
    include Registers
    type t = {
        pc : UInt16.t;
        i : UInt16.t;
        dt : Timer.t;
        st : Timer.t;
        vr : Registers.t
    }
    let tick_timers c =
        {
            c with
            dt = Timer.dec c.dt;
            st = Timer.dec c.st;
        }
    let register_value r cpu =
        cpu.vr |> Registers.find r
    let update_register r n cpu =
        {cpu with vr = cpu.vr |> Registers.update r n}
    let register_operation f dst src (cpu : t) =
        let x = cpu |> register_value dst in
        let y = cpu |> register_value src in
        let newv = f x y in
        newv, cpu |> update_register dst newv
end

module Nibbles : sig
    type t = int list
    val make : UInt8.t list -> t
    val to_int : t -> int
end =
struct
    type t = int list
    let nibbles (x: UInt8.t) =
        let x' = (UInt8.to_int x) in
        [
            x' lsr 4 land 0xf;
            x' land 0xf;
        ]
    let make bs = List.map nibbles bs |> List.concat
    let to_int ns =
        let rec aux acc xs =
            match xs with
            | [] -> acc
            | h::t -> aux (acc lor h lsl 4) t
        in
        aux 0 ns
end

let (|:>) x f = snd @@ f x
let (|.>) x f = fst @@ f x

let fetch (cpu : Cpu.t) memory : int list * Cpu.t =
    let instr =
        Array.to_list @@ Array.sub memory (UInt16.to_int cpu.pc) 2
        |> Nibbles.make in
    let pc = UInt16.add cpu.pc (UInt16.of_int 2) in
    let cpu' = {cpu with pc = pc} in
    instr,cpu'

let decode = function
    | 0x0::rest ->
            begin
            match rest with
            | [0x0; 0xe; 0x0] -> Cls
            | [0x0; 0xe; 0xe] -> Ret
            | _::rest -> failwith "Dw"
            | _ -> failwith "Unexpected"
            end
    | 0x1::rest -> Jump (Address.of_int @@ Nibbles.to_int rest)
    | 0x2::rest -> Call (Address.of_int @@ Nibbles.to_int rest)
    | 0x3::x::rest -> Se (Register.of_int x, UInt8.of_int @@ Nibbles.to_int rest)
    | 0x4::x::rest -> Sne (Register.of_int x, UInt8.of_int @@ Nibbles.to_int rest)
    | [0x5;x;y;0x0] -> SeReg (Register.of_int x, Register.of_int y)
    | 0x6::x::rest -> LdImmediate (Register.of_int x, UInt8.of_int @@ Nibbles.to_int rest)
    | 0x7::x::rest -> AddImmediate (Register.of_int x, UInt8.of_int @@ Nibbles.to_int rest)
    | [0x8;x;y;last] ->
            let dst,src = let open Register in of_int x, of_int y in
            begin
            match last with
            | 0x0 -> LdReg (dst, src)
            | 0x1 -> Or (dst, src)
            | 0x2 -> And (dst, src)
            | 0x3 -> Xor (dst, src)
            | 0x4 -> Add (dst, src)
            | 0x5 -> Sub (dst, src)
            | 0x6 -> Shr (dst, src)
            | 0x7 -> Subn (dst, src)
            | 0xe -> Shl (dst, src)
            | _ -> failwith "Unexpected"
            end
    | 0x9::x::y::_ -> SneReg (Register.of_int x, Register.of_int y)
    | 0xa::rest -> LdI (UInt16.of_int @@ Nibbles.to_int rest)
    | 0xb::rest -> Jump0 (Address.of_int @@ Nibbles.to_int rest)
    | 0xc::x::rest -> Rnd (Register.of_int x, UInt8.of_int @@ Nibbles.to_int rest)
    | [0xd;x;y;n] -> Draw {xpos=Register.of_int x; ypos=Register.of_int y; height=n}
    | 0xe::x::rest ->
            let r = Register.of_int x in
            begin
            match rest with
            | [0x9;0xe] -> Skp r
            | [0xa;0x1] -> Sknp r
            | _ -> failwith "Unexpected"
            end
    | 0xf::x::rest ->
            let r = Register.of_int x in
            begin
            match rest with
            | [0x0;0x7] -> LdFromDelayTimer r
            | [0x0;0xa] -> LdKey r
            | [0x1;0x5] -> LdDelayTimer r
            | [0x1;0x8] -> LdSoundTimer r
            | [0x1;0xe] -> AddI r
            | [0x2;0x9] -> LdSprite r
            | [0x3;0x3] -> LdBCD r
            | [0x5;0x5] -> LdMemory r
            | [0x6;0x5] -> LdFromMemory r
            | _ -> failwith "Unexpected"
            end
    | _ -> failwith "Unexpected"

let execute (cpu : Cpu.t) memory stack = function
    | Scd n -> failwith "TODO"
    | Scu n -> failwith "TODO"
    | LdReg (vx, vy) ->
            let y = cpu |> Cpu.register_value vy in
            cpu |> Cpu.update_register vx y, stack
    | LdImmediate (vx, n) ->
            cpu |> Cpu.update_register vx n, stack
    | LdI addr ->
            {cpu with i = addr}, stack
    | LdMemory vx -> failwith "TODO"
    | LdFromMemory vx -> failwith "TODO"
    | LdDelayTimer vx ->
            {cpu with dt = cpu |> Cpu.register_value vx}, stack
    | LdFromDelayTimer vx ->
            cpu |> Cpu.update_register vx cpu.dt, stack
    | LdKey vx -> failwith "TODO"
    | LdSoundTimer vx ->
            {cpu with st = cpu |> Cpu.register_value vx}, stack
    | LdSprite vx -> failwith "TODO"
    | LdBCD vx -> failwith "TODO"
    | Cls -> failwith "TODO"
    | Ret ->
            let return_address, stack' = Stack.pop stack in
            {cpu with pc = return_address}, stack'
    | Call addr ->
            let stack' = stack |> Stack.push cpu.pc in
            {cpu with pc = addr}, stack'
    | Or (vx, vy) ->
            let cpu' =
                cpu
                |:> Cpu.register_operation UInt8.logor vx vy
                |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 0) in
            cpu', stack
    | And (vx, vy) ->
            let cpu' =
                cpu
                |:> Cpu.register_operation UInt8.logand vx vy
                |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 0) in
            cpu', stack
    | Xor (vx, vy) ->
            let cpu' =
                cpu
                |:> Cpu.register_operation UInt8.logxor vx vy
                |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 0) in
            cpu', stack
    | Se (vx, n) ->
            let x = cpu |> Cpu.register_value vx in
            if x = n then
                {cpu with pc = UInt16.add cpu.pc (UInt16.of_int 2)}, stack
            else
                cpu, stack
    | SeReg (vx, vy) ->
            let x = cpu |> Cpu.register_value vx in
            let y = cpu |> Cpu.register_value vy in
            if x = y then
                {cpu with pc = UInt16.add cpu.pc (UInt16.of_int 2)}, stack
            else
                cpu, stack
    | Sne (vx, n) ->
            let x = cpu |> Cpu.register_value vx in
            if x <> n then
                {cpu with pc = UInt16.add cpu.pc (UInt16.of_int 2)}, stack
            else
                cpu, stack
    | SneReg (vx, vy) ->
            let x = cpu |> Cpu.register_value vx in
            let y = cpu |> Cpu.register_value vy in
            if x <> y then
                {cpu with pc = UInt16.add cpu.pc (UInt16.of_int 2)}, stack
            else
                cpu, stack
    | Jump addr ->
            {cpu with pc = addr},stack
    | Jump0 addr ->
            let z =
                cpu
                |> Cpu.register_value (Register.of_int 0)
                |> UInt8.to_int
                |> UInt16.of_int
            in
            {cpu with pc = UInt16.add cpu.pc z}, stack
    | Rnd (vx, n) ->
            let newv = UInt8.logand n (Random.int 0xff |> UInt8.of_int) in
            cpu |> Cpu.update_register vx newv, stack
    | AddI vx ->
            let x =
                cpu
                |> Cpu.register_value vx
                |> UInt8.to_int
                |> UInt16.of_int
            in
            {cpu with i = UInt16.add cpu.i x}, stack
    | AddImmediate (vx,v) ->
            let newv = UInt8.add (cpu |> Cpu.register_value vx) v in
            cpu |> Cpu.update_register vx newv, stack
    | Add (vx, vy) ->
            let x = cpu |> Cpu.register_value vx in
            let result, cpu' = cpu |> Cpu.register_operation UInt8.add vx vy in
            if result < x then
                cpu' |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 1), stack
            else
                cpu' |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 0), stack
    | Sub (vx, vy) ->
            let x = cpu |> Cpu.register_value vx in
            let result, cpu' = cpu |> Cpu.register_operation UInt8.sub vx vy in
            if result > x then
                cpu' |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 1), stack
            else
                cpu' |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 0), stack
    | Subn (vx, vy) ->
            let x = cpu |> Cpu.register_value vx in
            let y = cpu |> Cpu.register_value vy in
            let newv = UInt8.sub y x in
            if newv > y then
                cpu |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 1), stack
            else
                cpu |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 0), stack
    | Shr (vx, vy) ->
            let y = cpu |> Cpu.register_value vy in
            let lsbit = UInt8.logand y (UInt8.of_int 1) in
            let newv = UInt8.shift_right y 1 in
            let cpu' =
                cpu
                |> Cpu.update_register vx newv
                |> Cpu.update_register (Register.of_int 0xf) lsbit
            in
            cpu', stack
    | Shl (vx, vy) ->
            let y = cpu |> Cpu.register_value vy in
            let msbit = UInt8.logand y (UInt8.of_int 0x80) in
            let newv = UInt8.shift_left y 1 in
            let cpu' =
                cpu
                |> Cpu.update_register vx newv
                |> Cpu.update_register (Register.of_int 0xf) msbit
            in
            cpu', stack
    | Skp vx -> failwith "TODO"
    | Sknp vx -> failwith "TODO"
    | Draw {xpos : Register.t ; ypos : Register.t ; height : int} -> failwith "TODO"

let rec loop cpu memory stack t =
    (* handle inputs *)
    let cpu' = Cpu.tick_timers cpu in

    let rec fde c s = function
        | 0 -> c, s
        | i ->
            let (instr,cpu') = fetch cpu memory in
            let opcode = decode instr in
            let (cpu'', stack') = execute cpu' memory stack opcode in
            fde cpu'' stack' (i - 1)
    in
    let (cpu'', stack') = fde cpu' stack 10 in
    (* update screen *)
    let timedelta = 1000. *. Unix.gettimeofday () -. t in
    let _ = Unix.sleepf @@ 16.667 -. timedelta in
    let t' = 1000. *. Unix.gettimeofday () in
    loop cpu'' memory stack' t'

let () =
    let nullbyte = UInt8.of_int 0 in
    let memory = Array.make 4096 nullbyte in
    let stack = Stack.create () in
    let cpu : Cpu.t = {
        pc = UInt16.of_int 0x200;
        i = UInt16.of_int 0;
        dt = nullbyte;
        st = nullbyte;
        vr = Registers.create ()
    } in
    loop cpu memory stack (Unix.gettimeofday ())

