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
    let mask = logand (of_int 0xfff)
    let of_int n = of_int n |> mask
    let add x y = add x (of_int y) |> mask
    let sub x y = sub x (of_int y) |> mask
end


module Register : sig
    type t
    val range : int
    val in_range : t -> bool
    val of_int : int -> t
    val to_int : t -> int
    val compare : t -> t -> int
end =
struct
    type t = int
    let range = 0xf
    let in_range n : bool = n >= 0 && n <= range
    let of_int n =
        assert (in_range n);
        n
    let to_int r =
        r
    let compare = Int.compare
end


module Registers = struct
    module RegisterMap = Map.Make(Register)
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

module KeyPadKey : sig
    type t
    val make : Raylib.Key.t -> t option
    val of_code : int -> t
    val to_code : t -> int
    val to_key : t -> Raylib.Key.t
    val compare : t -> t -> int
end =
struct
    open Raylib
    type t = X | One | Two | Three | Q | W | E | A | S | D | Z | C | Four | R | F | V

    let make = function
        | Key.X -> Some X
        | Key.One -> Some One
        | Key.Two -> Some Two
        | Key.Three -> Some Three
        | Key.Q -> Some Q
        | Key.W -> Some W
        | Key.E -> Some E
        | Key.A -> Some A
        | Key.S -> Some S
        | Key.D -> Some D
        | Key.Z -> Some Z
        | Key.C -> Some C
        | Key.Four -> Some Four
        | Key.R -> Some R
        | Key.F -> Some F
        | Key.V -> Some V
        | _ -> None

    let to_key = function
        | X -> Key.X
        | One -> Key.One
        | Two -> Key.Two
        | Three -> Key.Three
        | Q -> Key.Q
        | W -> Key.W
        | E -> Key.E
        | A -> Key.A
        | S -> Key.S
        | D -> Key.D
        | Z -> Key.Z
        | C -> Key.C
        | Four -> Key.Four
        | R -> Key.R
        | F -> Key.F
        | V -> Key.V

    let of_code = function
        | 0x0 -> X
        | 0x1 -> One
        | 0x2 -> Two
        | 0x3 -> Three
        | 0x4 -> Q
        | 0x5 -> W
        | 0x6 -> E
        | 0x7 -> A
        | 0x8 -> S
        | 0x9 -> D
        | 0xa -> Z
        | 0xb -> C
        | 0xc -> Four
        | 0xd -> R
        | 0xe -> F
        | 0xf -> V
        | _ -> invalid_arg "key code"

    let to_code = function
        | X -> 0x0
        | One -> 0x1
        | Two -> 0x2
        | Three -> 0x3
        | Q -> 0x4
        | W -> 0x5
        | E -> 0x6
        | A -> 0x7
        | S -> 0x8
        | D -> 0x9
        | Z -> 0xa
        | C -> 0xb
        | Four -> 0xc
        | R -> 0xd
        | F -> 0xe
        | V -> 0xf

    let compare k1 k2 =
        Int.compare (to_code k1) (to_code k2)
end

module KeyState : sig
    type t
    val create : unit -> t
    val get : KeyPadKey.t -> t -> bool
    val released : t -> KeyPadKey.t option
    val update : t -> t
    val print : t -> unit
end =
struct
    module KeyMap = Map.Make(KeyPadKey)
    type t = bool KeyMap.t * KeyPadKey.t option
    let create () = KeyMap.of_list @@ List.init 0xf (fun x -> (KeyPadKey.of_code x,false)), None
    let get n ks = fst ks |> KeyMap.find n
    let set n ks = fst ks |> KeyMap.update n (Option.map (fun _ -> true)), snd ks
    let unset n ks = fst ks |> KeyMap.update n (Option.map (fun _ -> false)), Some n
    let released ks = snd ks
    let update (ks : t) =
        let open Raylib in
        let queued_keys =
            let rec aux = function
            | Key.Null -> []
            | x ->
                    begin
                    match KeyPadKey.make x with
                    | None -> aux (get_key_pressed ())
                    | Some k -> k :: aux (get_key_pressed ())
                    end
            in
            aux (Raylib.get_key_pressed ())
        in
        (fst ks, None)
        |> begin
        fst ks |> KeyMap.filter (fun _ state -> state)
        |> KeyMap.bindings
        |> List.map fst
        |> List.map (fun k ks ->
                match KeyPadKey.to_key k |> is_key_up with
                | true -> ks |> unset k
                | false -> ks)
        |> List.fold_left (>>) (fun x -> x)
        end |> begin
        queued_keys
        |> List.map (fun k ks -> ks |> set k)
        |> List.fold_left (>>) (fun x -> x)
        end
    let print ks =
        fst ks |> KeyMap.iter (fun k state -> Printf.eprintf "%x : %B\n" (KeyPadKey.to_code k) state);
        let last =
            match ks |> released with
            | None -> 0
            | Some x -> KeyPadKey.to_code x
        in
        Printf.eprintf "released: %x\n" last
end

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
    val create : unit -> t
    val load : t -> UInt8.t List.t -> Address.t -> unit
    val load_bytes : t -> Address.t -> bytes -> int -> unit
    val get : t -> Address.t -> UInt8.t
end =
struct
    type t = UInt8.t Array.t
    let create () = Array.make 4096 (UInt8.of_int 0)
    let load m src addr =
        let rec aux xs i =
            match xs with
            | [] -> ()
            | x::rest ->
                    let () = Array.set m (UInt16.to_int i) x in
                    aux rest (Address.add i 1)
        in
        aux src addr
    let load_bytes m addr bs n =
        Bytes.sub bs 0 n
        |> Bytes.iteri (fun i b ->
                let idx = UInt16.to_int (Address.add addr i) in
                Array.set m idx (UInt8.of_int @@ Char.code b))
    let get m addr = Array.get m (UInt16.to_int addr)
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
    val inc_pc : t -> t
    val dec_pc : t -> t
end =
struct
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
    let inc_pc c =
        {
            c with
            pc = UInt16.(add c.pc (of_int 2))
        }
    let dec_pc c =
        {
            c with
            pc = UInt16.(sub c.pc (of_int 2))
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
    val nibble_one : UInt8.t -> int * int
    val make : UInt8.t list -> t
    val to_int : t -> int
end =
struct
    type t = int list
    let nibble_one x =
        let x' = (UInt8.to_int x) in
            ( x' lsr 4 land 0xf,
              x' land 0xf)
    let nibbles x =
        let ns = nibble_one x in
        [
            fst ns;
            snd ns;
        ]
    let make bs = List.map nibbles bs |> List.concat
    let to_int ns =
        let rec aux acc xs =
            match xs with
            | [] -> acc
            | h::t -> aux (acc lsl 4 lor h) t
        in
        aux 0 ns
end

module FrameBuffer = struct
    type t = int Array.t
    let create () = Array.make 32 0
    let clear fb = Array.fill fb 0 32 0
end


type chop8 =
    {
        memory : Memory.t;
        fb : FrameBuffer.t;
        stack : UInt16.t Stack.t;
        cpu : Cpu.t;
        keystate : KeyState.t;
        waiting : bool;
    }

let fetch (cpu : Cpu.t) memory : int list * Cpu.t =
    let instr =
        Array.sub memory (UInt16.to_int cpu.pc) 2
        |> Array.to_list
        |> Nibbles.make
    in
    let cpu' = cpu |> Cpu.inc_pc in
    let () = Printf.eprintf "\n[" in
    let () = instr |> List.iter (Printf.eprintf "%#x ; " ) in
    let () = Printf.eprintf "]%!" in
    instr,cpu'

exception InstructionDecodeError of string
let instruction_decode_error op =
    let repr = Printf.sprintf "%#4x" (Nibbles.to_int op) in
    raise (InstructionDecodeError repr)

let decode op =
    match op with
    | 0x0::rest ->
            begin
            match rest with
            | [0x0; 0xe; 0x0] -> Cls
            | [0x0; 0xe; 0xe] -> Ret
            | _ -> instruction_decode_error op
            end
    | 0x1::rest -> Jump (Address.of_int @@ Nibbles.to_int rest)
    | 0x2::rest -> Call (Address.of_int @@ Nibbles.to_int rest)
    | 0x3::x::rest -> Se (Register.of_int x, UInt8.of_int @@ Nibbles.to_int rest)
    | 0x4::x::rest -> Sne (Register.of_int x, UInt8.of_int @@ Nibbles.to_int rest)
    | [0x5;x;y;0x0] -> SeReg (Register.of_int x, Register.of_int y)
    | 0x6::x::rest -> LdImmediate (Register.of_int x, UInt8.of_int @@ Nibbles.to_int rest)
    | 0x7::x::rest -> AddImmediate (Register.of_int x, UInt8.of_int @@ Nibbles.to_int rest)
    | [0x8;x;y;last] ->
            let dst,src = Register.(of_int x, of_int y) in
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
            | _ -> instruction_decode_error op
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
            | _ -> instruction_decode_error op
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
            | _ -> instruction_decode_error op
            end
    | _ -> instruction_decode_error op

let reg_value cpu r = cpu |> Cpu.register_value r
let load_reg cpu r v = cpu |> Cpu.update_register r v

let execute c = 
    let v = reg_value c.cpu in
    let load = load_reg c.cpu in
    function
    | LdReg (x, y) ->
            {c with
                cpu = load x (v y) }
    | LdImmediate (x, n) ->
            {c with
                cpu = load x n }
    | LdI addr ->
            {c with
                cpu = {c.cpu with
                    i = addr}}
    | LdMemory x ->
            let data =
                0 -- Register.to_int x
                |> List.map Register.of_int
                |> List.map (fun i ->
                        c.cpu |> Cpu.register_value i)
            in
            let () = Memory.load c.memory data c.cpu.i in
            let increment =
                UInt16.(add
                    (of_int (Register.to_int x))
                    (of_int 1))
            in
            {c with
                cpu = {c.cpu with
                    i = UInt16.add c.cpu.i increment}}
    | LdFromMemory x ->
            let range = 0 -- Register.to_int x in
            let data =
                range
                |> List.map UInt16.(fun i -> add c.cpu.i (of_int i))
                |> List.map @@ Memory.get c.memory
            in
            let cpu' =
                c.cpu
                |> begin
                List.map2 (
                    fun i b cpu ->
                            cpu |> Cpu.update_register i b)
                    (range |> List.map Register.of_int)
                    data
                |> List.fold_left (>>) (fun x -> x)
                end
            in
            let increment =
                UInt16.(add
                    (of_int (Register.to_int x))
                    (of_int 1))
            in
            {c with
                cpu = {cpu' with
                    i = UInt16.add c.cpu.i increment}}
    | LdDelayTimer x ->
            {c with
                cpu = {c.cpu with
                    dt = v x} }
    | LdFromDelayTimer x ->
            {c with
                cpu = load x c.cpu.dt }
    | LdKey x -> failwith "TODO" (* break and wait for next released key i htink *)
    | LdSoundTimer x ->
            {c with
                cpu = {c.cpu with
                    st = v x} }
    | LdSprite vx -> failwith "TODO"
    | LdBCD vx -> failwith "TODO"
    | Cls ->
            let () = FrameBuffer.clear c.fb in
            c
    | Ret ->
            let return_address, stack' = Stack.pop c.stack in
            {c with
                cpu = {c.cpu with
                    pc = return_address} }
    | Call addr ->
            let stack' = c.stack |> Stack.push c.cpu.pc in
            {c with
                cpu = {c.cpu with
                    pc = Address.sub addr 2};
                stack = stack' }
    | Or (x, y) ->
            let cpu' =
                c.cpu
                |:> Cpu.register_operation UInt8.logor x y
                |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 0) in
            {c with
                cpu = cpu' }
    | And (x, y) ->
            let cpu' =
                c.cpu
                |:> Cpu.register_operation UInt8.logand x y
                |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 0) in
            {c with
                cpu = cpu' }
    | Xor (x, y) ->
            let cpu' =
                c.cpu
                |:> Cpu.register_operation UInt8.logxor x y
                |> Cpu.update_register (Register.of_int 0xf) (UInt8.of_int 0) in
            {c with
                cpu = cpu' }
    | Se (x, n) ->
            if v x = n then
                {c with
                    cpu = c.cpu |> Cpu.inc_pc}
            else c
    | SeReg (x, y) ->
            if v x = v y then
                {c with
                    cpu = c.cpu |> Cpu.inc_pc}
            else c
    | Sne (x, n) ->
            if v x <> n then
                {c with
                    cpu = c.cpu |> Cpu.inc_pc}
            else c
    | SneReg (x, y) ->
            if v x <> v y then
                {c with
                    cpu = c.cpu |> Cpu.inc_pc}
            else c
    | Jump addr ->
            {c with
                cpu = {c.cpu with
                    pc = addr}}
    | Jump0 addr ->
            let z =
                c.cpu
                |> Cpu.register_value (Register.of_int 0)
                |> UInt8.to_int
                |> UInt16.of_int
            in
            {c with
                cpu = {c.cpu with
                    pc = UInt16.add c.cpu.pc z}}
    | Rnd (x, n) ->
            let newv = UInt8.logand n (Random.int 0xff |> UInt8.of_int) in
            {c with
                cpu = load x newv}
    | AddI x ->
            let vx =
                v x
                |> UInt8.to_int
                |> UInt16.of_int
            in
            {c with
                cpu = {c.cpu with
                    i = UInt16.add c.cpu.i vx}}
    | AddImmediate (x,n) ->
            let newval = UInt8.add (v x) n in
            {c with
                cpu = load x newval}
    | Add (x, y) ->
            let vx = v x in
            let vx', cpu' = c.cpu |> Cpu.register_operation UInt8.add x y in
            let f = Register.of_int 0xf in
            if vx' < vx then
                {c with
                    cpu = cpu' |> Cpu.update_register f (UInt8.of_int 1)}
            else
                {c with
                    cpu = cpu' |> Cpu.update_register f (UInt8.of_int 0)}
    | Sub (x, y) ->
            let vx = v x in
            let vx', cpu' = c.cpu |> Cpu.register_operation UInt8.sub x y in
            let f = Register.of_int 0xf in
            if vx' > vx then
                {c with
                    cpu = cpu' |> Cpu.update_register f (UInt8.of_int 1)}
            else
                {c with
                    cpu = cpu' |> Cpu.update_register f (UInt8.of_int 0)}
    | Subn (x, y) ->
            let vx = v x in
            let vy = v y in
            let newv = UInt8.sub vy vx in
            let f = Register.of_int 0xf in
            if newv > vy then
                {c with
                    cpu = load f (UInt8.of_int 1)}
            else
                {c with
                    cpu = load f (UInt8.of_int 0)}
    | Shr (x, y) ->
            let vy = v y in
            let lsbit = UInt8.(logand vy (of_int 1)) in
            let newv = UInt8.shift_right vy 1 in
            let cpu' =
                c.cpu
                |> Cpu.update_register x newv
                |> Cpu.update_register (Register.of_int 0xf) lsbit
            in
            {c with
                cpu = cpu'}
    | Shl (x, y) ->
            let vy = v y in
            let msbit = UInt8.(logand vy (of_int 0x80)) in
            let newv = UInt8.shift_left vy 1 in
            let cpu' =
                c.cpu
                |> Cpu.update_register x newv
                |> Cpu.update_register (Register.of_int 0xf) msbit
            in
            {c with
                cpu = cpu'}
    | Skp x ->
            let (_,ln) = Nibbles.nibble_one (v x) in
            let key = KeyPadKey.of_code ln in
            if c.keystate |> KeyState.get key then
                {c with
                    cpu = c.cpu |> Cpu.inc_pc}
            else c
    | Sknp x ->
            let (_,ln) = Nibbles.nibble_one (v x) in
            let key = KeyPadKey.of_code ln in
            if not (c.keystate |> KeyState.get key) then
                {c with
                    cpu = c.cpu |> Cpu.inc_pc}
            else c
    | Draw {xpos : Register.t ; ypos : Register.t ; height : int} ->
            {c with
                waiting = true}

let render fb =
    let draw_pixel (x,y) p =
        let color =
            match p with
            | 1 -> Raylib.Color.white
            | _ -> Raylib.Color.black
        in
        Raylib.draw_pixel x y color
    in
    let draw_row y pixels =
        0 -- (width - 1)
        |> List.iter (fun x ->
                let p = pixels lsr (63 - x) land 1 in
                draw_pixel (x,y) p)
    in
    let () = Raylib.begin_drawing () in
    let () =
        fb
        |> Array.iteri (fun y pixels ->
                draw_row y pixels)
    in
    Raylib.end_drawing ()

let rec fde c = function
    | 0 -> c
    | i ->
        let (instr,cpu') = fetch c.cpu c.memory in
        let opcode = decode instr in
        let c' = execute {c with cpu = cpu'} opcode in
        match c'.waiting with
        | true -> c'
        | false -> fde c' (i - 1)

let rec loop c =
    match Raylib.window_should_close () with
    | true -> Raylib.close_window ()
    | false ->
        let keystate' = c.keystate |> KeyState.update in
        let () = keystate' |> KeyState.print in
        let () = Printf.eprintf "%!" in
        let cpu' = Cpu.tick_timers c.cpu in
        let ipf = if c.waiting then 0 else 10 in
        let c' = fde {c with cpu = cpu'; keystate = keystate'} ipf in
        let timedelta = Raylib.get_frame_time () in
        let () = render c.fb in
        let () = Raylib.wait_time @@ Float.max 0. (0.016667 -. timedelta) in
        loop c'

let () =
    let () = Raylib.init_window width height "chop8" in
    let () = Raylib.set_target_fps 60 in
    let fb = FrameBuffer.create () in
    let memory = Memory.create () in
    let () = Memory.load memory font (Address.of_int 0x050) in
    let buf = Bytes.make 0xdff (Char.chr 0) in
    let nbytes = Unix.read Unix.stdin buf 0 0xdff in
    let () = Memory.load_bytes memory (Address.of_int 0x200) buf nbytes in
    (* let () = memory |> Array.iter (UInt8.to_int >> Printf.eprintf "%#02x ") in *)
    let stack = Stack.create () in
    let cpu : Cpu.t = {
        pc = UInt16.of_int 0x200;
        i = UInt16.of_int 0;
        dt = UInt8.of_int 0;
        st = UInt8.of_int 0;
        vr = Registers.create ()
    } in
    let chip8 = {
        memory;
        fb;
        stack;
        cpu;
        keystate = KeyState.create ();
        waiting = false;
    } in
    loop chip8

