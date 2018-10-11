open System
open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Concurrent

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type IAst = obj

[<CustomEquality; CustomComparison>]
type Value = 
    | String of string
    | Symbol of SymbolImpl
    | Int of int
    | List of Value list
    | Fn of (Locals -> Value list -> Result)

    override this.Equals(obj) =
        if obj :? Value then
            match (this, obj :?> Value) with
            | String(xv), String(yv) -> xv = yv
            | Symbol(xv), Symbol(yv) -> LanguagePrimitives.PhysicalEquality xv yv
            | Int(xv), Int(yv) -> xv = yv
            | List(xv), List(yv) -> xv = yv
            | Fn(xv), Fn(yv) -> LanguagePrimitives.PhysicalEquality xv yv
            | _, _ -> false
        else 
            false

    override this.GetHashCode() =
        match this with
        | String(v) -> hash(v)
        | Symbol(v) -> LanguagePrimitives.PhysicalHash(v)
        | Int(v) -> hash(v)
        | List(v) -> hash(v)
        | Fn(v) -> LanguagePrimitives.PhysicalHash(v)

    interface System.IComparable with
        member this.CompareTo other =
            if other :? Value then
                match (this, other :?> Value) with
                | Symbol(a), Symbol(b) ->
                    if LanguagePrimitives.PhysicalEquality a b then
                        0
                    elif LanguagePrimitives.PhysicalHash(a) < LanguagePrimitives.PhysicalHash(b) then
                        -1
                    else
                        1
                | _, _ -> -1
            else 
                -1



 
and SymbolImpl(nm:string) =

    static let intern_table:ConcurrentDictionary<string, SymbolImpl> = ConcurrentDictionary()

    static member intern(nm:string) =
        match intern_table.TryGetValue nm with
        | true, s -> Symbol(s)
        | false, _ -> 
            let s = SymbolImpl(nm) in
              if intern_table.TryAdd(nm, s) then
                Symbol(s)
              else 
                SymbolImpl.intern nm
    
    override this.ToString() = nm
   
and Locals = (Value * Value) list

and Result = 
    | RValue of Value
    | Effect of Value * (Value -> Result)

let if_sym = SymbolImpl.intern "if"
let set_global_sym = SymbolImpl.intern "set-global"
let get_global_sym = SymbolImpl.intern "get-global"



let rec make_k f k v = 
    let r = k v in
        match r with
        | RValue(vi) -> f vi
        | Effect(s, k) ->
            Effect(s, make_k f k)

let rec enclose_k f k = 
    fun v ->
        let r = k v in
            match r with
            | RValue(vi) -> f vi
            | Effect(s, k) ->
                Effect(s, enclose_k f k)

type EffectBuilder() =
    member this.Return(x:Value) = RValue(x)
    member this.ReturnFrom(x:Result) = x

    member this.Bind(x:Result, f:(Value->Result)) =
        match x with
        | RValue(v) -> f v
        | Effect(s, k) ->
            Effect(s, enclose_k f k)
            
      

let effect = EffectBuilder()

let get_global nm = 
    Effect(List([get_global_sym; nm]), RValue)

let set_global nm v =
    Effect(List([set_global_sym; nm; v]), RValue)

let rec lookup (env:Locals) (sym:Value) = 
    effect {
        match env with
        | [] -> return! get_global sym
        | (k, v) :: _ when k = sym -> return v
        | _ :: tail -> return! lookup tail sym
    }

let rec eval (env:Locals) (form:Value) = 
    effect {
        match form with
        | Symbol(_) -> return! lookup env form
        | List(lst) -> return! eval_sexpr env lst
        | any -> return any
    }

and eval_sexpr (env:Locals) (lst:Value list) =
    effect {
        let f :: args = lst in
        let! (Fn(rfn)) = eval env f in
        return! rfn env args
    }

let rec emap (f: Value -> Result) (lst:Value list) =
    effect {
        match lst with
        | [] -> return List([])
        | h :: tail ->
            let! List(new_tail) = emap f tail
            let! new_head = f h
            return List(new_head :: new_tail)
    }

let wrap_evaled (f: Value list -> Result) (env:Locals) (args:Value list) =
    effect {
        let! result = emap (eval env) args
        match result with 
        | List(eargs) -> return! f eargs
        | _ -> return failwith "Bad arguments"
    }

let rec add_fn args =
    effect {
        match args with
        | [] -> return Int(0)
        | [Int(_) as v] -> return v 
        | Int(x) :: Int(y) :: tail -> return! add_fn (Int(x + y) :: tail)
        | _ -> return! failwith "Can't add types"
    }

let wrapped = wrap_evaled add_fn

let default_globals:Map<Value, Value> = [(SymbolImpl.intern("+"), Fn(wrap_evaled <| add_fn))]
                                        |> Map.ofList


let rec run_all (globals:Map<Value, Value>) x =
    match x with
    | RValue(v) -> v
    | Effect(List([cmd; nm]),  k) when cmd = get_global_sym ->
        let value = globals.Item(nm) in
        let new_val = k value
        run_all globals new_val
    | Effect(List([cmd; nm; v]), k) when cmd = set_global_sym ->
        run_all <| globals.Add(nm, v) <| (k v)
    | _ -> failwith "unknown effect "

[<EntryPoint>]
let main argv = 
    let prog = List([SymbolImpl.intern("+"); Int(1); Int(2)])
    printfn "%A" (run_all default_globals (eval [] prog))
    0 // return an integer exit code
