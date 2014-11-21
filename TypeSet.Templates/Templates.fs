module TypeSet.Templates.Parser

open Microsoft.FSharp.Quotations

type ParserState =
    {
        Vars : Map<string * string, Var>
        Tree : Expr
        Rest : char list
    }
    static member Empty =
        { Vars = Map.empty; Tree = <@@ List.empty<string> @@>; Rest = [] }

let charsToString chars =
    chars
    |> List.rev
    |> List.map string
    |> String.concat ""

let stringToChars (str : string) =
    str
    |> List.ofSeq

let (|Int|Str|Dt|) typeName =
    match typeName with
    | "int" -> Int
    | "str" -> Str
    | "dt" -> Dt
    | s -> failwithf "Unknown type %s" s

let typeNameToType typeName =
    match typeName with
    | Int -> typeof<int>
    | Str -> typeof<string>
    | Dt -> typeof<System.DateTime>

let createPrinter var typeName =
    match typeName with
    | Str ->
        Expr.Var var
    | Int ->
        <@@ string<int> (%%Expr.Var var) @@>
    | Dt ->
        <@@ string<System.DateTime> (%%Expr.Var var) @@>

let rec parseField state fieldName chars =
    match state.Rest with
    | [] ->
        failwith "Templates should not end mid-field."
    | ' '::'}'::'}'::t ->
        match fieldName with
        | None ->
            failwith "You must specify a name and type for every field."
        | Some f ->
            let typeName =
                (charsToString chars).Trim()
            let (v, vars) =
                match Map.tryFind (f, typeName) state.Vars with
                | Some var -> (var, state.Vars)
                | None ->
                    let x = Var(f, typeNameToType typeName)
                    (x, Map.add (f, typeName) x state.Vars)
            let expr =
                <@@ (%%createPrinter v typeName:string)::(%%state.Tree) @@>
            { state with Rest = t; Tree = expr; Vars = vars }
    | ':'::t ->
        let fieldName =
            (charsToString chars).Trim()
        parseField { state with Rest = t } (Some fieldName) []
    | c::t ->
        parseField { state with Rest = t } fieldName (c::chars)

let appendBuffer tree buffer =
    let str = Expr.Value(charsToString buffer)
    <@@ (%%str:string)::(%%tree) @@>

let rec parseTemplate state buffer =
    match state.Rest with
    | '{'::'{'::' '::t ->
        let newState =
            { state with
                Rest = t
                Tree = appendBuffer state.Tree buffer }
        parseTemplate (parseField newState None []) []
    | c::t ->
        parseTemplate { state with Rest = t } (c::buffer)
    | [] ->
        let finalExpr = <@@ (%%appendBuffer state.Tree buffer:string list) |> List.rev |> String.concat "" @@>
        state.Vars, finalExpr