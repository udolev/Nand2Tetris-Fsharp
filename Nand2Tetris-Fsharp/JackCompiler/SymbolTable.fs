// Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace JackCompiler

module SymbolTable = 

    type Kind = 
        | Static
        | Field
        | Arg
        | Var

    type Symbol = {
        Name: string
        Type: string
        Kind: Kind
        Index: int
    }

    type SymbolTable() =
        let mutable classTable = Map.empty<string, Symbol>
        let mutable subroutineTable = Map.empty<string, Symbol>

        member this.reset() =
            subroutineTable <- Map.empty

        member this.define(name: string, symbol_type: string, kind: Kind) =
            let index = this.varCount(kind)
            let symbol = { Name = name; Type = symbol_type; Kind = kind; Index = index }
            match kind with
            | Static | Field -> classTable <- classTable.Add(name, symbol)
            | Arg | Var -> subroutineTable <- subroutineTable.Add(name, symbol)

        member this.varCount(kind: Kind) =
            match kind with
            | Static | Field ->
                classTable
                |> Map.filter (fun _ symbol -> symbol.Kind = kind)
                |> Map.count
            | Arg | Var ->
                subroutineTable
                |> Map.filter (fun _ symbol -> symbol.Kind = kind)
                |> Map.count

        member this.kindOf(name: string) =
            match classTable.TryFind(name) with
            | Some(symbol) -> Some symbol.Kind
            | None -> 
                match subroutineTable.TryFind(name) with
                | Some(symbol) -> Some symbol.Kind
                | None -> None

        member this.typeOf(name: string) =
            match classTable.TryFind(name) with
            | Some(symbol) -> symbol.Type
            | None -> 
                match subroutineTable.TryFind(name) with
                | Some(symbol) -> symbol.Type
                | None -> failwith (sprintf "symbol %s does not exist" name)
                    
        member this.indexOf(name: string) =
            match classTable.TryFind(name) with
            | Some(symbol) -> symbol.Index
            | None -> 
                match subroutineTable.TryFind(name) with
                | Some(symbol) -> symbol.Index
                | None -> failwith (sprintf "symbol %s does not exist" name)