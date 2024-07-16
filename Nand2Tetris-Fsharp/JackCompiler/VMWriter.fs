// Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace JackCompiler

open System.IO

module VMWriter = 

    type Segment =
        | Constant
        | Argument
        | Local
        | Static
        | This
        | That
        | Pointer
        | Temp

    type Command =
        | Add
        | Sub
        | Neg
        | Eq
        | Gt
        | Lt
        | And
        | Or
        | Not

    type VMWriter(output: StreamWriter) =

        member this.writePush(segment: Segment, index: int) =
            let segmentStr = 
                match segment with
                | Constant -> "constant"
                | Argument -> "argument"
                | Local -> "local"
                | Static -> "static"
                | This -> "this"
                | That -> "that"
                | Pointer -> "pointer"
                | Temp -> "temp"
            output.WriteLine(sprintf "push %s %d" segmentStr index)

        member this.writePop(segment: Segment, index: int) =
            let segmentStr = 
                match segment with
                | Constant -> "constant"
                | Argument -> "argument"
                | Local -> "local"
                | Static -> "static"
                | This -> "this"
                | That -> "that"
                | Pointer -> "pointer"
                | Temp -> "temp"
            output.WriteLine(sprintf "pop %s %d" segmentStr index)

        member this.writeArithmetic(command: Command) =
            let commandStr = 
                match command with
                | Add -> "add"
                | Sub -> "sub"
                | Neg -> "neg"
                | Eq -> "eq"
                | Gt -> "gt"
                | Lt -> "lt"
                | And -> "and"
                | Or -> "or"
                | Not -> "not"
            output.WriteLine(commandStr)

        member this.writeLabel(label: string) =
            output.WriteLine(sprintf "label %s" label)

        member this.writeGoto(label: string) =
            output.WriteLine(sprintf "goto %s" label)

        member this.writeIf(label: string) =
            output.WriteLine(sprintf "if-goto %s" label)

        member this.writeCall(name: string, nArgs: int) =
            output.WriteLine(sprintf "call %s %d" name nArgs)

        member this.writeFunction(name: string, nVars: int) =
            output.WriteLine(sprintf "function %s %d" name nVars)

        member this.writeReturn() =
            output.WriteLine("return")

        member this.close() =
            output.Close()