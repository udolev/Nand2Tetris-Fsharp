// Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace VMTranslator

open System.IO
open System.Text.RegularExpressions
open VMTranslator.CommandType

module Parser =

    // The parser module gets a vm file and parses each line into a well structured vm command
    type Parser(filePath: string) = 
        // Open the vm file as a stream so we can read it line by line as parser instance initiated
        let fileStream = new StreamReader(filePath)
        let mutable currentCommand: string = "" // Place holder

        // Check if there are more lines to read
        member this.hasMoreLines() : bool = 
            not fileStream.EndOfStream
            
        // Advance to the next command
        member this.advance() = 
            if this.hasMoreLines() then
                currentCommand <- fileStream.ReadLine().Trim()
            else
                failwith "Can't advance: Reached EOF"

            while (Option.isNone (this.commandType()) && this.hasMoreLines()) do
                currentCommand <- fileStream.ReadLine().Trim()

        // Get the current command (For debugging purpoeses)
        member this.getCurrentCommand() : string =
            currentCommand

        // Extract command type from currentCommand according to vm command structure
        // Assumes that all lines are either commands, notes or empty lines
        member this.commandType() : CommandType option = 
            match currentCommand with 
            | cmd when cmd.StartsWith("push") -> Some C_PUSH
            | cmd when cmd.StartsWith("pop") -> Some C_POP
            | cmd when cmd.StartsWith("add") || cmd.StartsWith("sub") || cmd.StartsWith("neg") || 
                       cmd.StartsWith("eq")  || cmd.StartsWith("gt")  || cmd.StartsWith("lt")  || 
                       cmd.StartsWith("and") || cmd.StartsWith("or")  || cmd.StartsWith("not") -> Some C_ARITHMETIC
            | cmd when cmd.StartsWith("label") -> Some C_LABEL
            | cmd when cmd.StartsWith("goto") -> Some C_GOTO
            | cmd when cmd.StartsWith("if-goto") -> Some C_IF
            | cmd when cmd.StartsWith("function") -> Some C_FUNCTION
            | cmd when cmd.StartsWith("call") -> Some C_CALL
            | cmd when cmd.StartsWith("return") -> Some C_RETURN
            | _ -> None

        // Get the first argument of the current command
        member this.arg1() : string =
            if Option.isNone (this.commandType()) then
                failwith "No command provided"
            elif this.commandType() = Some C_RETURN then
                failwith "Cannot use arg1() on RETURN command"
            elif this.commandType() = Some C_ARITHMETIC then 
                currentCommand
            else
                let tokens = Regex.Split(currentCommand, @"\s+")
                tokens[1]

        // Get the second argument of the current command
        member this.arg2() : int =
            if not(List.contains (this.commandType()) [Some C_PUSH; Some C_POP; Some C_FUNCTION; Some C_CALL]) then
                failwith "arg2() can only be called for command types: PUSH, POP, FUNCTION and CALL"
            else
                let tokens = Regex.Split(currentCommand, @"\s+")
                int tokens[2]

        // Close the file stream
        member this.close() =
            fileStream.Close()