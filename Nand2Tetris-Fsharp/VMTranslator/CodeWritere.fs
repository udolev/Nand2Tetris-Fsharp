namespace VMTranslator

open System.IO
open VMTranslator.CommandType

module CodeWritere =
    
    // Writes VM commands to the output file in Hack assembly language.
    type CodeWriter(filePath: string) = 
         // Opens the output file as a stream so we can translate each vm command and write the asm in the output file
        let fileStream = new StreamWriter(filePath)
        
        // Writes the translation of the given arithmetic command.
        member this.writeArithmetic(command : string) = 
            None

        // Writes the assembly code that is the translation of the given command.
        member this.writePushPop(command : CommandType option, segment : string, index : int) = 
            match command with
            | Some C_PUSH -> 
                None
            | Some C_POP  -> 
                None
            | _           -> failwith "Invalid command type"

        // Close the file stream
        member this.close() =
            fileStream.Close()