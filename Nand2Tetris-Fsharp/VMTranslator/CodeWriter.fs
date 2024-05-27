namespace VMTranslator

open System.IO
open VMTranslator.CommandType

module CodeWriter =

    type CodeWriter(outputFile: string) =
        let fileStream = new StreamWriter(outputFile, true)
        let mutable arthJumpFlag = 0

        // Helper function to write a single line to the output file
        let writeLine (line: string) = 
            fileStream.WriteLine(line)

        // Helper function to write multiple lines to the output file
        let writeLines (lines: string list) =
            lines |> List.iter fileStream.WriteLine

        // Template for two-operand arithmetic operations (add, sub, and, or)
        let arithmeticTemplateTwoOperands() =
            [
                "@SP"
                "AM=M-1"
                "D=M"
                "A=A-1"
            ]

        // Template for comparison operations (gt, lt, eq)
        let arithmeticTemplateComparison (jumpCondition: string) (flag: int) =
            [
                "@SP"
                "AM=M-1"
                "D=M"
                "A=A-1"
                "D=M-D"
                sprintf "@FALSE%d" flag
                sprintf "D;%s" jumpCondition
                "@SP"
                "A=M-1"
                "M=-1"
                sprintf "@CONTINUE%d" flag
                "0;JMP"
                sprintf "(FALSE%d)" flag
                "@SP"
                "A=M-1"
                "M=0"
                sprintf "(CONTINUE%d)" flag
            ]

        // Template for push operations
        let pushTemplate (segment: string) (index: int) (isDirect: bool) =
            let noPointerCode = 
                if isDirect then 
                    [] 
                else 
                    [
                        sprintf "@%d" index
                        "A=D+A"
                        "D=M"
                    ]

            [
                sprintf "@%s" segment
                "D=M"
            ] @ noPointerCode @ [
                "@SP"
                "A=M"
                "M=D"
                "@SP"
                "M=M+1"
            ]

        // Template for pop operations
        let popTemplate (segment: string) (index: int) (isDirect: bool) =
            let noPointerCode = 
                if isDirect then 
                    "D=A"
                else 
                    [
                        "D=M"
                        sprintf "@%d" index
                        "D=D+A"
                    ] |> String.concat "\n"

            [
                sprintf "@%s" segment
                noPointerCode
                "@R13"
                "M=D"
                "@SP"
                "AM=M-1"
                "D=M"
                "@R13"
                "A=M"
                "M=D"
            ]

        // Writes the assembly code that is the translation of the given arithmetic command
        member this.writeArithmetic(command: string) =
            let arthLines =
                match command with
                | "add" -> arithmeticTemplateTwoOperands() @ ["M=M+D"]
                | "sub" -> arithmeticTemplateTwoOperands() @ ["M=M-D"]
                | "and" -> arithmeticTemplateTwoOperands() @ ["M=M&D"]
                | "or" -> arithmeticTemplateTwoOperands() @ ["M=M|D"]
                | "gt" -> 
                    let lines = arithmeticTemplateComparison "JLE" arthJumpFlag
                    arthJumpFlag <- arthJumpFlag + 1
                    lines
                | "lt" -> 
                    let lines = arithmeticTemplateComparison "JGE" arthJumpFlag
                    arthJumpFlag <- arthJumpFlag + 1
                    lines
                | "eq" -> 
                    let lines = arithmeticTemplateComparison "JNE" arthJumpFlag
                    arthJumpFlag <- arthJumpFlag + 1
                    lines
                | "not" -> ["@SP"; "A=M-1"; "M=!M"]
                | "neg" -> ["D=0"; "@SP"; "A=M-1"; "M=D-M"]
                | _ -> failwith "Invalid arithmetic command"
            
            writeLines (["// " + command] @ arthLines)

        // Writes the assembly code that is the translation of the given command where the command is either PUSH or POP
        member this.writePushPop(commandType: CommandType, segment: string, index: int) =
            let pushPopLines =
                match commandType with
                | C_PUSH ->
                    match segment with
                    | "constant" ->
                        [
                            sprintf "@%d" index
                            "D=A"
                            "@SP"
                            "A=M"
                            "M=D"
                            "@SP"
                            "M=M+1"
                        ]
                    | "local" -> pushTemplate "LCL" index false
                    | "argument" -> pushTemplate "ARG" index false
                    | "this" -> pushTemplate "THIS" index false
                    | "that" -> pushTemplate "THAT" index false
                    | "temp" -> pushTemplate "R5" (index + 5) false
                    | "pointer" when index = 0 -> pushTemplate "THIS" index true
                    | "pointer" when index = 1 -> pushTemplate "THAT" index true
                    | "static" -> pushTemplate (sprintf "%s.%d" outputFile index) index true
                    | _ -> failwith "Invalid segment for push"
                | C_POP ->
                    match segment with
                    | "local" -> popTemplate "LCL" index false
                    | "argument" -> popTemplate "ARG" index false
                    | "this" -> popTemplate "THIS" index false
                    | "that" -> popTemplate "THAT" index false
                    | "temp" -> popTemplate "R5" (index + 5) false
                    | "pointer" when index = 0 -> popTemplate "THIS" index true
                    | "pointer" when index = 1 -> popTemplate "THAT" index true
                    | "static" -> popTemplate (sprintf "%s.%d" outputFile index) index true
                    | _ -> failwith "Invalid segment for pop"
                | _ -> failwith "Invalid command type for writePushPop"

            writeLines (["// " + (if commandType = C_PUSH then "push" else "pop") + " " + segment + " " + index.ToString()] @ pushPopLines)

        // Closes the output file stream
        member this.close() =
            fileStream.Close()
