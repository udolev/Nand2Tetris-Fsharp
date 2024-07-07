// Exercise 2 - Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace VMTranslator

open System.IO
open VMTranslator.CommandType

module CodeWriter =

    // The CodeWriter module writes different vm commands into an output file in Hack language
    type CodeWriter(outputFile: string) =
        let fileStream = new StreamWriter(outputFile)
        let mutable arthJumpCounter:int = 0
        let mutable retAddrCounter:int = 0
        let mutable fileName:string = ""

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
                "D=M" // y
                "A=A-1"
                "D=M-D" // x - y
                sprintf "@TRUE%d" flag
                sprintf "D;%s" jumpCondition
                "@SP"
                "A=M-1"
                "M=0" // false
                sprintf "@CONTINUE%d" flag
                "0;JMP"
                sprintf "(TRUE%d)" flag
                "@SP"
                "A=M-1"
                "M=-1" // true
                sprintf "(CONTINUE%d)" flag
            ]

        // Template of instructuins in Hack to push a value found in a specific variable to the stack.
        // To use, calculate address of the variable in A.
        let memoryPushInstructions = 
            [
                "D=M"
                "@SP"
                "A=M"
                "M=D"
                "@SP"
                "M=M+1"
            ]

        // Function to generate push code for different segments and indeces
        let pushTemplate baseAddress index =
            [
                sprintf "@%s" baseAddress
                "D=M"
                sprintf "@%d" index
                "A=D+A"
            ] @ memoryPushInstructions

        // Template of instructuins in Hack to pop a value to a specific variable location in the memory.
        // To use, calculate address of variable and save it in D.
        let memoryPopInstructions = 
            [
                "@R13" // save address in general register
                "M=D"
                "@SP"
                "AM=M-1"
                "D=M"
                "@R13" // retrive address
                "A=M"
                "M=D"
            ]

        // Function to generate pop code for different segments and indeces
        let popTemplate baseAddress index =
            [
                sprintf "@%s" baseAddress
                "D=M"
                sprintf "@%d" index
                "D=D+A" // base address + index
            ] @ memoryPopInstructions

        // Writes the assembly code that is the translation of the given arithmetic command
        member this.writeArithmetic(command: string) =
            let arthLines =
                match command with
                | "add" -> arithmeticTemplateTwoOperands() @ ["M=M+D"]
                | "sub" -> arithmeticTemplateTwoOperands() @ ["M=M-D"]
                | "and" -> arithmeticTemplateTwoOperands() @ ["M=M&D"]
                | "or" -> arithmeticTemplateTwoOperands() @ ["M=M|D"]
                | "gt" -> 
                    let lines = arithmeticTemplateComparison "JGT" arthJumpCounter
                    arthJumpCounter <- arthJumpCounter + 1
                    lines
                | "lt" -> 
                    let lines = arithmeticTemplateComparison "JLT" arthJumpCounter
                    arthJumpCounter <- arthJumpCounter + 1
                    lines
                | "eq" -> 
                    let lines = arithmeticTemplateComparison "JEQ" arthJumpCounter
                    arthJumpCounter <- arthJumpCounter + 1
                    lines
                | "not" -> ["@SP"; "A=M-1"; "M=!M"]
                | "neg" -> ["@SP"; "A=M-1"; "M=-M"]
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
                    | "local" -> pushTemplate "LCL" index 
                    | "argument" -> pushTemplate "ARG" index
                    | "this" -> pushTemplate "THIS" index
                    | "that" -> pushTemplate "THAT" index
                    | "temp" -> [sprintf "@%d" (5+index)] @ memoryPushInstructions
                    | "pointer" when index = 0 -> ["@THIS"] @ memoryPushInstructions
                    | "pointer" when index = 1 -> ["@THAT"] @ memoryPushInstructions
                    | "static" -> [sprintf "@%s.%d" fileName index] @ memoryPushInstructions
                    | _ -> failwith "Invalid segment for push"
                | C_POP ->
                    match segment with
                    | "local" -> popTemplate "LCL" index
                    | "argument" -> popTemplate "ARG" index
                    | "this" -> popTemplate "THIS" index
                    | "that" -> popTemplate "THAT" index
                    | "temp" ->
                        [
                            sprintf "@%d" (5 + index)
                            "D=A"
                        ] @ memoryPopInstructions
                    | "pointer" when index = 0 ->
                        [
                            "@SP"
                            "AM=M-1"
                            "D=M"
                            "@THIS"
                            "M=D"
                        ]
                    | "pointer" when index = 1 ->
                        [
                            "@SP"
                            "AM=M-1"
                            "D=M"
                            "@THAT"
                            "M=D"
                        ]
                    | "static" ->
                        [
                            "@SP"
                            "AM=M-1"
                            "D=M"
                            sprintf "@%s.%d" fileName index
                            "M=D"
                        ]
                    | _ -> failwith "Invalid segment for pop"
                | _ -> failwith "Invalid command type for writePushPop"

            writeLines (["// " + (if commandType = C_PUSH then "push" else "pop") + " " + segment + " " + index.ToString()] @ pushPopLines)

        // Writes the assembly code of the bootstrap
        member this.writeInit() = 
            writeLines(["// bootstrap"; "@256"; "D=A"; "@SP"; "M=D"])
            // call Sys.init
            this.writeCall("Sys.init", 0)

        // Writes the assembly code of a given label
        member this.writeLabel(label:string) = 
            writeLine(sprintf "(%s.%s)" fileName label)

        // Writes the assembly code of a given goto command
        member this.writeGoto(label:string) = 
            let label_file = sprintf "%s.%s" fileName label 
            writeLines([
                        sprintf "// goto %s" label_file
                        sprintf "@%s" label_file
                        "0;JMP"
                        ])

        // Writes the assembly code of a given if-goto command
        member this.writeIf(label:string) = 
            let label_file = sprintf "%s.%s" fileName label 
            writeLines([
                sprintf "// if-goto %s" label_file
                "@SP"
                "AM=M-1"
                "D=M"
                sprintf "@%s" label_file
                "D;JNE"
            ])
        
        // Writes the assmebly code of a vm call command  
        member this.writeCall(functionName:string, numArgs:int) =
            let newARG = numArgs + 5
            let ra_label = sprintf "%s.RA%d" functionName retAddrCounter
            writeLines(
                ["// push return-address"; sprintf "@%s" ra_label; "D=A"] @ List.tail memoryPushInstructions @
                ["// push LCL"; "@LCL"] @ memoryPushInstructions @
                ["// push ARG"; "@ARG"] @ memoryPushInstructions @
                ["// push THIS"; "@THIS"] @ memoryPushInstructions @
                ["// push THAT"; "@THAT"] @ memoryPushInstructions @
                ["// ARG = SP-n-5"; "@SP"; "D=M"; sprintf "@%d" newARG; "D=D-A"; "@ARG"; "M=D"] @
                ["// LCL = SP"; "@SP"; "D=M"; "@LCL"; "M=D"] @
                [sprintf "// goto %s" functionName; sprintf "@%s" functionName; "0;JMP"] @
                ["// label return-address"; sprintf "(%s)" ra_label]
            )
            retAddrCounter <- retAddrCounter + 1

        // Writes the assembly code of a vm return command
        member this.writeReturn() =
            writeLines([
                "// return"
                "// FRAME = LCL"; "@LCL"; "D=M";
                "// RET = *(FRAME-5)"; "// RAM[13] = (LOCAL - 5)"; "@5"; "A=D-A"; "D=M"; "@13"; "M=D";
                "// *ARG = pop()"; "@SP"; "M=M-1"; "A=M"; "D=M"; "@ARG"; "A=M"; "M=D";
                "// SP = ARG+1"; "@ARG"; "D=M"; "@SP"; "M=D+1";
                "// THAT = *(FRAM-1)"; "@LCL"; "AM=M-1"; "D=M"; "@THAT"; "M=D";
                "// THIS = *(FRAM-2)"; "@LCL"; "AM=M-1"; "D=M"; "@THIS"; "M=D";
                "// ARG = *(FRAM-3)"; "@LCL"; "AM=M-1"; "D=M"; "@ARG"; "M=D";
                "// LCL = *(FRAM-4)"; "@LCL"; "AM=M-1"; "D=M"; "@LCL"; "M=D";
                "// goto RET"; "@13"; "A=M"; "0;JMP"
            ])

        // Writes the assembly code of a vm function command
        member this.writeFunction(functionName:string, numLocals:int) =
            writeLine(sprintf "// function %s %d" functionName numLocals)
            writeLine(sprintf "(%s)" functionName)
            // initialize locals with zero
            writeLines([sprintf "@%d" numLocals; "D=A"; sprintf "@%s.End" functionName; "D;JEQ"])
            writeLine(sprintf "(%s.Loop)" functionName)
            writeLines(["@SP"; "A=M"; "M=0"; "@SP"; "M=M+1"; sprintf "@%s.Loop" functionName; "D=D-1;JNE"])
            writeLine(sprintf "(%s.End)" functionName)

        // Sets the current file name (used for static variables and labels)
        member this.setFileName(name: string) =
            fileName <- name

        // Closes the output file stream
        member this.close() =
            fileStream.Close()
