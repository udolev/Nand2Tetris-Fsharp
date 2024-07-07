// Exercise 2 - Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace VMTranslator

open System
open System.IO
open VMTranslator.Parser
open VMTranslator.CodeWriter
open VMTranslator.CommandType

type pathType = FILE | DIR

module Translator =
    // The main function gets a path to a directory/vm file and translates all of the vm files in it using the Parser and CodeWriter modules
    [<EntryPoint>]
    let main argv =
        if argv.Length <> 1 then
            printfn "Usage: VMTranslator.exe <input file/directory>"
            Environment.Exit 1

        let path = argv[0]

        let path_type = 
            if Directory.Exists(path) then
                Some DIR
            elif File.Exists(path) then
                Some FILE
            else
                printfn "Invalid path: not a file or directory"
                Environment.Exit 1
                None
            
        let files, outputFile = 
            match path_type with
            | Some DIR -> 
                Directory.GetFiles(path, "*.vm"), Path.Combine(path, Path.GetFileName(path) + ".asm")
            | Some FILE -> 
                [|path|], Path.ChangeExtension(path, ".asm")

        if Array.isEmpty files then
                    printfn "Invalid directory: no vm files to translate"
                    Environment.Exit 1

        let codeWriter = new CodeWriter(outputFile)

        if path_type = Some DIR then
            codeWriter.writeInit()
        
        files |> Array.iter (fun file ->
            let parser = new Parser(file)
            codeWriter.setFileName(Path.GetFileNameWithoutExtension(file))
            while parser.hasMoreLines() do
                parser.advance()
                match parser.commandType() with
                | Some C_ARITHMETIC -> 
                    codeWriter.writeArithmetic(parser.arg1())
                | Some C_PUSH -> 
                    codeWriter.writePushPop(C_PUSH, parser.arg1(), parser.arg2())
                | Some C_POP -> 
                    codeWriter.writePushPop(C_POP, parser.arg1(), parser.arg2())
                | Some C_LABEL ->
                    codeWriter.writeLabel(parser.arg1())
                | Some C_GOTO ->
                    codeWriter.writeGoto(parser.arg1())
                | Some C_IF ->
                    codeWriter.writeIf(parser.arg1())
                | Some C_FUNCTION ->
                    codeWriter.writeFunction(parser.arg1(), parser.arg2())
                | Some C_RETURN ->
                    codeWriter.writeReturn()
                | Some C_CALL ->
                    codeWriter.writeCall(parser.arg1(), parser.arg2())
                | _ -> ()
            parser.close()
        )
        
        codeWriter.close()
        printfn "Translation complete: %s" outputFile
        0
