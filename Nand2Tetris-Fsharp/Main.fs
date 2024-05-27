open System
open System.IO
open VMTranslator.Parser
open VMTranslator.CodeWriter
open VMTranslator.CommandType

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: VMTranslator.exe <input file/directory>"
        Environment.Exit 1

    let path = argv[0]
    let files = 
        if Directory.Exists(path) then
            Directory.GetFiles(path, "*.vm")
        else if File.Exists(path) && path.EndsWith(".vm") then
            [| path |]
        else
            printfn "Invalid file or directory"
            Environment.Exit 1
            [||]

    let outputFile =
        if Directory.Exists(path) then
            let directoryName = Path.GetFileName(path)
            Path.Combine(path, directoryName + ".asm")
        else
            Path.ChangeExtension(path, ".asm")

    let codeWriter = new CodeWriter(outputFile)
        
    files |> Array.iter (fun file ->
        let parser = new Parser(file)

        while parser.hasMoreLines() do
            parser.advance()
            match parser.commandType() with
            | Some C_ARITHMETIC -> 
                codeWriter.writeArithmetic(parser.arg1())
            | Some C_PUSH -> 
                codeWriter.writePushPop(C_PUSH, parser.arg1(), parser.arg2())
            | Some C_POP -> 
                codeWriter.writePushPop(C_POP, parser.arg1(), parser.arg2())
            | _ -> ()
        parser.close()
    )
        
    codeWriter.close()
    printfn "Translation complete: %s" outputFile
    0
