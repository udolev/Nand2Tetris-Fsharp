// Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace JackCompiler.SyntaxAnalyzer

open System.IO
open JackTokenizer
open CompilationEngine
open XMLHelper

module JackAnalyzer =

    let tokenizeFile (inputFilePath: string) = 
        let outputFilePath = Path.Combine(Path.GetDirectoryName(inputFilePath), Path.GetFileNameWithoutExtension(inputFilePath) + "T.xml")
        let tokenizer = new JackTokenizer(inputFilePath)
        // Create a StreamWriter to write to the output file, ensuring it is properly disposed of after use
        use writer = new StreamWriter(outputFilePath)
        writer.WriteLine("<tokens>")
        while tokenizer.hasMoreTokens() do
            tokenizer.advance()
            writeTokenToXml writer tokenizer (Some 0)
        writer.WriteLine("</tokens>")
        printfn "Tokenization complete: %s" outputFilePath

    let compileFile (inputFilePath: string) = 
        let outputFilePath = Path.ChangeExtension(inputFilePath, "xml")
        let compilationEngine = CompilationEngine(inputFilePath, outputFilePath)
        compilationEngine.CompileClass()
        printfn "Compilation complete: %s" outputFilePath

    // Analyzes the specified Jack file
    let analyzeFile (inputFilePath: string) =
        tokenizeFile(inputFilePath)
        compileFile(inputFilePath)

    // Analyzes all .jack files in the specified directory
    let analyzeDirectory (directoryPath: string) =
        let files = Directory.GetFiles(directoryPath, "*.jack")
        files |> Array.iter analyzeFile

    // Entry point of the program
    [<EntryPoint>]
    let main argv =
        if argv.Length <> 1 then
            printfn "Usage: JackAnalyzer <input file/directory>"
            1
        else
            let inputPath = argv.[0]
            if Directory.Exists(inputPath) then
                analyzeDirectory inputPath
                0
            elif File.Exists(inputPath) then
                analyzeFile inputPath
                0
            else
                printfn "Invalid path: not a file or directory"
                1
