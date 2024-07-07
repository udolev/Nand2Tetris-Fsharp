namespace JackCompiler.SyntaxAnalyzer

open System
open System.IO
open JackCompiler.SyntaxAnalyzer.JackTokenizer

module JackAnalyzer =

    // Function to escape XML special characters
    let escapeXml (str: string) =
        str.Replace("&", "&amp;")
           .Replace("<", "&lt;")
           .Replace(">", "&gt;")
           .Replace("\"", "&quot;")
           .Replace("'", "&apos;")

    // Function to convert token type to string
    let tokenTypeToString tokenType =
        match tokenType with
        | JackTokenizer.TokenType.KEYWORD -> "keyword"
        | JackTokenizer.TokenType.SYMBOL -> "symbol"
        | JackTokenizer.TokenType.INT_CONST -> "integerConstant"
        | JackTokenizer.TokenType.STRING_CONST -> "stringConstant"
        | JackTokenizer.TokenType.IDENTIFIER -> "identifier"

    // Function to convert keyword to string
    let keywordToString keyword =
        match keyword with
        | JackTokenizer.Keyword.CLASS -> "class"
        | JackTokenizer.Keyword.METHOD -> "method"
        | JackTokenizer.Keyword.FUNCTION -> "function"
        | JackTokenizer.Keyword.CONSTRUCTOR -> "constructor"
        | JackTokenizer.Keyword.INT -> "int"
        | JackTokenizer.Keyword.BOOLEAN -> "boolean"
        | JackTokenizer.Keyword.CHAR -> "char"
        | JackTokenizer.Keyword.VOID -> "void"
        | JackTokenizer.Keyword.VAR -> "var"
        | JackTokenizer.Keyword.STATIC -> "static"
        | JackTokenizer.Keyword.FIELD -> "field"
        | JackTokenizer.Keyword.LET -> "let"
        | JackTokenizer.Keyword.DO -> "do"
        | JackTokenizer.Keyword.IF -> "if"
        | JackTokenizer.Keyword.ELSE -> "else"
        | JackTokenizer.Keyword.WHILE -> "while"
        | JackTokenizer.Keyword.RETURN -> "return"
        | JackTokenizer.Keyword.TRUE -> "true"
        | JackTokenizer.Keyword.FALSE -> "false"
        | JackTokenizer.Keyword.NULL -> "null"
        | JackTokenizer.Keyword.THIS -> "this"

    // Function to write the token to XML
    let writeTokenToXml (writer: StreamWriter) (tokenizer: JackTokenizer) =
        let tokenType = tokenizer.tokenType()
        let tokenTypeStr = tokenTypeToString tokenType
        let tokenValue =
            match tokenType with
            | JackTokenizer.TokenType.KEYWORD -> keywordToString (tokenizer.keyword())
            | JackTokenizer.TokenType.SYMBOL -> escapeXml (tokenizer.symbol().ToString())
            | JackTokenizer.TokenType.INT_CONST -> tokenizer.intVal().ToString()
            | JackTokenizer.TokenType.STRING_CONST -> tokenizer.stringVal()
            | JackTokenizer.TokenType.IDENTIFIER -> tokenizer.identifier()

        writer.WriteLine(sprintf "<%s> %s </%s>" tokenTypeStr tokenValue tokenTypeStr)

    // Main function to analyze the Jack file
    let analyzeFile (inputFilePath: string) =
        let outputFilePath = Path.Combine(Path.GetDirectoryName(inputFilePath), Path.GetFileNameWithoutExtension(inputFilePath) + "T.xml")
        let tokenizer = new JackTokenizer.JackTokenizer(inputFilePath)
        use writer = new StreamWriter(outputFilePath)

        writer.WriteLine("<tokens>")
        while tokenizer.hasMoreTokens() do
            tokenizer.advance()
            writeTokenToXml writer tokenizer
        writer.WriteLine("</tokens>")
        writer.Close()
        printfn "Tokenization complete: %s" outputFilePath

    // Function to analyze all .jack files in a directory
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
