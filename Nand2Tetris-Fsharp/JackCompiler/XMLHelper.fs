// Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace JackCompiler

open System.IO
open JackTokenizer

module XMLHelper =

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
        | TokenType.KEYWORD -> "keyword"
        | TokenType.SYMBOL -> "symbol"
        | TokenType.INT_CONST -> "integerConstant"
        | TokenType.STRING_CONST -> "stringConstant"
        | TokenType.IDENTIFIER -> "identifier"

    // Function to get token value based on the type
    let getTokenValue (tokenType:TokenType) (tokenizer: JackTokenizer) =
        match tokenType with
            | TokenType.KEYWORD -> KeywordMap.getValue (tokenizer.keyword())
            | TokenType.SYMBOL -> tokenizer.symbol().ToString()
            | TokenType.INT_CONST -> tokenizer.intVal().ToString()
            | TokenType.STRING_CONST -> tokenizer.stringVal()
            | TokenType.IDENTIFIER -> tokenizer.identifier()

    let writeLine (writer:StreamWriter) (line: string) (indentation:int) =
        writer.WriteLine(String.replicate indentation "  " + line)

    // Function to write the token to XML
    let writeTokenToXml (writer: StreamWriter) (tokenizer: JackTokenizer) (indentation:int option) =
        let tokenType = tokenizer.tokenType()
        let tokenTypeStr = tokenTypeToString tokenType
        let mutable tokenValue = getTokenValue tokenType tokenizer
        if tokenType = TokenType.SYMBOL then
            tokenValue <- escapeXml tokenValue
        let indent = Option.defaultValue 0 indentation
        writeLine writer (sprintf "<%s> %s </%s>" tokenTypeStr tokenValue tokenTypeStr) indent
