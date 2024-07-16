// Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace JackCompiler

open System.Text.RegularExpressions

module JackTokenizer =

    // Regular expression patterns for different token types and comments
    let COMMENT = @"(//.*)|(/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/)"
    let EMPTY_TEXT_PATTERN = @"\s*"
    let KEY_WORD_PATTERN = @"^\s*(class|constructor|function|method|static|field|var|int|char|boolean|void|true|false|null|this|let|do|if|else|while|return)\s*"
    let SYMBOL_PATTERN = @"^\s*([{}()\[\].,;+\-*/&|<>=~])\s*"
    let DIGIT_PATTERN = @"^\s*(\d+)\s*"
    let STRING_PATTERN = @"^\s*""([^""]*)""\s*"
    let IDENTIFIER_PATTERN = @"^\s*([a-zA-Z_][a-zA-Z1-9_]*)\s*"

    // Enumeration for token types
    type TokenType =
        | KEYWORD
        | SYMBOL
        | INT_CONST
        | STRING_CONST
        | IDENTIFIER

    // Enumeration for keyword types
    type Keyword =
        | CLASS | METHOD | FUNCTION | CONSTRUCTOR | INT
        | BOOLEAN | CHAR | VOID | VAR | STATIC | FIELD | LET
        | DO | IF | ELSE | WHILE | RETURN | TRUE | FALSE
        | NULL | THIS

    // This data type enables enumeration of Keyword type to string and vise versa
    type KeywordMap =
        // Map from string representation to Keyword value
        static member private fromString =
            [
                "class", CLASS; "method", METHOD; "function", FUNCTION; "constructor", CONSTRUCTOR;
                "int", INT; "boolean", BOOLEAN; "char", CHAR; "void", VOID; "var", VAR;
                "static", STATIC; "field", FIELD; "let", LET; "do", DO; "if", IF; "else", ELSE;
                "while", WHILE; "return", RETURN; "true", TRUE; "false", FALSE; "null", NULL; "this", THIS
            ] |> Map.ofList

        static member private toString =
            KeywordMap.fromString
            |> Map.toList
            |> List.map (fun (k, v) -> (v, k))
            |> Map.ofList

        static member getValue (key: string) : Keyword =
            KeywordMap.fromString.[key]

        static member getValue (key: Keyword) : string =
            KeywordMap.toString.[key]

    // JackTokenizer class
    type JackTokenizer(inputFilePath: string) =
        // Read the entire input file into a mutable string
        let mutable text = System.IO.File.ReadAllText(inputFilePath)
        let mutable _tokenType = None
        let mutable currentToken = None

        // Remove all comments from the input text
        let clearAllComments() =
            text <- Regex.Replace(text, COMMENT, "")

        // Clear comments upon initialization
        do
            clearAllComments()

        // Check if there are more tokens in the input
        member this.hasMoreTokens() =
            not (Regex.IsMatch(text, $"^{EMPTY_TEXT_PATTERN}$"))

        // Get the next token from the input and make it the current token
        member this.advance() =
            if this.hasMoreTokens() then
                let matchToken pattern tokenType =
                    let m = Regex.Match(text, pattern)
                    if m.Success then
                        text <- Regex.Replace(text, pattern, "")
                        _tokenType <- Some tokenType
                        currentToken <- Some m.Groups.[1].Value
                        true
                    else
                        false

                if matchToken KEY_WORD_PATTERN KEYWORD then ()
                elif matchToken SYMBOL_PATTERN SYMBOL then ()
                elif matchToken DIGIT_PATTERN INT_CONST then ()
                elif matchToken STRING_PATTERN STRING_CONST then ()
                elif matchToken IDENTIFIER_PATTERN IDENTIFIER then ()
                else
                    failwith "No matching token found"

        // Return the type of the current token
        member this.tokenType() =
            _tokenType.Value

        // Return the keyword which is the current token
        member this.keyword() =
            if _tokenType <> Some KEYWORD then
                failwith "keyword() should only be called when tokenType is KEYWORD"
            KeywordMap.getValue(currentToken.Value)
            

        // Return the symbol which is the current token
        member this.symbol() =
            if _tokenType <> Some SYMBOL then
                failwith "symbol() should only be called when tokenType is SYMBOL"
            currentToken.Value

        // Return the identifier which is the current token
        member this.identifier() =
            if _tokenType <> Some IDENTIFIER then
                failwith "identifier() should only be called when tokenType is IDENTIFIER"
            currentToken.Value

        // Return the integer value of the current token
        member this.intVal() =
            if _tokenType <> Some INT_CONST then
                failwith "intVal() should only be called when tokenType is INT_CONST"
            int currentToken.Value

        // Return the string value of the current token
        member this.stringVal() =
            if _tokenType <> Some STRING_CONST then
                failwith "stringVal() should only be called when tokenType is STRING_CONST"
            currentToken.Value
