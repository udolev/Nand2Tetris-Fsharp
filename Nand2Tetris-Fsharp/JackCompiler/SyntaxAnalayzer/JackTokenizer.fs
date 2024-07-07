namespace JackCompiler.SyntaxAnalyzer

open System
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
            match currentToken with
            | Some token -> 
                match token.ToUpper() with
                | "CLASS" -> CLASS
                | "METHOD" -> METHOD
                | "FUNCTION" -> FUNCTION
                | "CONSTRUCTOR" -> CONSTRUCTOR
                | "INT" -> INT
                | "BOOLEAN" -> BOOLEAN
                | "CHAR" -> CHAR
                | "VOID" -> VOID
                | "VAR" -> VAR
                | "STATIC" -> STATIC
                | "FIELD" -> FIELD
                | "LET" -> LET
                | "DO" -> DO
                | "IF" -> IF
                | "ELSE" -> ELSE
                | "WHILE" -> WHILE
                | "RETURN" -> RETURN
                | "TRUE" -> TRUE
                | "FALSE" -> FALSE
                | "NULL" -> NULL
                | "THIS" -> THIS
                | _ -> failwith "Invalid keyword"
            | None -> failwith "No current token"

        // Return the symbol which is the current token
        member this.symbol() =
            currentToken.Value

        // Return the identifier which is the current token
        member this.identifier() =
            currentToken.Value

        // Return the integer value of the current token
        member this.intVal() =
            int currentToken.Value

        // Return the string value of the current token
        member this.stringVal() =
            currentToken.Value
