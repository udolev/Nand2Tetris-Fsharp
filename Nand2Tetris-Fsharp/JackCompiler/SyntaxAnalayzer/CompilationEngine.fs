namespace JackCompiler.SyntaxAnalyzer

open System.IO
open JackTokenizer
open XMLHelper

module CompilationEngine =

    type CompilationEngine(inputFilePath: string, outputPath: string) =
        let mutable indentation:int = 0
        let tokenizer = JackTokenizer(inputFilePath)
        let output = new StreamWriter(outputPath)

        let engineWriteLine (line: string) =
            writeLine output line indentation

        let process_token (expectedType: TokenType) (expectedValue: string) =
            let currentType = tokenizer.tokenType()
            let currentValue = getTokenValue currentType tokenizer
                
            if currentType = expectedType && currentValue = expectedValue then
                writeTokenToXml output tokenizer (Some indentation)
                tokenizer.advance()
            else
                printfn "Syntax error: Expected %s (%A) but got %s (%A)" expectedValue expectedType currentValue currentType
                tokenizer.advance()

        let checkForSpecificKeywords(keywordsList) =
            List.contains (tokenizer.keyword()) keywordsList

        // Compiles a complete class
        // Deduction rule: 'class' className '{' classVarDec* subroutingDec* '}'
        member this.CompileClass() =
            if tokenizer.hasMoreTokens() then
                tokenizer.advance() // get first token
                engineWriteLine "<class>"
                indentation <- indentation + 1

                process_token TokenType.KEYWORD "class"

                process_token TokenType.IDENTIFIER (tokenizer.identifier()) // className

                process_token TokenType.SYMBOL "{"

                while tokenizer.tokenType() = TokenType.KEYWORD && checkForSpecificKeywords([Keyword.STATIC;Keyword.FIELD]) do
                    this.CompileClassVarDec()

                while tokenizer.tokenType() = TokenType.KEYWORD && checkForSpecificKeywords([Keyword.CONSTRUCTOR;Keyword.FUNCTION;Keyword.METHOD]) do
                    this.CompileSubroutine()

                process_token TokenType.SYMBOL "}"

                indentation <- indentation - 1
                engineWriteLine "</class>"
                output.Close()

        // Compiles a static variable declaration, or a field declaration
        // Deduction rule: ('static'|'field') type varName (',' varName)* ';'
        member this.CompileClassVarDec() =
            engineWriteLine "<classVarDec>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD (KeywordMap.getValue (tokenizer.keyword())) // 'static' or 'field'

            this.CompileTypeAndVarName()

            indentation <- indentation - 1
            engineWriteLine "</classVarDec>"

        // Compiles the rule: type varName
        member this.CompileTypeAndVarName() =
            match tokenizer.tokenType() with
            | TokenType.KEYWORD -> process_token TokenType.KEYWORD (KeywordMap.getValue (tokenizer.keyword()))
            | TokenType.IDENTIFIER -> process_token TokenType.IDENTIFIER (tokenizer.identifier())
            | _ -> ()

            process_token TokenType.IDENTIFIER (tokenizer.identifier()) // varName

            while tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "," do
                process_token TokenType.SYMBOL "," // ','
                process_token TokenType.IDENTIFIER (tokenizer.identifier()) // varName
            process_token TokenType.SYMBOL ";" // ';'
        

        member this.CompileSubroutineBody() = 
            engineWriteLine "<subroutineBody>"
            indentation <- indentation + 1
            process_token TokenType.SYMBOL "{"

            while tokenizer.tokenType() = TokenType.KEYWORD && tokenizer.keyword() = Keyword.VAR do
                this.CompileVarDec()

            this.CompileStatements()

            process_token TokenType.SYMBOL "}"
            indentation <- indentation - 1
            engineWriteLine "</subroutineBody>"

        member this.CompileSubroutine() =
            engineWriteLine "<subroutineDec>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD (KeywordMap.getValue (tokenizer.keyword())) // 'constructor', 'function', or 'method'

            match tokenizer.tokenType() with
            | TokenType.KEYWORD -> process_token TokenType.KEYWORD (KeywordMap.getValue (tokenizer.keyword())) // 'void' or type
            | TokenType.IDENTIFIER -> process_token TokenType.IDENTIFIER (tokenizer.identifier()) // className
            | _ -> ()

            process_token TokenType.IDENTIFIER (tokenizer.identifier()) // subroutineName
            process_token TokenType.SYMBOL "("

            this.CompileParameterList()

            process_token TokenType.SYMBOL ")"

            this.CompileSubroutineBody()

            indentation <- indentation - 1
            engineWriteLine "</subroutineDec>"

        member this.CompileParameterList() =
            engineWriteLine "<parameterList>"
            indentation <- indentation + 1

            if tokenizer.tokenType() = TokenType.KEYWORD ||  tokenizer.tokenType() = TokenType.IDENTIFIER then
                match tokenizer.tokenType() with
                | TokenType.KEYWORD -> process_token TokenType.KEYWORD (KeywordMap.getValue (tokenizer.keyword()))
                | TokenType.IDENTIFIER -> process_token TokenType.IDENTIFIER (tokenizer.identifier())

                process_token TokenType.IDENTIFIER (tokenizer.identifier()) // varName

                while tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "," do
                    process_token TokenType.SYMBOL "," // ','

                    match tokenizer.tokenType() with
                    | TokenType.KEYWORD -> process_token TokenType.KEYWORD (KeywordMap.getValue (tokenizer.keyword()))
                    | TokenType.IDENTIFIER -> process_token TokenType.IDENTIFIER (tokenizer.identifier())
                    | _ -> ()

                    process_token TokenType.IDENTIFIER (tokenizer.identifier()) // varName

            indentation <- indentation - 1
            engineWriteLine "</parameterList>"

        member this.CompileVarDec() =
            engineWriteLine "<varDec>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD "var"

            this.CompileTypeAndVarName()

            indentation <- indentation - 1
            engineWriteLine "</varDec>"

        member this.CompileStatements() =
            engineWriteLine "<statements>"
            indentation <- indentation + 1

            while tokenizer.tokenType() = TokenType.KEYWORD && 
                  checkForSpecificKeywords([Keyword.LET;Keyword.IF;Keyword.WHILE;Keyword.DO;Keyword.RETURN]) do
                // statement*
                match tokenizer.keyword() with
                | Keyword.LET -> this.CompileLet()
                | Keyword.IF -> this.CompileIf()
                | Keyword.WHILE -> this.CompileWhile()
                | Keyword.DO -> this.CompileDo()
                | Keyword.RETURN -> this.CompileReturn()

            indentation <- indentation - 1
            engineWriteLine "</statements>"

        member this.CompileDo() =
            engineWriteLine "<doStatement>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD "do"
            process_token TokenType.IDENTIFIER (tokenizer.identifier())
            this.CompileSubroutineCall()
            process_token TokenType.SYMBOL ";"

            indentation <- indentation - 1
            engineWriteLine "</doStatement>"

        member this.CompileLet() =
            engineWriteLine "<letStatement>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD "let"
            process_token TokenType.IDENTIFIER (tokenizer.identifier()) // varName

            if tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "[" then
                process_token TokenType.SYMBOL "[" // '['
                this.CompileExpression()
                process_token TokenType.SYMBOL "]"

            process_token TokenType.SYMBOL "="
            this.CompileExpression()
            process_token TokenType.SYMBOL ";"

            indentation <- indentation - 1
            engineWriteLine "</letStatement>"

        member this.CompileWhile() =
            engineWriteLine "<whileStatement>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD "while"
            process_token TokenType.SYMBOL "("
            this.CompileExpression()
            process_token TokenType.SYMBOL ")"
            process_token TokenType.SYMBOL "{"
            this.CompileStatements()
            process_token TokenType.SYMBOL "}"

            indentation <- indentation - 1
            engineWriteLine "</whileStatement>"

        member this.CompileReturn() =
            engineWriteLine "<returnStatement>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD "return"

            if tokenizer.tokenType() <> TokenType.SYMBOL || tokenizer.symbol() <> ";" then
                this.CompileExpression()

            process_token TokenType.SYMBOL ";"

            indentation <- indentation - 1
            engineWriteLine "</returnStatement>"

        member this.CompileIf() =
            engineWriteLine "<ifStatement>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD "if"
            process_token TokenType.SYMBOL "("
            this.CompileExpression()
            process_token TokenType.SYMBOL ")"
            process_token TokenType.SYMBOL "{"
            this.CompileStatements()
            process_token TokenType.SYMBOL "}"

            if tokenizer.tokenType() = TokenType.KEYWORD && tokenizer.keyword() = Keyword.ELSE then
                process_token TokenType.KEYWORD "else"
                process_token TokenType.SYMBOL "{"
                this.CompileStatements()
                process_token TokenType.SYMBOL "}"

            indentation <- indentation - 1
            engineWriteLine "</ifStatement>"

        member this.CompileExpression() =
            engineWriteLine "<expression>"
            indentation <- indentation + 1

            this.CompileTerm()

            while tokenizer.tokenType() = TokenType.SYMBOL && 
                  (tokenizer.symbol() = "+" || tokenizer.symbol() = "-" || 
                   tokenizer.symbol() = "*" || tokenizer.symbol() = "/" || 
                   tokenizer.symbol() = "&" || tokenizer.symbol() = "|" || 
                   tokenizer.symbol() = "<" || tokenizer.symbol() = ">" || 
                   tokenizer.symbol() = "=") do
                process_token TokenType.SYMBOL (tokenizer.symbol().ToString())
                this.CompileTerm()

            indentation <- indentation - 1
            engineWriteLine "</expression>"

        member this.CompileTerm() =
            engineWriteLine "<term>"
            indentation <- indentation + 1

            match tokenizer.tokenType() with
            | TokenType.INT_CONST -> process_token TokenType.INT_CONST (tokenizer.intVal().ToString())
            | TokenType.STRING_CONST -> process_token TokenType.STRING_CONST (tokenizer.stringVal())
            | TokenType.KEYWORD -> process_token TokenType.KEYWORD (KeywordMap.getValue (tokenizer.keyword()))
            | TokenType.IDENTIFIER ->
                process_token TokenType.IDENTIFIER (tokenizer.identifier())

                if tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "[" then
                    process_token TokenType.SYMBOL "["
                    this.CompileExpression()
                    process_token TokenType.SYMBOL "]"
                elif tokenizer.tokenType() = TokenType.SYMBOL && (tokenizer.symbol() = "(" || tokenizer.symbol() = ".") then
                    this.CompileSubroutineCall()                    
            | TokenType.SYMBOL ->
                let symbol = tokenizer.symbol()
                if symbol = "(" then
                    process_token TokenType.SYMBOL "("
                    this.CompileExpression()
                    process_token TokenType.SYMBOL ")"
                elif symbol = "-" || symbol = "~" then
                    process_token TokenType.SYMBOL (symbol.ToString())
                    this.CompileTerm()

            indentation <- indentation - 1
            engineWriteLine "</term>"

        member this.CompileExpressionList() =
            engineWriteLine "<expressionList>"
            indentation <- indentation + 1

            if tokenizer.tokenType() <> TokenType.SYMBOL || tokenizer.symbol() <> ")" then
                this.CompileExpression()
                while tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "," do
                    process_token TokenType.SYMBOL ","
                    this.CompileExpression()

            indentation <- indentation - 1
            engineWriteLine "</expressionList>"

        member this.CompileSubroutineCall() =
            // process_token TokenType.IDENTIFIER (tokenizer.identifier()) // subroutineName or (className | varName), activated before calling the function

            if tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "." then
                process_token TokenType.SYMBOL "."
                process_token TokenType.IDENTIFIER (tokenizer.identifier()) // subroutineName

            process_token TokenType.SYMBOL "("
            this.CompileExpressionList()
            process_token TokenType.SYMBOL ")"