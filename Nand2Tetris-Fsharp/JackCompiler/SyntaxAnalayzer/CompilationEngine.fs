// Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace JackCompiler.SyntaxAnalyzer

open System.IO
open JackTokenizer
open XMLHelper

module CompilationEngine =

    type CompilationEngine(inputFilePath: string, outputPath: string) =
        let mutable indentation:int = 0
        let tokenizer = JackTokenizer(inputFilePath)
        let output = new StreamWriter(outputPath)

        // Writes a line to the xml considering the number of we're currently at
        let engineWriteLine (line: string) =
            writeLine output line indentation

        // Makes sure the current toekn matches what it is supposed to be and writes it to the xml file
        let process_token (expectedType: TokenType) (expectedValue: string) =
            let currentType = tokenizer.tokenType()
            let currentValue = getTokenValue currentType tokenizer
                
            if currentType = expectedType && currentValue = expectedValue then
                writeTokenToXml output tokenizer (Some indentation)
                tokenizer.advance()
            else
                printfn "Syntax error: Expected %s (%A) but got %s (%A)" expectedValue expectedType currentValue currentType
                tokenizer.advance()

        // Checks if the current keyword is one of the keywords in keywordsList
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

        // Helper compile function
        // Compiles the rule: type varName (',' varName)* ';'
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
        
        // Compiles a subroutine body
        // Deduction rule: '{' varDec* statements '}'
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

        // Compiles a complete subroutine
        // Deduction rule: ('constructor'|'function'|'method') ('void'|type) subroutineName '(' parameterList ')' subroutineBody
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

        // Compiles a parameter list
        // Deduction rule: ((type varName) (',' type varName)*)?
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

        // Compiles a variable declaration
        // Deduction rule: 'var' type varName (',' varName)* ';'
        member this.CompileVarDec() =
            engineWriteLine "<varDec>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD "var"

            this.CompileTypeAndVarName() // type varName (',' varName)* ';'

            indentation <- indentation - 1
            engineWriteLine "</varDec>"

        // Compiles a sequence of statements
        // Deduction rule: statement*
        member this.CompileStatements() =
            engineWriteLine "<statements>"
            indentation <- indentation + 1

            while tokenizer.tokenType() = TokenType.KEYWORD && 
                  checkForSpecificKeywords([Keyword.LET;Keyword.IF;Keyword.WHILE;Keyword.DO;Keyword.RETURN]) do
                // statement: letStatement | ifStatement | whileStatement | doStatement | returnStatement
                match tokenizer.keyword() with
                | Keyword.LET -> this.CompileLet()
                | Keyword.IF -> this.CompileIf()
                | Keyword.WHILE -> this.CompileWhile()
                | Keyword.DO -> this.CompileDo()
                | Keyword.RETURN -> this.CompileReturn()
                | _ -> ()

            indentation <- indentation - 1
            engineWriteLine "</statements>"

        // Compiles a do statement
        // Deduction rule: 'do' subroutineCall ';'
        member this.CompileDo() =
            engineWriteLine "<doStatement>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD "do"
            process_token TokenType.IDENTIFIER (tokenizer.identifier())
            this.CompileSubroutineCall()
            process_token TokenType.SYMBOL ";"

            indentation <- indentation - 1
            engineWriteLine "</doStatement>"

        // Compiles a let statement
        // Deduction rule: 'let' varName ('[' expression ']')? '=' expression ';'
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

        // Compiles a while statement
        // Deduction rule: 'while' '(' expression ')' '{' statements '}'
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

        // Compiles a return statement
        // Deduction rule: 'return' expression? ';'
        member this.CompileReturn() =
            engineWriteLine "<returnStatement>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD "return"

            if tokenizer.tokenType() <> TokenType.SYMBOL || tokenizer.symbol() <> ";" then
                this.CompileExpression()

            process_token TokenType.SYMBOL ";"

            indentation <- indentation - 1
            engineWriteLine "</returnStatement>"

        // Compiles an if statement, possibly with a trailing else clause
        // Deduction rule: 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
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

        // Compiles an expression
        // Deduction rule: term (op term)*
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

        // Compiles a term
        // Deduction rule: integerConstant | stringConstant | keywordConstant | varName | varName '[' expression ']' | subroutineCall | '(' expression ')' | unaryOp term
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

        // Compiles a (possibly empty) comma-separated list of expressions
        // Deduction rule: (expression (',' expression)* )?
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

        // Compiles a subroutine call
        // Deduction rule: subroutineName '(' expressionList ')' | (className | varName) '.' subroutineName '(' expressionList ')'
        member this.CompileSubroutineCall() =
            // process_token TokenType.IDENTIFIER (tokenizer.identifier()) // subroutineName or (className | varName), activated before calling the function
            if tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "." then
                process_token TokenType.SYMBOL "."
                process_token TokenType.IDENTIFIER (tokenizer.identifier()) // subroutineName

            process_token TokenType.SYMBOL "("
            this.CompileExpressionList()
            process_token TokenType.SYMBOL ")"
