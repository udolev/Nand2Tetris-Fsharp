// Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace JackCompiler

open System.IO
open JackTokenizer
open XMLHelper
open VMWriter
open SymbolTable

module CompilationEngine =

    type CompilationEngine(inputFilePath: string, xmlOutputPath: string, vmOutputPath: string) =
        let tokenizer = JackTokenizer(inputFilePath)
        let mutable indentation:int = 0
        let xmlWriter = new StreamWriter(xmlOutputPath)

        let vmOutputStream = new StreamWriter(vmOutputPath)
        let vmWriter = VMWriter(vmOutputStream)
        let symbolTable = SymbolTable()
        let mutable className = ""
        let mutable whileIndex = 0
        let mutable ifIndex = 0

       
        // Writes a line to the xml considering the number of we're currently at
        let engineWriteLine (line: string) =
            writeLine xmlWriter line indentation

        // Makes sure the current toekn matches what it is supposed to be and writes it to the xml file
        let process_token (expectedType: TokenType) (expectedValue: string) =
            let currentType = tokenizer.tokenType()
            let currentValue = getTokenValue currentType tokenizer
                
            if currentType = expectedType && currentValue = expectedValue then
                writeTokenToXml xmlWriter tokenizer (Some indentation)
                tokenizer.advance()
            else
                printfn "Syntax error: Expected %s (%A) but got %s (%A)" expectedValue expectedType currentValue currentType
                tokenizer.advance()
        
        let process_parameter () =
            if tokenizer.tokenType() = TokenType.KEYWORD then
                let token_type = KeywordMap.getValue (tokenizer.keyword())
                process_token TokenType.KEYWORD token_type
                let varName = tokenizer.identifier()
                symbolTable.define(varName, token_type, Kind.Arg)
                process_token TokenType.IDENTIFIER varName
            else if tokenizer.tokenType() = TokenType.IDENTIFIER then
                let token_type = tokenizer.identifier()
                process_token TokenType.IDENTIFIER token_type
                let varName = tokenizer.identifier()
                symbolTable.define(varName, token_type, Kind.Arg)
                process_token TokenType.IDENTIFIER varName
            
        let writePush (name: string) =
            let kind = symbolTable.kindOf(name)
            let segment = 
                match kind with
                | Some Kind.Field -> Segment.This
                | Some Kind.Var -> Segment.Local
                | Some Kind.Arg -> Segment.Argument
                | Some Kind.Static -> Segment.Static
                | None -> failwith (sprintf "symbol %s does not exist" name)
            vmWriter.writePush(segment, symbolTable.indexOf(name))
                                    
        let writePop(name: string) =
            let kind = symbolTable.kindOf(name)
            let segment = 
                match kind with
                | Some Kind.Field -> Segment.This
                | Some Kind.Var -> Segment.Local
                | Some Kind.Arg -> Segment.Argument
                | Some Kind.Static -> Segment.Static
                | None -> failwith (sprintf "symbol %s does not exist" name)
            vmWriter.writePop(segment, symbolTable.indexOf(name))

        let writeOperation(operator: string) =
            match operator with
            | "+" -> vmWriter.writeArithmetic(Add)
            | "-" -> vmWriter.writeArithmetic(Sub)
            | "*" -> vmWriter.writeCall("Math.multiply", 2)
            | "/" -> vmWriter.writeCall("Math.divide", 2)
            | "|" -> vmWriter.writeArithmetic(Or)
            | "&" -> vmWriter.writeArithmetic(And)
            | "=" -> vmWriter.writeArithmetic(Eq)
            | "<" -> vmWriter.writeArithmetic(Lt)
            | ">" -> vmWriter.writeArithmetic(Gt)

        let writeStringConstant(str: string) = 
            vmWriter.writePush(Constant, str.Length)
            vmWriter.writeCall("String.new", 1)
            for i in 0 .. (str.Length - 1) do
                vmWriter.writePush(Constant, int str.[i])
                vmWriter.writeCall("String.appendChar", 2)


        let writeKeyWordConstant(key: Keyword) = 
            match key with
                |Keyword.NULL -> vmWriter.writePush(Constant, 0)
                |Keyword.FALSE -> vmWriter.writePush(Constant, 0)
                |Keyword.TRUE ->
                    vmWriter.writePush(Constant, 0)
                    vmWriter.writeArithmetic(Not)
                |Keyword.THIS -> vmWriter.writePush(Pointer, 0)
                |_ -> ()
           

        let writeUnaryOperation(operator: string) =
            match operator with
                | "~" -> vmWriter.writeArithmetic(Not)
                | "-" -> vmWriter.writeArithmetic(Neg)

        // Checks if the current keyword is one of the keywords in keywordsList
        let checkForSpecificKeywords(keywordsList) =
            List.contains (tokenizer.keyword()) keywordsList

        // Compiles a complete class
        // Deduction rule: 'class' className '{' classVarDec* subroutingDec* '}'
        member this.CompileClass() =
            if tokenizer.hasMoreTokens() then
                tokenizer.advance() // get first token

                process_token TokenType.KEYWORD "class"

                className <- tokenizer.identifier()
                process_token TokenType.IDENTIFIER (className) // className

                process_token TokenType.SYMBOL "{"

                while tokenizer.tokenType() = TokenType.KEYWORD && checkForSpecificKeywords([Keyword.STATIC;Keyword.FIELD]) do
                    this.CompileClassVarDec()

                while tokenizer.tokenType() = TokenType.KEYWORD && checkForSpecificKeywords([Keyword.CONSTRUCTOR;Keyword.FUNCTION;Keyword.METHOD]) do
                    this.CompileSubroutine()

                process_token TokenType.SYMBOL "}"
                xmlWriter.Close()
                vmOutputStream.Close()

        // Compiles a static variable declaration, or a field declaration
        // Deduction rule: ('static'|'field') type varName (',' varName)* ';'
        member this.CompileClassVarDec() =
            let symbol_kind = KeywordMap.getValue (tokenizer.keyword()) // 'static' or 'field'
            process_token TokenType.KEYWORD (symbol_kind)

            this.CompileTypeAndVarName(get_kind symbol_kind)

        // Helper compile function
        // Compiles the rule: type varName (',' varName)* ';'
        member this.CompileTypeAndVarName(symbol_kind:Kind) =
            let token_type:TokenType = tokenizer.tokenType()
            let symbol_type:string = 
                match token_type with
                | TokenType.KEYWORD -> KeywordMap.getValue (tokenizer.keyword())
                | TokenType.IDENTIFIER -> tokenizer.identifier()
            process_token token_type symbol_type

            let mutable symbol_name = tokenizer.identifier()
            process_token TokenType.IDENTIFIER (symbol_name) // varName

            // Add symbol to the symbol table
            symbolTable.define(symbol_name, symbol_type, symbol_kind)

            while tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "," do
                process_token TokenType.SYMBOL "," // ','
                symbol_name <- tokenizer.identifier()
                process_token TokenType.IDENTIFIER (symbol_name) // varName
                // Add symbol to the symbol table
                symbolTable.define(symbol_name, symbol_type, symbol_kind)
            process_token TokenType.SYMBOL ";" // ';'
        
        // Compiles a subroutine body
        // Deduction rule: '{' varDec* statements '}'
        member this.CompileSubroutineBody(subroutineName:string, subroutineType:string) = 
            process_token TokenType.SYMBOL "{"

            while tokenizer.tokenType() = TokenType.KEYWORD && tokenizer.keyword() = Keyword.VAR do
                this.CompileVarDec()

            vmWriter.writeFunction(subroutineName, symbolTable.varCount(Var))
            // if subroutineType is constructor, construct the object
            if subroutineType = "constructor" then
                vmWriter.writePush(Constant, symbolTable.varCount(Field))
                vmWriter.writeCall("Memory.alloc", 1)
                vmWriter.writePop(Pointer, 0)
            elif subroutineType = "method" then
                vmWriter.writePush(Argument, 0)
                vmWriter.writePop(Pointer, 0)
            ifIndex <- 0
            this.CompileStatements()

            process_token TokenType.SYMBOL "}"

        // Compiles a complete subroutine
        // Deduction rule: ('constructor'|'function'|'method') ('void'|type) subroutineName '(' parameterList ')' subroutineBody
        member this.CompileSubroutine() =
            symbolTable.reset()
            let subroutineType = KeywordMap.getValue (tokenizer.keyword())
            if subroutineType = "method" then
                symbolTable.define("this", className, Kind.Arg)
            process_token TokenType.KEYWORD (subroutineType) // 'constructor', 'function', or 'method'

            match tokenizer.tokenType() with
            | TokenType.KEYWORD -> process_token TokenType.KEYWORD (KeywordMap.getValue (tokenizer.keyword())) // 'void' or type
            | TokenType.IDENTIFIER -> process_token TokenType.IDENTIFIER (tokenizer.identifier()) // className
            | _ -> ()

            let subroutineName = className + "." + tokenizer.identifier()
            process_token TokenType.IDENTIFIER (tokenizer.identifier()) // subroutineName
            process_token TokenType.SYMBOL "("

            this.CompileParameterList()

            process_token TokenType.SYMBOL ")"

            this.CompileSubroutineBody(subroutineName, subroutineType)


        // Compiles a parameter list
        // Deduction rule: ((type varName) (',' type varName)*)?
        member this.CompileParameterList() =
            process_parameter()
            while tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "," do
                    process_token TokenType.SYMBOL "," // ','
                    process_parameter()

        // Compiles a variable declaration
        // Deduction rule: 'var' type varName (',' varName)* ';'
        member this.CompileVarDec() =
            process_token TokenType.KEYWORD "var"
            this.CompileTypeAndVarName(Kind.Var) // type varName (',' varName)* ';'


        // Compiles a sequence of statements
        // Deduction rule: statement*
        member this.CompileStatements() =
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

        // Compiles a do statement
        // Deduction rule: 'do' subroutineCall ';'
        member this.CompileDo() =
            process_token TokenType.KEYWORD "do"
            let subroutineName = tokenizer.identifier()
            process_token TokenType.IDENTIFIER (subroutineName)
            this.CompileSubroutineCall(subroutineName)
            vmWriter.writePop(Temp, 0)
            process_token TokenType.SYMBOL ";"

        // Compiles a subroutine call
        // Deduction rule: subroutineName '(' expressionList ')' | (className | varName) '.' subroutineName '(' expressionList ')'
        member this.CompileSubroutineCall(baseName: string) =
            // process_token TokenType.IDENTIFIER (tokenizer.identifier()) // subroutineName or (className | varName), activated before calling the function
            let mutable fullSubRutineName = baseName
            let mutable numArgs = 0
            if tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "." then
                process_token TokenType.SYMBOL "."
                let functionName = tokenizer.identifier()
                fullSubRutineName <- fullSubRutineName + "." + functionName
                process_token TokenType.IDENTIFIER (functionName) // subroutineName
                
                if symbolTable.kindOf(baseName) <> None then // case obj.func()
                    writePush(baseName)
                    numArgs <- 1
                    fullSubRutineName <- symbolTable.typeOf(baseName) + "." + functionName
            else
                vmWriter.writePush(Pointer, 0)
                fullSubRutineName <- className + "." + fullSubRutineName
                numArgs <- 1


            process_token TokenType.SYMBOL "("
            numArgs <- numArgs + this.CompileExpressionList()
            process_token TokenType.SYMBOL ")"
            vmWriter.writeCall(fullSubRutineName, numArgs)


        // Compiles a let statement
        // Deduction rule: 'let' varName ('[' expression ']')? '=' expression ';'
        member this.CompileLet() =
            process_token TokenType.KEYWORD "let"
            let mutable array_check = false
            let token_name = tokenizer.identifier()
            process_token TokenType.IDENTIFIER (token_name) // varName

            if tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "[" then
                array_check <- true
                process_token TokenType.SYMBOL "[" // '['
                this.CompileExpression()
                process_token TokenType.SYMBOL "]"
                writePush(token_name)
                vmWriter.writeArithmetic(Add)
            process_token TokenType.SYMBOL "="
            this.CompileExpression()
            if array_check then
                vmWriter.writePop(Temp, 0)
                vmWriter.writePop(Pointer,1)
                vmWriter.writePush(Temp, 0)
                vmWriter.writePop(That, 0)
            else 
                writePop(token_name)
            process_token TokenType.SYMBOL ";"

        // Compiles a while statement
        // Deduction rule: 'while' '(' expression ')' '{' statements '}'
        member this.CompileWhile() =       
            process_token TokenType.KEYWORD "while"
            let strWhile = string whileIndex
            whileIndex <- whileIndex + 1
            vmWriter.writeLabel("WHILE_EXP" + strWhile)
            process_token TokenType.SYMBOL "("
            this.CompileExpression()
            vmWriter.writeArithmetic(Not)
            vmWriter.writeIf("WHILE_END" + strWhile)
            process_token TokenType.SYMBOL ")"
            process_token TokenType.SYMBOL "{"
            this.CompileStatements()
            vmWriter.writeGoto("WHILE_EXP" + strWhile)
            vmWriter.writeLabel("WHILE_END" + strWhile)
            process_token TokenType.SYMBOL "}"

        // Compiles an if statement, possibly with a trailing else clause
        // Deduction rule: 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
        member this.CompileIf() =
            engineWriteLine "<ifStatement>"
            indentation <- indentation + 1

            let strif = string ifIndex
            ifIndex <- ifIndex + 1
            process_token TokenType.KEYWORD "if"
            process_token TokenType.SYMBOL "("
            this.CompileExpression()
            process_token TokenType.SYMBOL ")"
            
            vmWriter.writeIf("IF_TRUE" + strif)
            vmWriter.writeGoto("IF_FALSE" + strif)
            vmWriter.writeLabel("IF_TRUE" + strif)
            process_token TokenType.SYMBOL "{"
            this.CompileStatements()
            process_token TokenType.SYMBOL "}"

            if tokenizer.tokenType() = TokenType.KEYWORD && tokenizer.keyword() = Keyword.ELSE then
                vmWriter.writeGoto("IF_END" + strif)
                vmWriter.writeLabel("IF_FALSE" + strif)
                process_token TokenType.KEYWORD "else"
                process_token TokenType.SYMBOL "{"
                this.CompileStatements()
                process_token TokenType.SYMBOL "}"
                vmWriter.writeLabel("IF_END" + strif)
            else
                vmWriter.writeLabel("IF_FALSE" + strif)

            indentation <- indentation - 1
            engineWriteLine "</ifStatement>"

        // Compiles a return statement
        // Deduction rule: 'return' expression? ';'
        member this.CompileReturn() =
            engineWriteLine "<returnStatement>"
            indentation <- indentation + 1

            process_token TokenType.KEYWORD "return"

            if tokenizer.tokenType() <> TokenType.SYMBOL || tokenizer.symbol() <> ";" then
                this.CompileExpression()
            else
                vmWriter.writePush(Constant, 0)
           
            process_token TokenType.SYMBOL ";"
            vmWriter.writeReturn()

            indentation <- indentation - 1
            engineWriteLine "</returnStatement>"

        // Compiles an expression
        // Deduction rule: term (op term)*
        member this.CompileExpression() =
            engineWriteLine "<expression>"
            indentation <- indentation + 1

            this.CompileTerm()

            let mutable operationSymbol = tokenizer.symbol()
            while tokenizer.tokenType() = TokenType.SYMBOL && 
                   (operationSymbol = "+" || operationSymbol = "-" || 
                   operationSymbol = "*" || operationSymbol = "/" || 
                   operationSymbol = "&" || operationSymbol = "|" || 
                   operationSymbol = "<" || operationSymbol = ">" || 
                   operationSymbol = "=") do
                process_token TokenType.SYMBOL operationSymbol
                this.CompileTerm()
                writeOperation operationSymbol
                operationSymbol <- tokenizer.symbol()

            indentation <- indentation - 1
            engineWriteLine "</expression>"

        // Compiles a term
        // Deduction rule: integerConstant | stringConstant | keywordConstant | varName | varName '[' expression ']' | subroutineCall | '(' expression ')' | unaryOp term
        member this.CompileTerm() =
            engineWriteLine "<term>"
            indentation <- indentation + 1

            match tokenizer.tokenType() with
            | TokenType.INT_CONST ->
                vmWriter.writePush(Constant, tokenizer.intVal())
                process_token TokenType.INT_CONST (tokenizer.intVal().ToString())
            | TokenType.STRING_CONST ->
                writeStringConstant(tokenizer.stringVal())
                process_token TokenType.STRING_CONST (tokenizer.stringVal())
            |   TokenType.KEYWORD -> 
                writeKeyWordConstant(tokenizer.keyword())
                process_token TokenType.KEYWORD (KeywordMap.getValue (tokenizer.keyword()))
            | TokenType.IDENTIFIER ->
                let name = tokenizer.identifier()
                process_token TokenType.IDENTIFIER (tokenizer.identifier())               
                if tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "[" then
                    writePush(name)
                    process_token TokenType.SYMBOL "["
                    this.CompileExpression()
                    process_token TokenType.SYMBOL "]"
                    vmWriter.writeArithmetic(Add)
                    vmWriter.writePop(Pointer, 1)
                    vmWriter.writePush(That, 0)
                elif tokenizer.tokenType() = TokenType.SYMBOL && (tokenizer.symbol() = "(" || tokenizer.symbol() = ".") then
                    this.CompileSubroutineCall(name)
                else
                    writePush(name)
            | TokenType.SYMBOL ->
                let symbol = tokenizer.symbol()
                if symbol = "(" then
                    process_token TokenType.SYMBOL "("
                    this.CompileExpression()
                    process_token TokenType.SYMBOL ")"
                elif symbol = "-" || symbol = "~" then
                    process_token TokenType.SYMBOL (symbol)
                    this.CompileTerm()
                    writeUnaryOperation(symbol)

            indentation <- indentation - 1
            engineWriteLine "</term>"

        // Compiles a (possibly empty) comma-separated list of expressions
        // Deduction rule: (expression (',' expression)* )?
        member this.CompileExpressionList() =
            engineWriteLine "<expressionList>"
            indentation <- indentation + 1
            let mutable numArgs = 0
            if tokenizer.tokenType() <> TokenType.SYMBOL || tokenizer.symbol() <> ")" then
                this.CompileExpression()
                numArgs <- numArgs + 1
                while tokenizer.tokenType() = TokenType.SYMBOL && tokenizer.symbol() = "," do
                    process_token TokenType.SYMBOL ","
                    this.CompileExpression()
                    numArgs <- numArgs + 1
            
            indentation <- indentation - 1
            engineWriteLine "</expressionList>"

            numArgs

