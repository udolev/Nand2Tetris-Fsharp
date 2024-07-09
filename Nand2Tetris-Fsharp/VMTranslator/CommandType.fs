// Uriel Dolev 215676560 and Shilo Sofir 328308002
// Course Group ID: 150060.01.5784.46

namespace VMTranslator

module CommandType =

    type CommandType = 
        | C_ARITHMETIC
        | C_PUSH
        | C_POP
        | C_LABEL
        | C_GOTO
        | C_IF
        | C_FUNCTION
        | C_RETURN
        | C_CALL