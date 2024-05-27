// Exercise 1 - Uriel Dolev 215676560 and Shilo Sofir 328308002

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