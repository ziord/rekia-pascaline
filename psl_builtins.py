"""
TokenType class and StandardTypes class for Pascaline tokens and standard types
"""

import enum
import math
from time import time

class TokenType(enum.Enum):
    #reserved keywords
    BEGIN = "begin"
    VAR = "var"
    PROGRAM = "PROGRAM"
    DIV = "div"
    COMM = "comm"
    INTEGER_CONST = "integer"
    FLOAT_CONST = "real"
    PROCEDURE = "procedure"
    INTEGER_DIV = "div"
    CHAR_CONST = "char"
    STRING_CONST = "string"
    PROC = "proc"
    FUNCION = "function"
    FUNC = "func"
    AND = "and"
    OR = "or"
    NOT = "not"
    IF = "if"
    THEN = "then"
    ELSE_IF = "else if"
    ELIF = "elif"
    ELSE = "else"
    END_IF = "endif"
    RET = "ret"
    WHILE = "while"
    FOR = "for"
    TO = "to"
    DO = "do"
    DOWNTO = "downto"
    BREAK = "break"
    CONTINUE = "continue"
    STOP = "stop" #alias for break
    REPEAT = "repeat"
    UNTIL = "until"
    BOOLEAN = "boolean"
    FROM = "from"
    TRUE = "true"
    FALSE = "false"
    PASS = "pass"
    SKIP = "skip" #alias for continue
    END = "end"

    #single character tokens
    PLUS = '+'
    MINUS = '-'
    MULT = '*'
    FLOAT_DIV = '/'
    COLON = ':'
    SEMI = ';'
    DOT = '.'
    LPAREN = '('
    RPAREN = ')'
    COMMA = ','
    MOD = '%'
    REF = '&'
    APOS = "'"
    DAPOS = "\""

    #others
    ID = "id"
    EOF = None
    FUNC_ID = "func_id"
    POST_PLUS_PLUS = "post_plus_plus"
    POST_MINUS_MINUS = "post_minus_minus"
    PRE_PLUS_PLUS = "pre_plus_plus"
    PRE_MINUS_MINUS = "pre_minus_minus"

    #Binary operators
    GT = ">"
    LT = "<"
    EQ = "="
    NE = "!="
    LE = "<="
    GE = ">="
    AD = "&&"

    #assignment operators
    ASSIGN = ":="
    PLUS_ASSIGN = "+:="
    INTEGER_DIV_ASSIGN = "//="
    MULT_ASSIGN = "mult_assign"
    FLOAT_DIV_ASSIGN = "/:="
    MINUS_ASSIGN = "-:="
    POWER_ASSIGN = "**:="
    MOD_ASSIGN = "%:="
    POWER = "**"

    CONTROL = "control"
    GENERATOR = "generator"

class StandardTypes(enum.Enum):
    INTEGER_CONST = "integer"
    FLOAT_CONST = "real"
    BOOLEAN = "boolean"
    STRING = "string"
    CHAR = "char"
    NULL = "null"

class StandardFunctions(enum.Enum):
    """
    Standard functions for Pascaline
    """
    def sqr(num):
        return num**2
    def succ(num):
        if type(num) is int:
            return num+1
        else:
            return 0
    #functions with integer return types
    TRUNC = "trunc"
    SUCC = "succ"
    #functions with real return types
    COS = "cos"
    SIN = "sin"
    CLOCK = "clock"
    TAN = "tan"
    #functions with integer/real return types
    SUM = "sum"
    SQ = "sqr"
    SQRT = "sqr"
    MAX = "max"
    MIN = "min"
    ROUND = "round"
    POW = "pow"
    

    #functions with null return types
    write = "write"
    writeln = "writeln"
    
def get_standard_func():
    lst = [func.value for func in list(StandardFunctions)]
    return lst

def get_standard_func_return_type(func_name):
    std_funcs = list(StandardFunctions)

    int_start_index = std_funcs.index(StandardFunctions.TRUNC)
    int_end_index = std_funcs.index(StandardFunctions.COS)

    real_start_index = std_funcs.index(StandardFunctions.COS)
    real_end_index = std_funcs.index(StandardFunctions.TAN)

    int_real_start_index = std_funcs.index(StandardFunctions.SUM)
    int_real_end_index = std_funcs.index(StandardFunctions.POW)

    null_start_ind = std_funcs.index(StandardFunctions.write)
    null_end_ind = std_funcs.index(StandardFunctions.writeln)

    int_ret_types = {func.value : f"{StandardTypes.INTEGER_CONST.value}" for func in std_funcs[int_start_index:int_end_index]}
    real_ret_types = {func.value : f"{StandardTypes.FLOAT_CONST.value}" for func in std_funcs[real_start_index:real_end_index+1]}
    int_real_ret_types = {func.value :f"{StandardTypes.INTEGER_CONST.value}|{StandardTypes.FLOAT_CONST.value}" \
        for func in std_funcs[int_real_start_index:int_real_end_index+1]}
    null_ret_types = {func.value : f"{StandardTypes.NULL.value}" for func in std_funcs[null_start_ind:null_end_ind+1]}
    ret_types = dict()
    ret_types.update(int_ret_types)
    ret_types.update(real_ret_types)
    ret_types.update(int_real_ret_types)
    ret_types.update(null_ret_types)
    return ret_types[func_name]

def get_func(func_name):
    if func_name == 'sqr':
        return StandardFunctions.sqr
    elif func_name == 'succ':
        return StandardFunctions.succ
    elif func_name == 'sum':
        return sum
    elif func_name == 'sqrt':
        return math.sqrt
    elif func_name == 'max':
        return max
    elif func_name == 'min':
        return min
    elif func_name == 'tan':
        return math.tan
    elif func_name == 'round':
        return round
    elif func_name == 'trunc':
        return int
    elif func_name == 'cos':
        return math.cos
    elif func_name == 'sin':
        return math.sin
    elif func_name == 'pow':
        return math.pow
    elif func_name == 'write':
        return 'write'
    elif func_name == 'writeln':
        return 'writeln'
    elif func_name == 'clock':
        return time

def get_lst_dependent_funcs():
    return ["sum", "min", "max", "write", "writeln"]
 