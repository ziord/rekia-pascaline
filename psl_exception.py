"""
Error class with customized exceptions for various kinds of errors in the Pascaine programming language
"""
import enum

class ErrCode(enum.Enum):
    ID_NOT_FOUND = "Identifier(ID) not found"
    UNREC_TOK = "Unrecognized token"
    DECLR_ERROR = "Identifier(ID) declared more than once"
    FUNC_DECLR_ERROR = "Function declared more than once"
    REF_BEFORE_ASSIGN = "Identifier(ID) referenced before assignment"
    NAME_ERROR = "Name is not defined"
    RETURN_ERROR = "Function must return a matching value"
    TYPE_MISMATCH = "Operand types does not match"
    ARGS_TYPE_MISMATCH = "Arguments types does not match"
    ILLEGAL_USE_OF_RET = "Return value outside function or procedure definition"
    ILLEGAL_USE_OF_EQ = "Return value outside function or procedure definition"
    
class ErrType(enum.Enum):
    UNBOUND = "Unbound Local Error"
    NAME = "Name Error"
    SEMANTIC = "Semantic Error"
    ARGUMENT = "Argument Error"


class Error(Exception):
    def __init__(self, err_code=None, token=None, msg=None):
        self._err_code = err_code
        self._token = token
        self._msg = msg
    
    def __str__(self):
        return f"{self._msg}"
    
    def __repr__(self):
        return self.__str__()

class SyntaxError(Error): 
    pass

class ParserError(Error):
    pass

class SemanticError(Error):
    pass

class UnboundLocalError(Error):
    pass

class TypeError(Error):
    pass

class TypeMismatchError(Error):
    pass