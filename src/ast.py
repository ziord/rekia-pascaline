import utility
import psl_builtins
import psl_exception


class AST:
    pass

class NoOp(AST):
    pass

class Num(AST):
    def __init__(self, token):
        self._token = token
        self._value = token._value
    
class Var(AST):
    def __init__(self, token, reference=False):
        self._token = token
        self._value = token._value
        self._reference = reference 

class UnaryOp(AST):
    def __init__(self, op, right):
        self._token = self._op = op
        self._right = right
    
class BinOp(AST):
    def __init__(self, left, op, right):
        self._left = left 
        self._op = op
        self._right = right

class Assign(AST):
    def __init__(self, left, op, right):
        self._left = left
        self._op = op
        self._right = right

class MultiAssign(AST):
    def __init__(self):
        self.assignment_nodes = []

class Type(AST):
    def __init__(self, token):
        self._token = token
        self._type = token._value

class VarDecl(AST):
    def __init__(self, var_node, type_node, ):
        self._var_node = var_node
        self._type_node = type_node
        
    
class Compound(AST):
    def __init__(self):
        self.children = []

class Procedure(AST):
    def __init__(self, name, block, params=None):
        self._name = name
        self._block = block
        self._params = params if params else []

class Function(AST): 
    def __init__(self, name, block, params=None, return_type=None):
        self._name = name
        self._block = block
        self._params = params if params else []
        self._return_type = return_type
        self._token = name._token

class FunctionCall(AST):
    def __init__(self, node, params=None):
        self._name = node._value
        self._token = node._token
        self._params = params if params else []

class ProcedureCall(AST):
    def __init__(self, node, params=None):
        self._name = node._value
        self._token = node._token
        self._params = params if params else []

class Return(AST):
    def __init__(self, func_name, value):
        self._func_name = func_name
        self._value = value    
        self._token = value._token if hasattr(value, '_token') else None

class Block(AST):
    def __init__(self, decls_node, comp_node):
        self._decls_node = decls_node
        self._comp_node = comp_node
    
class Program(AST):
    def __init__(self, prog_name_token, block_node):
        self._name = prog_name_token
        self._block_node = block_node

class PreOperation(AST):
    def __init__(self, op, right):
        self._token = right._token
        self._op = op
        self._right = right

class PostOperation(AST):
    def __init__(self, left, op):
        self._left = left
        self._op = op
        self._token = left._token

class CompoundIf(AST):
    def __init__(self):
        self.statements = []

class If(AST):
    def __init__(self, condition, block):
        self._name = "if"
        self._condition = condition
        self._block = block

class ElseIf(AST):
    def __init__(self, condition, block):
        self._name = "elif"
        self._condition = condition
        self._block = block

class Else(AST):
    def __init__(self, block):
        self._name = "else"
        self._block = block
    
class WhileLoop(AST):
    def __init__(self, condition, block):
        self._condition = condition
        self._block = block
    
class ForLoop(AST):
    def __init__(self, initial_condition, stop_condition, loop_condition, block):
        self._initial_condition = initial_condition
        self._stop_condition = stop_condition
        self._loop_condition = loop_condition
        self._block = block
    
class RepeatUntilLoop(AST):
    def __init__(self, condition, block):
        self._condition = condition
        self._block = block
    
class FromLoop(AST):
    def __init__(self, initial_condition, stop_condition, loop_condition, block):
        self._initial_condition = initial_condition
        self._stop_condition = stop_condition
        self._loop_condition = loop_condition
        self._block = block

class LoopControl(AST):
    def __init__(self, control_token):
        self._token = control_token
        self._type = control_token._type
        self._value = control_token._value

class Boolean(AST):
    def __init__(self, token):
        self._token = token
        self._value = token._value
        self._cvalue = 1 if token._value == psl_builtins.TokenType.TRUE.value else 0 #value used for computation

class Pass(AST):
    def __init__(self, token):
        self._token = token

class Char(AST):
    def __init__(self, token):
        self._token = token
        self._value = token._value

class String(AST):
    def __init__(self, token):
        self._token = token
        self._value = token._value
class NodeVisitor(AST):
    def visit(self, node): 
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_exception)
        return visitor(node)
    
    def generic_exception(self):
        raise Exception("Cannot find an attribute for current node")


class NodeTypeVisitor(NodeVisitor):
    """
    Type visitor for the SemanticAnalyzer class.
    Particularly for type checking
    """
    def tvisit(self, node):
        method_name = "tvisit_" + type(node).__name__
        tvisitor = getattr(self, method_name, self.generic_exception)
        return tvisitor(node)
       