from collections import namedtuple
import os
from interpreter import *
from psl_builtins import *


_Scope = namedtuple("_Scope", ["name", "level", "enclosing_scope"])

class Src2ScrCompiler(NodeVisitor):
    def __init__(self, parser, file, logger):
        self._parser = parser
        self._file = file
        self._logger = logger
        self._current_scope = None
        
    
    def init_global_scope(self):
        g_s = ScopedSymbolTable('global', 0, None)
        return g_s

    def update_file(self, text):
        ...

    def scan_and_update(self):
        self._file.close()
        contents = open(self._file.name).read()
        ind1 = contents.find('time()')
        if ind1 != -1:
            with open(self._file.name, mode=self._file.mode) as file:
                file.write("from time import time\n\n")
                file.write(contents)
        if any(elem in contents for elem in get_import_dependent_funcs()):
            with open(self._file.name, mode=self._file.mode) as file:
                file.write("from math import *\n\n")
                file.write(contents)
        

    def error(self, err_code, token, arg):
        super().__init__.error(err_code, token, arg)
    
    def visit_NoOp(self, node):
        pass
    
    def visit_Num(self, node):
        self._file.write(str(node._value))
    
    def visit_UnaryOp(self, node):
        if node._op._type == psl_builtins.TokenType.MINUS:
            self._file.write(' - ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.PLUS:
            self._file.write(' + ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.NOT:
            self._file.write(' not ')
            self.visit(node._right)
        
    def visit_BinOp(self, node):
        if node._op._type == psl_builtins.TokenType.PLUS:
            self.visit(node._left)
            self._file.write(' + ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.MINUS:
            self.visit(node._left)
            self._file.write(' - ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.FLOAT_DIV:
            self.visit(node._left)
            self._file.write(' / ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.INTEGER_DIV:
            self.visit(node._left)
            self._file.write(' // ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.MULT:
            self.visit(node._left)
            self._file.write(' * ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.POWER:
            self.visit(node._left)
            self._file.write(' ** ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.LT:
            self.visit(node._left)
            self._file.write(' < ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.GT:
            self.visit(node._left)
            self._file.write(' > ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.LE:
            self.visit(node._left)
            self._file.write(' <= ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.GE:
            self.visit(node._left)
            self._file.write(' >= ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.EQ:
            self.visit(node._left)
            self._file.write(' == ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.NE:
            self.visit(node._left)
            self._file.write(' != ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.AND:
            self.visit(node._left)
            self._file.write(' and ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.OR:
            self.visit(node._left)
            self._file.write(' or ')
            self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.MOD:
            self.visit(node._left)
            self._file.write(' % ')
            self.visit(node._right)
        #self._file.write('\n')

    def visit_Var(self, node):
        var = node._value
        self._file.write(var)
    
    def visit_Assign(self, node):
        #self._file.write("\t"*self._current_scope._scope_level)
        self.visit(node._left) #._value
        op = node._op.value
        self._file.write(' ')
        l = list(op); l.remove(':'); self._file.write(''.join(l))   #adjust this
        self._file.write(' ')
        self.visit(node._right)
        #self._file.write('\n')

    def visit_MultiAssign(self, node):
        for node_ in node.assignment_nodes:
            self.visit(node_)
    
    def visit_VarDecl(self, node):
        pass

    def visit_Function(self, node):
        func_scope = ScopedSymbolTable(node._name._value, self._current_scope._scope_level+1, self._current_scope)
        self._file.write(str('\t'*self._current_scope._scope_level))
        self._current_scope = func_scope
        self._file.write("def ")
        self._file.write(node._name._value)
        self._file.write("(")
        if node._params:
            c = len(node._params)-1
            for param in node._params:
                self.visit(param._var_node)
                if len(node._params) > 1:
                    if c > 1:
                        self._file.write(', ')
                c -= 1
        self._file.write("): \n")
        #self._file.write('\t')
        self.visit(node._block)
        #self._file.write('\n')
        self._current_scope = func_scope._enclosing_scope

    def visit_FunctionCall(self, node):
        func_name = node._name
        params = node._params
        #self._file.write("\t"*self._current_scope._scope_level)
        if func_name in (
            psl_builtins.StandardFunctions.writeln.value,
            psl_builtins.StandardFunctions.write.value
        ):
            self._file.write("print")
        elif func_name == psl_builtins.StandardFunctions.CLOCK.value:
            self._file.write("time")
        else:
            self._file.write(func_name)
        self._file.write("(")
        c = len(node._params)
        if c > 0 and node._params[0] is not None:
            for param in params:
                self.visit(param)
                if len(params) > 1:
                    if c > 1:
                        self._file.write(', ')
                c -= 1
            self._file.write(")")
        else:
            self._file.write(")")
        
    
    def visit_Return(self, node):
        if 'if' in self._current_scope._scope_name:
            self._file.write('\t'*(self._current_scope._scope_level))
        self._file.write("return ")
        self.visit(node._value)
        #self._file.write("\n")

    def visit_PostOperation(self, node):
        #self._file.write("\t"*self._current_scope._scope_level)
        node_ = node._left
        token = node_._token
        op = node._op
        if op == psl_builtins.TokenType.POST_PLUS_PLUS: 
            if node_._token._type == psl_builtins.TokenType.ID: 
                self._file.write(node._token._value)
                self._file.write(" += 1")
            elif node_._token._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                num_node = node_
                init_value = num_node._value
                self._file.write(str(init_value+1))
               
        elif op == psl_builtins.TokenType.POST_MINUS_MINUS:
            if node_._token._type == psl_builtins.TokenType.ID: 
                self._file.write(node._token._value)
                self._file.write(" -= 1")
            elif node_._token._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                num_node = node_
                init_value = num_node._value
                self._file.write(str(init_value-1))

    def visit_PreOperation(self, node):
        #self._file.write("\t"*self._current_scope._scope_level)
        op = node._op
        node_ = node._right
        if op == psl_builtins.TokenType.PRE_PLUS_PLUS:
            
            if node_._token._type == psl_builtins.TokenType.ID: 
                var_node = node_
                var = var_node._value
                self._file.write(var)
                self._file.write(" += 1")
            elif node_._token._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                num_node = node_
                init_value = num_node._value
                self._file.write(str(init_value+1))

        elif op == psl_builtins.TokenType.PRE_MINUS_MINUS:
            if node_._token._type == psl_builtins.TokenType.ID: 
                var_node = node_
                var = var_node._value
                self._file.write(var)
                self._file.write(" -= 1")
            elif node_._token._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                num_node = node_
                init_value = num_node._value
                self._file.write(str(init_value-1))

    def visit_CompoundIf(self, node):
        statements = node.statements
        for node_ in statements:
            condition = node_._condition if node_._name in ("if", "elif", "else if") else None
            block = node_._block
            
            if node_._name == psl_builtins.TokenType.IF.value:
                if_scope = ScopedSymbolTable(str('if'+str(self._current_scope._scope_level)), self._current_scope._scope_level+1, self._current_scope)
                self._current_scope = if_scope
                self._file.write("\t"*(self._current_scope._scope_level-1))
                self._file.write("if ")
                self.visit(condition)
                self._file.write(": \n")
                #self._file.write('\t')
                self.visit(block)
                self._current_scope = self._current_scope._enclosing_scope
                
            if node_._name in (psl_builtins.TokenType.ELSE_IF.value, psl_builtins.TokenType.ELIF.value):
                elif_scope = ScopedSymbolTable(str('elif'+str(self._current_scope._scope_level)), self._current_scope._scope_level+1, self._current_scope)
                self._current_scope = elif_scope
                self._file.write("\n")
                self._file.write("\t"*(self._current_scope._scope_level-1))
                self._file.write("elif ")
                self.visit(condition)
                self._file.write(': \n')
                self.visit(block)
                self._current_scope = self._current_scope._enclosing_scope
            
            if node_._name == psl_builtins.TokenType.ELSE.value:
                else_scope = ScopedSymbolTable(str('else'+str(self._current_scope._scope_level)), self._current_scope._scope_level+1, self._current_scope)
                self._current_scope = else_scope
                self._file.write("\n")
                self._file.write("\t"*(self._current_scope._scope_level-1))
                self._file.write("else: \n")
                #self._file.write('\t')
                self.visit(block)
                self._current_scope = self._current_scope._enclosing_scope
       
    def visit_If(self, node):
        condition = node._condition if node._name in ("if", "elif", "else if") else None
        block = node._block
        if_scope = ScopedSymbolTable(str('if'+str(self._current_scope._scope_level)), self._current_scope._scope_level+1, self._current_scope)
        self._current_scope = if_scope
        #self._file.write("\t"*(self._current_scope._scope_level-1))
        self._file.write("if ")
        self.visit(condition)
        self._file.write(": \n")
        #self._file.write('\t')
        self.visit(block)
        self._current_scope = self._current_scope._enclosing_scope
        ...
    
    def visit_ElseIf(self, node):
        ...

    def visit_Else(self, node):
        ...

    def visit_LoopControl(self, node):
        #self._file.write("\t"*self._current_scope._scope_level)
        ctrl_val = node._value
        if ctrl_val == 'stop':
            self._file.write('break')
            return
        elif ctrl_val == 'skip':
            self._file.write('continue')
            return
        self._file.write(ctrl_val)

    def visit_WhileLoop(self, node):
        #self._file.write("\t"*self._current_scope._scope_level)
        condition = node._condition
        block = node._block
        self._file.write("while ")
        self.visit(condition)
        self._file.write(": \n")
        self._file.write('\t')
        self.visit(block)
    
    def visit_ForLoop(self, node):
        #self._file.write("\t"*self._current_scope._scope_level)
        init_condition = node._initial_condition
        stop_condition = node._stop_condition
        loop_condition = node._loop_condition._value
        block = node._block
        

        assign_node_lst = init_condition.assignment_nodes
        assign_node = assign_node_lst[0]
        self.visit(assign_node) 
        var_node = assign_node._left

        self._file.write("for ")
        self._file.write(var_node._value)
        self._file.write(" in range(")
        self._file.write(str(assign_node._left._value))
        self._file.write(", ")
        self._file.write(str(stop_condition._value+1))
        self._file.write("): \n")
        self._file.write('\t')
        self.visit(block)

    
    def visit_RepeatUntilLoop(self, node):
        #self._file.write("\t"*self._current_scope._scope_level)
        condition = node._condition
        block = node._block
        loop_control = str()
        self._file.write("while not ")
        self.visit(condition)
        self._file.write(": \n")
        self._file.write('\t')
        self.visit(block)
    
    def visit_FromLoop(self, node):
        #self._file.write("\t"*self._current_scope._scope_level)
        init_condition = node._initial_condition
        stop_condition = node._stop_condition
        loop_condition = node._loop_condition
        block = node._block
        self._file.write("for i in range(")
        self._file.write(str(self.visit(init_condition)))
        self._file.write(", ")
        self._file.write(str(self.visit(stop_condition)))
        self._file.write("): \n")
        self._file.write('\t')
        self.visit(block)

    def visit_Boolean(self, node):
        self._file.write(node._cvalue)
    
    def visit_Procedure(self, node):
        #self._proc_nodes.append(node)
        func_scope = ScopedSymbolTable(node._name._value, self._current_scope._scope_level+1, self._current_scope)
        self._file.write(str('\t'*self._current_scope._scope_level))
        self._current_scope = func_scope
        self.file.write("def ")
        self._file.write(node._name)
        self._file.write("(")
        if node._params:
            c = len(node._params)-1
            for param in node._params:
                self.visit(param._var_node)
                if len(node._params) > 1:
                    if c > 1:
                        self._file.write(', ')
                c -= 1
        self._file.write("): \n")
        self._file.write('\t')
        self.visit(node._block)
        #self._file.write('\n')
        self._current_scope = func_scope.enclosing_scope
    
    def visit_ProcedureCall(self, node):
        #self._file.write("\t"*self._current_scope._scope_level)
        func_name = node._name
        params = node._params
        self._file.write(func_name)
        self._file.write("(")
        c = len(node._params)
        if c > 0 and node._params[0] is not None:
            for param in params:
                self.visit(param)
                if len(params) > 1:
                    if c > 1:
                        self._file.write(', ')
                c -= 1
            self._file.write(")")
        else:
            self._file.write(")")

    def visit_Pass(self, node):
        self._file.write("\t"*self._current_scope._scope_level)
        self._file.write("pass")
    
    def visit_Compound(self, node):
        for node_ in node.children:
            self._file.write("\t"*self._current_scope._scope_level)
            self.visit(node_)
            self._file.write('\n')
    
    def visit_String(self, node):
        self._file.write(repr(node._value))
    
    def visit_Char(self, node):
        self._file.write(repr(node._value))

    def visit_Block(self, node):
        decls_node = node._decls_node
        comp_node = node._comp_node
        for decl_node in decls_node:
            self.visit(decl_node)
        self.visit(comp_node)

    def visit_Program(self, node):
        self._current_scope = self.init_global_scope()
        self.visit(node._block_node)
        self._current_scope = self._current_scope._enclosing_scope

    def compile(self):
        root_node = self._parser.parse()
        sem = SemanticAnalyzer(self._logger)
        sem.visit(root_node)
        self.visit(root_node)
        self.scan_and_update()

if __name__ == '__main__':
        strn = "condtnls.py"
        strn2 = "fctrl2.py"
        strn3 = "lps.py"
        strn4 = "fctl2.py"
        pfile = open(strn, 'w+')
        file1 = "psl code snippets/factorial2.psl"
        file2= "psl code snippets/conditionals.psl"
        file3 = "psl code snippets/loops.psl"
        logger = utility.create_logger()
        text = utility.reader(file2)
        lexer = Lexer(text)
        parser = Parser(lexer)
        compiler = Src2ScrCompiler(parser, pfile, logger)
        compiler.compile()
        pfile.close()
        