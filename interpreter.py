from ast import *
from psl_parser import *

class Interpreter(NodeVisitor):
    """
    Interpreter, visits each node respectively
    """
    def __init__(self, parser, logger=None):
        self._parser = parser
        self._current_memory_space = None
        self._func_nodes = []
        self._proc_nodes = []
        self._logger = logger
    
    def error(self, err_code, token, arg):
        if arg == psl_exception.ErrType.UNBOUND:
            raise psl_exception.UnboundLocalError(
                err_code=err_code, 
                token=token, 
                msg=f"{err_code.value} -> {token}"
            )
        if arg == psl_exception.ErrType.NAME or arg == psl_exception.ErrType.SEMANTIC:
            raise psl_exception.SemanticError(
                err_code=err_code, 
                token=token, 
                msg=f"{err_code.value} -> {token}"
            )
        if arg == psl_exception.ErrType.ARGUMENT:
            raise psl_exception.TypeError(
                err_code=err_code,
                token=None, 
                msg=f"{err_code}" 
            )
    
    def _init_builtins_mem_space(self):
        self._current_memory_space = ScopedMemorySpace(
                                        scope_name="Global Memory Space",
                                        scope_level=1,
                                        enclosing_mem_space=None
                                    )
    
    def visit_NoOp(self, node):
        pass
    
    def visit_Num(self, node):
        return node._value
    
    def visit_UnaryOp(self, node):
        if node._op._type == psl_builtins.TokenType.MINUS:
            return -self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.PLUS:
            return +self.visit(node._right)
        elif node._op._type == psl_builtins.TokenType.NOT:
            return not self.visit(node._right)
       
    def visit_BinOp(self, node):
        if node._op._type == psl_builtins.TokenType.PLUS:
            return self.visit(node._left) + self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.MINUS:
            return self.visit(node._left) - self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.FLOAT_DIV:
            return self.visit(node._left) / self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.INTEGER_DIV:
            return self.visit(node._left) // self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.MULT:
            return self.visit(node._left) * self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.POWER:
            return self.visit(node._left) ** self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.LT:
            return self.visit(node._left) < self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.GT:
            return self.visit(node._left) > self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.LE:
            return self.visit(node._left) <= self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.GE:
            return self.visit(node._left) >= self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.EQ:
            return self.visit(node._left) == self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.NE:
            return self.visit(node._left) != self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.AND:
            return self.visit(node._left) and self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.OR:
            return self.visit(node._left) or self.visit(node._right)
        if node._op._type == psl_builtins.TokenType.MOD:
            return self.visit(node._left) % self.visit(node._right)
        
    
    def visit_Var(self, node):
        var = node._value
        try:
            val = self._current_memory_space.retrieve(var)
        except:
            self.error(
                err_code=psl_exception.ErrCode.REF_BEFORE_ASSIGN,
                token=node._token,
                arg=psl_exception.ErrType.UNBOUND,
            )
        if val is None: 
            self.error(
                err_code=psl_exception.ErrCode.REF_BEFORE_ASSIGN,
                token=node._token,
                arg=psl_exception.ErrType.UNBOUND,
            )
        return val
    
    def visit_Assign(self, node):
        var = node._left._value
        op = node._op
        token = node._left._token
        if op == psl_builtins.TokenType.ASSIGN:
            val = self.visit(node._right) 
            self._current_memory_space.insert(var, val) 
        
        elif op == psl_builtins.TokenType.PLUS_ASSIGN:
            try:
                init_val = self._current_memory_space.retrieve(var)
            except: 
                self.error(
                    err_code=psl_exception.ErrCode.REF_BEFORE_ASSIGN,
                    token=token, 
                    arg=psl_exception.ErrType.UNBOUND,
                )
            val =  init_val + self.visit(node._right) 
            self._current_memory_space.insert(var, val)

        elif op == psl_builtins.TokenType.MINUS_ASSIGN:
            try:
                init_val = self._current_memory_space.retrieve(var)
            except: 
                self.error(
                    err_code=psl_exception.ErrCode.REF_BEFORE_ASSIGN,
                    token=token, 
                    arg=psl_exception.ErrType.UNBOUND,
                )
            val = init_val - self.visit(node._right) 
            self._current_memory_space.insert(var, val)

        elif op == psl_builtins.TokenType.FLOAT_DIV_ASSIGN:
            try:
                init_val = self._current_memory_space.retrieve(var)
            except: 
                self.error(
                    err_code=psl_exception.ErrCode.REF_BEFORE_ASSIGN,
                    token=token,
                    arg=psl_exception.ErrType.UNBOUND,
                )
            val = init_val / self.visit(node._right)  
            self._current_memory_space.insert(var, val)

        elif op == psl_builtins.TokenType.MULT_ASSIGN:
            try:
                init_val = self._current_memory_space.retrieve(var)
            except: 
                self.error(
                    err_code=psl_exception.ErrCode.REF_BEFORE_ASSIGN,
                    token=token, 
                    arg=psl_exception.ErrType.UNBOUND,
                )
            val = init_val * self.visit(node._right)  
            self._current_memory_space.insert(var, val)
        
        elif op == psl_builtins.TokenType.POWER_ASSIGN:
            try:
                init_val = self._current_memory_space.retrieve(var)
            except: 
                self.error(
                    err_code=psl_exception.ErrCode.REF_BEFORE_ASSIGN,
                    token=token, 
                    arg=psl_exception.ErrType.UNBOUND,
                )
            val = init_val ** self.visit(node._right)  
            self._current_memory_space.insert(var, val)
        
        elif op == psl_builtins.TokenType.MOD_ASSIGN:
            try:
                init_val = self._current_memory_space.retrieve(var)
            except: 
                self.error(
                    err_code=psl_exception.ErrCode.REF_BEFORE_ASSIGN,
                    token=token, 
                    arg=psl_exception.ErrType.UNBOUND,
                )
            val = init_val % self.visit(node._right)  
            self._current_memory_space.insert(var, val)
        self._current_memory_space.update_vals()

    def visit_MultiAssign(self, node):
        for node_ in node.assignment_nodes:
            self.visit(node_)

    def visit_VarDecl(self, node):
        var = node._var_node._value
        type_ = node._type_node._type
        self._current_memory_space.store_var_type(var, type_)
    
    def visit_Function(self, node):
        self._func_nodes.append(node)

    
    def initialize_params(self, values_param, func_params, func_name): 
        for i, param in enumerate(values_param):
            var_decl = func_params[i]
            var_name = var_decl._var_node._value
            value = self.visit(values_param[i])
            self._current_memory_space.insert(var_name, value)
               
    def visit_FunctionCall(self, node):
        func_name = node._name
        params = node._params
        func_mem_space = ScopedMemorySpace(
            scope_name=func_name, 
            scope_level=self._current_memory_space._scope_level+1,
            enclosing_mem_space=self._current_memory_space,
            current_scope_only = False
        )
        self._current_memory_space = func_mem_space
        kwfunc = None 
        for func in self._func_nodes:
            if func._name._value == func_name:
                kwfunc = func
                break
        if kwfunc is None:
            if func_name in psl_builtins.get_standard_func():
                nparams = []
                if params[0] is not None:
                    for param_ in params:
                        nparams.append(self.visit(param_))
                kwfunc = psl_builtins.get_func(func_name)
                val = None
                
                if func_name in psl_builtins.get_lst_dependent_funcs():
                    if func_name in (psl_builtins.StandardFunctions.write.value, psl_builtins.StandardFunctions.writeln.value):
                        if func_name == psl_builtins.StandardFunctions.write.value:
                            print(*nparams, end=' ')
                        elif func_name == psl_builtins.StandardFunctions.writeln.value:
                            print(*nparams)
                            self._current_memory_space = self._current_memory_space._enclosing_mem_space
                        return
                    val = kwfunc(nparams)

                elif len(nparams) == 2:
                    val = kwfunc(nparams[0], nparams[1])
                elif len(nparams) == 1:
                    val = kwfunc(nparams[0])
                else:
                    val = kwfunc()
                self._current_memory_space = self._current_memory_space._enclosing_mem_space
                return val
            elif self._proc_nodes:
                name = Var(node._token)
                token = node._token
                params = node._params
                proc_node = ProcedureCall(name, params)
                proc_node._token = token
                self.visit(proc_node)
                return
                
            else:
                self.error(
                    err_code=psl_exception.ErrCode.NAME_ERROR,
                    token=Token(psl_builtins.TokenType.FUNC_ID, func_name),
                    arg=psl_exception.ErrType.NAME,
                )

        kwfunc_params = kwfunc._params
        block = kwfunc._block

        self.initialize_params(params, kwfunc_params, func_name)
        self.visit(block)
        
        for i, param_ in enumerate(kwfunc_params):
            if param_._var_node._reference:
                val = self.visit(param_._var_node)
                var = params[i]._value 
                self._current_memory_space._enclosing_mem_space.insert(var, val)

        ret_type = kwfunc._return_type._type if kwfunc._return_type else None   
        var_token = kwfunc._token

        tok = func_name + ' : ' + (ret_type if ret_type else psl_builtins.StandardTypes.NULL.value) + \
            ' (' + str(var_token._linenum) + ':' + str(var_token._colnum) + ')'
        if ret_type: 
            try: 
                val = self._current_memory_space.retrieve(func_name)
            except AttributeError:
                self.error(
                    err_code=psl_exception.ErrCode.RETURN_ERROR,
                    token=tok,
                    arg=psl_exception.ErrType.SEMANTIC
                    )
            else:
                self._current_memory_space = self._current_memory_space._enclosing_mem_space
                return val
        else:
            self._current_memory_space = self._current_memory_space._enclosing_mem_space
        
    def visit_Return(self, node):
        func_name = node._func_name._value
        value = self.visit(node._value)
        self._current_memory_space.insert(func_name, value)


    def visit_PostOperation(self, node):
        node_ = node._left
        token = node_._token
        op = node._op
        if op == psl_builtins.TokenType.POST_PLUS_PLUS: 
            if node_._token._type == psl_builtins.TokenType.ID: 
                var_node = node_
                var = var_node._value
                try:
                    init_value = self._current_memory_space.retrieve(var)
                except: 
                    self.error(
                        err_code=psl_exception.ErrCode.REF_BEFORE_ASSIGN,
                        token=token, 
                        arg=psl_exception.ErrType.UNBOUND,
                    )
                val = self.visit(var_node) + 1
                self._current_memory_space.insert(var, val) 
                return init_value
            elif node_._token._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                num_node = node_
                init_value = self.visit(num_node) 
                return init_value
               
        elif op == psl_builtins.TokenType.POST_MINUS_MINUS:
            if node_._token._type == psl_builtins.TokenType.ID: 
                var_node = node_
                var = var_node._value
                try:
                    init_value = self._current_memory_space.retrieve(var)
                except: 
                    self.error(
                        err_code=psl_exception.ErrCode.REF_BEFORE_ASSIGN,
                        token=token, 
                        arg=psl_exception.ErrType.UNBOUND,
                    )
                val = self.visit(var_node) - 1
                self._current_memory_space.insert(var, val) 
                return init_value
            elif node_._token._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                num_node = node_
                init_value = self.visit(num_node) 
                return init_value 


    def visit_PreOperation(self, node):
        op = node._op
        node_ = node._right
        if op == psl_builtins.TokenType.PRE_PLUS_PLUS:
            
            if node_._token._type == psl_builtins.TokenType.ID: 
                var_node = node_
                var = var_node._value
                val = self.visit(var_node) + 1
                self._current_memory_space.insert(var, val) 
                return val
            elif node_._token._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                num_node = node_
                value = self.visit(num_node) + 1
                return value

        elif op == psl_builtins.TokenType.PRE_MINUS_MINUS:
            if node_._token._type == psl_builtins.TokenType.ID: 
                var_node = node_
                var = var_node._value
                val = self.visit(var_node) - 1
                self._current_memory_space.insert(var, val) 
                return val
            elif node_._token._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                num_node = node_
                value = self.visit(num_node) - 1
                return value
            
    def visit_CompoundIf(self, node):
        statements = node.statements
        bool_stack = list()
        for node_ in statements:
            condition = node_._condition if node_._name in ("if", "elif", "else if") else None
            block = node_._block
            
            if node_._name == psl_builtins.TokenType.IF.value:
                
                if self.visit(condition): 
                    bool_stack.append(True) 
                    self.visit(block) 
                else:
                    bool_stack.append(False)
               
            if node_._name in (psl_builtins.TokenType.ELSE_IF.value, psl_builtins.TokenType.ELIF.value):
               
                bool_val = bool_stack.pop() 
                if bool_val is False: 
                    if self.visit(condition): 
                        bool_stack.append(True)
                        self.visit(block) 
                    else: 
                        bool_stack.append(False)
                else: 
                    bool_stack.append(True)
               
            if node_._name == psl_builtins.TokenType.ELSE.value:
                
                bool_val = bool_stack.pop() 
                if bool_val is False: 
                    bool_stack.append(True)
                    self.visit(block) 
                else: 
                    bool_stack.append(True)
               
    
    def visit_If(self, node):
        ...
    
    def visit_ElseIf(self, node):
        ...

    def visit_Else(self, node):
       ...

    def visit_LoopControl(self, node):
        ctrl_val = node._value
        self._current_memory_space.insert(psl_builtins.TokenType.CONTROL.value, ctrl_val); 

    def visit_WhileLoop(self, node):
        condition = node._condition
        block = node._block
        i = 0
        loop_control = str()
        while self.visit(condition): 
            for decl in block._decls_node:
                self.visit(decl)
            for node_ in block._comp_node.children:
                self.visit(node_)
                try:   
                    ctrl_tok = self._current_memory_space.retrieve(psl_builtins.TokenType.CONTROL.value)
                except:
                    ctrl_tok = ''
                loop_control = ctrl_tok
                if ctrl_tok in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                    break
                elif ctrl_tok in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                    break
            if loop_control in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                self._current_memory_space.purge()
                break
            elif loop_control in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                self._current_memory_space.purge()
                continue

    def visit_ForLoop(self, node):
        init_condition = node._initial_condition
        stop_condition = node._stop_condition
        loop_condition = node._loop_condition
        block = node._block
        assign_node_lst = init_condition.assignment_nodes
        assign_node = assign_node_lst[0]
        self.visit(assign_node) 
        var_node = assign_node._left
        val = self._current_memory_space.retrieve(var_node._value)
        stop_condition_val = self.visit(stop_condition)
        loop_condition_val = loop_condition._value
        loop_control = str()
        if loop_condition_val == psl_builtins.TokenType.TO.value:
            while val <= stop_condition_val:
                for decl in block._decls_node:
                    self.visit(decl)
                for node_ in block._comp_node.children:
                    self.visit(node_)
                    try:
                        ctrl_val = self._current_memory_space.retrieve(psl_builtins.TokenType.CONTROL.value)
                    except:
                        ctrl_val = ''
                    loop_control = ctrl_val
                    if ctrl_val in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                        break
                    elif ctrl_val in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                        break
                val = self._current_memory_space.retrieve(var_node._value)
                val += 1
                self._current_memory_space.insert(var_node._value, val)
                if loop_control in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                    self._current_memory_space.purge()
                    break
                elif loop_control in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                    self._current_memory_space.purge()
                    continue 
        elif loop_condition_val == psl_builtins.TokenType.DOWNTO.value:
             while  stop_condition_val <= val:
                    for decl in block._decls_node:
                        self.visit(decl)
                    for node_ in block._comp_node.children:
                        self.visit(node_)
                        try:
                            ctrl_val = self._current_memory_space.retrieve(psl_builtins.TokenType.CONTROL.value)
                        except:
                            ctrl_val = ''
                        loop_control = ctrl_val
                        if ctrl_val in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                            break
                        elif ctrl_val in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                            break
                    val = self._current_memory_space.retrieve(var_node._value)
                    val -= 1
                    self._current_memory_space.insert(var_node._value, val)
                    if loop_control in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                        self._current_memory_space.purge()
                        break
                    elif loop_control in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                        self._current_memory_space.purge()
                        continue 

    
    def visit_RepeatUntilLoop(self, node):
        condition = node._condition
        block = node._block
        loop_control = str()
        for decl in block._decls_node:
            self.visit(decl)
        for node_ in block._comp_node.children:
            self.visit(node_)
            try:
                ctrl_val = self._current_memory_space.retrieve(psl_builtins.TokenType.CONTROL.value)
            except:
                ctrl_val = ''
            loop_control = ctrl_val
            if ctrl_val in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                self._current_memory_space.purge()
                break;
            elif ctrl_val in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                self._current_memory_space.purge()
                continue;
        
        while not self.visit(condition):
            for decl in block._decls_node:
                self.visit(decl)
            for node_ in block._comp_node.children:
                self.visit(node_)
                try:
                    ctrl_val = self._current_memory_space.retrieve(psl_builtins.TokenType.CONTROL.value)
                except:
                    ctrl_val = ''
                loop_control = ctrl_val
                if ctrl_val in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                    self._current_memory_space.purge()
                    break;
                elif ctrl_val in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                    self._current_memory_space.purge()
                    continue;
            if loop_control in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                self._current_memory_space.purge()
                break
            elif loop_control in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                self._current_memory_space.purge()
                continue 
            

    def visit_FromLoop(self, node):
        init_condition = node._initial_condition
        stop_condition = node._stop_condition
        loop_condition = node._loop_condition
        block = node._block
        init_condition_val = self.visit(init_condition)
        stop_condition_val = self.visit(stop_condition)
        
        loop_condition_val = loop_condition._value
        loop_control = str()
        val = init_condition_val
        if loop_condition_val == psl_builtins.TokenType.TO.value:
            while val <= stop_condition_val:
                for decl in block._decls_node:
                    self.visit(decl)
                for node_ in block._comp_node.children:
                    self.visit(node_)
                    try:
                        ctrl_val = self._current_memory_space.retrieve(psl_builtins.TokenType.CONTROL.value)
                    except:
                        ctrl_val = ''
                    loop_control = ctrl_val
                    if ctrl_val in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                        break
                    elif ctrl_val in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                        break
                val += 1
                if loop_control in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                    self._current_memory_space.purge()
                    break
                elif loop_control in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                    self._current_memory_space.purge()
                    continue 
        elif loop_condition_val == psl_builtins.TokenType.DOWNTO.value:
             while  stop_condition_val <= val:
                    for decl in block._decls_node:
                        self.visit(decl)
                    for node_ in block._comp_node.children:
                        self.visit(node_)
                        try:
                            ctrl_val = self._current_memory_space.retrieve(psl_builtins.TokenType.CONTROL.value)
                        except:
                            ctrl_val = ''
                        loop_control = ctrl_val
                        if ctrl_val in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                            break
                        elif ctrl_val in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                            break
                    val -= 1
                    if loop_control in (psl_builtins.TokenType.BREAK.value, psl_builtins.TokenType.STOP.value):
                        self._current_memory_space.purge()
                        break
                    elif loop_control in (psl_builtins.TokenType.CONTINUE.value, psl_builtins.TokenType.SKIP.value):
                        self._current_memory_space.purge()
                        continue 

    def visit_Boolean(self, node):
        return node._cvalue
    
    def visit_Procedure(self, node):
        self._proc_nodes.append(node)

    def visit_ProcedureCall(self, node):
        proc_name = node._name
        params = node._params
        proc_mem_space = ScopedMemorySpace(
            scope_name=proc_name, 
            scope_level=self._current_memory_space._scope_level+1,
            enclosing_mem_space=self._current_memory_space,
        )
        self._current_memory_space = proc_mem_space
        kwproc = None
        for proc in self._proc_nodes:
            if proc._name._value == proc_name:
                kwproc = proc
                break
        if kwproc:
            kwproc_params = kwproc._params
            self.initialize_params(params, kwproc_params, proc_name)
            self.visit(kwproc._block)
            self._current_memory_space = self._current_memory_space._enclosing_mem_space
            return
        else:
            self.error(err_code=psl_exception.ErrCode.NAME_ERROR, token=node._token)
    
    def visit_Pass(self, node):
        pass
        
    def visit_Compound(self, node):
        for node_ in node.children:
            self.visit(node_)
            if type(node_) == LoopControl:
                return 
    
    def visit_String(self, node):
        return node._value
    
    def visit_Char(self, node):
        return node._value

    def visit_Block(self, node):
        decls_node = node._decls_node
        comp_node = node._comp_node
        for decl_node in decls_node:
            self.visit(decl_node)
        self.visit(comp_node)
    
    def visit_Program(self, node):
        self._init_builtins_mem_space()
        self.visit(node._block_node)
    
    def interpret(self):
        root_node = self._parser.parse()
        sem = SemanticAnalyzer(self._logger)
        sem.visit(root_node)
        self.visit(root_node)