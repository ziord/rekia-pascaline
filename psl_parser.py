from ast import *
from lexer import *

class Parser:
    def __init__(self, lexer):
        self._lexer = lexer
        self._current_token = self._lexer.get_next_token()
    
    def error(self):
        errcode = psl_exception.ErrCode.UNREC_TOK
        token = self._current_token
        msg = errcode.value + " -> " + str(token)
        raise psl_exception.ParserError(err_code=errcode, token=token, msg=msg)
    
    def consume_token(self, type):
        if self._current_token._type == type:
            self._current_token = self._lexer.get_next_token()
        else:
            self.error()
            
    def number(self):
        token = self._current_token
        node = Num(token)
        if token._type == psl_builtins.TokenType.FLOAT_CONST:
            self.consume_token(psl_builtins.TokenType.FLOAT_CONST)
        elif token._type == psl_builtins.TokenType.INTEGER_CONST:
            self.consume_token( psl_builtins.TokenType.INTEGER_CONST)
        return node

    def variable(self):
        token = self._current_token
        self.consume_token(psl_builtins.TokenType.ID)
        node = Var(token)
        return node
    
    def factor(self): 
        """
        Factor is the smallest unit of an expression and has the highest precedence
        e.g. of factor includes: a number(3, 4.4), parenthesis expressions,
        post/pre-operations, Unary operations
        """
        token = self._current_token
        if token._type ==  psl_builtins.TokenType.INTEGER_CONST:
            node = self.number()
            return node
        elif token._type ==  psl_builtins.TokenType.FLOAT_CONST:
            node = self.number()
            return node
        elif token._type ==  psl_builtins.TokenType.LPAREN:
            self.consume_token( psl_builtins.TokenType.LPAREN)
            node = self.complex_expr()
            self.consume_token(psl_builtins.TokenType.RPAREN)
            return node
        elif token._type ==  psl_builtins.TokenType.PLUS:
            self.consume_token(psl_builtins.TokenType.PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token._type ==  psl_builtins.TokenType.MINUS:
            self.consume_token(psl_builtins.TokenType.MINUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token._type == psl_builtins.TokenType.CHAR_CONST:
            self.consume_token(psl_builtins.TokenType.CHAR_CONST)
            node = Char(token)
            return node
        elif token._type == psl_builtins.TokenType.STRING_CONST:
            self.consume_token(psl_builtins.TokenType.STRING_CONST)
            node = String(token)
            return node
        elif token._type ==  psl_builtins.TokenType.PRE_MINUS_MINUS:
            self.consume_token(psl_builtins.TokenType.PRE_MINUS_MINUS)
            token_ = token._value #? id? or number? 
            if token_._type == psl_builtins.TokenType.ID:
                node_ = Var(token_)
                node = PreOperation(psl_builtins.TokenType.PRE_MINUS_MINUS, node_)
                return node
            elif token_._type in (psl_builtins.TokenType.FLOAT_CONST, psl_builtins.TokenType.FLOAT_CONST):
                node_ = Num(token_)
                node = PreOperation(psl_builtins.TokenType.PRE_MINUS_MINUS, node_)
                return node
        elif token._type == psl_builtins.TokenType.PRE_PLUS_PLUS:
            self.consume_token(psl_builtins.TokenType.PRE_PLUS_PLUS)
            token_ = token._value
            if token_._type == psl_builtins.TokenType.ID:
                node_ = Var(token_)
                node = PreOperation(psl_builtins.TokenType.PRE_PLUS_PLUS, node_)
                return node
            elif token_._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                node_ = Num(token_)
                node = PreOperation(psl_builtins.TokenType.PRE_PLUS_PLUS, node_)
                return node
        elif token._type == psl_builtins.TokenType.POST_MINUS_MINUS:
            self.consume_token(psl_builtins.TokenType.POST_MINUS_MINUS)
            token_ = token._value
            if token_._type == psl_builtins.TokenType.ID:
                node_ = Var(token_)
                node = PostOperation(node_, psl_builtins.TokenType.POST_MINUS_MINUS)
                return node
            elif token_._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                node_ = Num(token_)
                node = PostOperation(node_, psl_builtins.TokenType.POST_MINUS_MINUS)
                return node
        elif token._type == psl_builtins.TokenType.POST_PLUS_PLUS:
            self.consume_token(psl_builtins.TokenType.POST_PLUS_PLUS)
            token_ = token._value
            if token_._type == psl_builtins.TokenType.ID:
                node_ = Var(token_)
                node = PostOperation(node_, psl_builtins.TokenType.POST_PLUS_PLUS)
                return node
            elif token_._type in (psl_builtins.TokenType.INTEGER_CONST, psl_builtins.TokenType.FLOAT_CONST):
                node_ = Num(token_)
                node = PostOperation(node_, psl_builtins.TokenType.POST_PLUS_PLUS)
                return node
        elif token._type == psl_builtins.TokenType.NOT:
            self.consume_token(psl_builtins.TokenType.NOT)
            node = UnaryOp(token, self.complex_expr())
            return node
        if self._current_token._type in (psl_builtins.TokenType.TRUE, psl_builtins.TokenType.FALSE):
            node = Boolean(self._current_token)
            self.consume_token(self._current_token._type)
            return node
        else:
            var_node = self.variable()
            if self._current_token._type == psl_builtins.TokenType.LPAREN:
                params = self.function_call_statement()
                node = FunctionCall(var_node, params)
                return node
            node = var_node        
            return node
    
    def compound_term(self):
        node = self.factor()
        while self._current_token._type == psl_builtins.TokenType.POWER:
            token = self._current_token
            self.consume_token(psl_builtins.TokenType.POWER)
            node = BinOp(node, token, self.factor())
        return node
    
    def semi_compound_term(self):
        """
        Handles modulus operator %, this has higher precedence than operators such as +, -,
        *, /, but has lower precedence than power operator **
        """
        node = self.compound_term()
        while self._current_token._type == psl_builtins.TokenType.MOD:
            token = self._current_token
            self.consume_token(psl_builtins.TokenType.MOD)
            node = BinOp(node, token, self.compound_term())
        return node
    
    def term(self): 
        """
        Handles operators such as /, *, which has higher precedence than +, and -
        """
        node = self.semi_compound_term()
        while self._current_token._type in (psl_builtins.TokenType.MULT, \
             psl_builtins.TokenType.FLOAT_DIV, psl_builtins.TokenType.INTEGER_DIV):
            token = self._current_token
            if token._type == psl_builtins.TokenType.MULT:
                self.consume_token(psl_builtins.TokenType.MULT)
            elif token._type == psl_builtins.TokenType.FLOAT_DIV:
                self.consume_token(psl_builtins.TokenType.FLOAT_DIV)
            elif token._type == psl_builtins.TokenType.INTEGER_DIV:
                self.consume_token(psl_builtins.TokenType.INTEGER_DIV)
            node = BinOp(node, token, self.semi_compound_term())
        return node
    
    def expr(self):
        """
        For expressions like 2+4-3*2 -- handles operators such as + and - which has lower precedence than
        other operators such as / and *
        expr() calls term() for expressions involving higher precedence
        """
        node = self.term()
        while self._current_token._type in (psl_builtins.TokenType.PLUS, psl_builtins.TokenType.MINUS):
            token = self._current_token
            if token._type == psl_builtins.TokenType.PLUS:
                self.consume_token(psl_builtins.TokenType.PLUS)
            elif token._type == psl_builtins.TokenType.MINUS:
                self.consume_token(psl_builtins.TokenType.MINUS)
            node = BinOp(node, token, self.term())
        return node
    
    def compound_expr(self):
        """
        For expressions like 2+3<56, 45*21>=6, x<=y -- handles the relative operators
        The relative operators (<, >, <=, >=, =) all have lower precedence
        compared to other operators
        """
        node = self.expr()
        while self._current_token._type in (
            psl_builtins.TokenType.LT,
            psl_builtins.TokenType.GT,
            psl_builtins.TokenType.LE,
            psl_builtins.TokenType.GE,
            psl_builtins.TokenType.EQ,
            psl_builtins.TokenType.NE,
            psl_builtins.TokenType.AD
        ):
            token = self._current_token
            if token._type == psl_builtins.TokenType.LT:
                self.consume_token(psl_builtins.TokenType.LT)
            elif token._type == psl_builtins.TokenType.GT:
                self.consume_token(psl_builtins.TokenType.GT)
            elif token._type == psl_builtins.TokenType.LE:
                self.consume_token(psl_builtins.TokenType.LE)
            elif token._type == psl_builtins.TokenType.GE:
                self.consume_token(psl_builtins.TokenType.GE)
            elif token._type == psl_builtins.TokenType.EQ:
                self.consume_token(psl_builtins.TokenType.EQ)
            elif token._type == psl_builtins.TokenType.NE:
                self.consume_token(psl_builtins.TokenType.NE)
            elif token._type == psl_builtins.TokenType.AD:
                self.consume_token(psl_builtins.TokenType.AD)
            node = BinOp(node, token, self.expr())
        return node
    
    def complex_expr(self):
        """
        For expressions like 2+3<56 and 4 < 2* 44-2, 45*21>=6 or 4/3 +-3, not x<=y 
        In short, for expressions including or involving the logical operators
        and, or. The logical not operator is handled in the self.factor() method
        as a unary operation since it always comes first before its operand (which may or may not be an expr)
        The relative operators (<, >, <=, >=, =) all have lower precedence
        compared to other operators
        """
        node = self.compound_expr()
        while self._current_token._type in (
            psl_builtins.TokenType.AND,
            psl_builtins.TokenType.OR
        ):
            token = self._current_token
            if token._type == psl_builtins.TokenType.AND:
                self.consume_token(psl_builtins.TokenType.AND)
            elif token._type == psl_builtins.TokenType.OR:
                self.consume_token(psl_builtins.TokenType.OR)
            node = BinOp(node, token, self.compound_expr())
        return node


    def empty(self):
        node = NoOp()
        return node
    
    def return_statement(self):
        RET = RESERVED_KEYWORDS.get("ret")._type
        if self._current_token._type == RET:
            self.consume_token(RET)
            value = self.complex_expr()
        node = value
        return node

    def assignment_statement(self):
        token = self._current_token
        left = Var(token)
        if hasattr(left, '_params'): 
            return left
        self.consume_token(psl_builtins.TokenType.ID)
        op = self._current_token
        var_node_list = [left]
        values = []
        RET = RESERVED_KEYWORDS.get("ret")._type

        if self._current_token._type == psl_builtins.TokenType.LPAREN:
            params = self.function_call_statement()
            node = FunctionCall(left, params)
            return node

        if self._current_token._type == RET:
            right = self.return_statement() 
            func_name = left; value = right
            node = Return(func_name, value)
            return node

        if self._current_token._type == psl_builtins.TokenType.COMMA: 
            while self._current_token._type == psl_builtins.TokenType.COMMA:
                self.consume_token(psl_builtins.TokenType.COMMA)
                var_node = self.variable()
                var_node_list.append(var_node)
            self.consume_token(psl_builtins.TokenType.ASSIGN)
            right = self.complex_expr()
            values.append(right)
            while self._current_token._type == psl_builtins.TokenType.COMMA:
                self.consume_token(psl_builtins.TokenType.COMMA)
                value = self.complex_expr()
                values.append(value)
            multi_assign = MultiAssign()
            for i, node in enumerate(var_node_list):
                node = Assign(node, psl_builtins.TokenType.ASSIGN, values[i])
                multi_assign.assignment_nodes.append(node)
            return multi_assign
        
        if self._current_token._type == psl_builtins.TokenType.PLUS_ASSIGN:
            op = psl_builtins.TokenType.PLUS_ASSIGN
            self.consume_token(psl_builtins.TokenType.PLUS_ASSIGN)
            right = self.complex_expr()
            node = Assign(left, op, right)
            return node
        
        if self._current_token._type == psl_builtins.TokenType.MINUS_ASSIGN:
            op = psl_builtins.TokenType.MINUS_ASSIGN
            self.consume_token(psl_builtins.TokenType.MINUS_ASSIGN)
            right = self.complex_expr()
            node = Assign(left, op, right)
            return node
        
        if self._current_token._type == psl_builtins.TokenType.MULT_ASSIGN:
            op = psl_builtins.TokenType.MULT_ASSIGN
            self.consume_token(psl_builtins.TokenType.MULT_ASSIGN)
            right = self.complex_expr()
            node = Assign(left, op, right)
            return node
        
        if self._current_token._type == psl_builtins.TokenType.FLOAT_DIV_ASSIGN:
            op = psl_builtins.TokenType.FLOAT_DIV_ASSIGN
            self.consume_token(psl_builtins.TokenType.FLOAT_DIV_ASSIGN)
            right = self.complex_expr()
            node = Assign(left, op, right)
            return node
        
        if self._current_token._type == psl_builtins.TokenType.POWER_ASSIGN:
            op = psl_builtins.TokenType.POWER_ASSIGN
            self.consume_token(psl_builtins.TokenType.POWER_ASSIGN)
            right = self.complex_expr()
            node = Assign(left, op, right)
            return node
        
        if self._current_token._type == psl_builtins.TokenType.MOD_ASSIGN:
            op = psl_builtins.TokenType.MOD_ASSIGN
            self.consume_token(psl_builtins.TokenType.MOD_ASSIGN)
            right = self.complex_expr()
            node = Assign(left, op, right)
            return node

        else: 
            vars_lst = [left]
            op = self._current_token
            while self._current_token._type == psl_builtins.TokenType.ASSIGN:
                self.consume_token(psl_builtins.TokenType.ASSIGN)
                right = self.complex_expr()
            
                if self._current_token._type == psl_builtins.TokenType.LPAREN:
                    params = self.function_call_statement()
                    var_node = right
                    func_call_node = FunctionCall(var_node, params)
                    node = Assign(left, psl_builtins.TokenType.ASSIGN, func_call_node)
                    return node
                vars_lst.append(right)
            ref = vars_lst[len(vars_lst)-1] 
            cln_cpy = vars_lst[:-1]
            multi_assign = MultiAssign()
            for var in cln_cpy:
                node = Assign(var, psl_builtins.TokenType.ASSIGN, ref)
                multi_assign.assignment_nodes.append(node)
            return multi_assign
           
    
    def type_spec(self):
        token = self._current_token
        type_node = Type(token)
        if token._type == psl_builtins.TokenType.FLOAT_CONST:
            self.consume_token(psl_builtins.TokenType.FLOAT_CONST)
        elif token._type == psl_builtins.TokenType.INTEGER_CONST:
            self.consume_token(psl_builtins.TokenType.INTEGER_CONST)
        elif token._type == psl_builtins.TokenType.BOOLEAN:
            self.consume_token(psl_builtins.TokenType.BOOLEAN)
        elif token._type == psl_builtins.TokenType.STRING_CONST:
            self.consume_token(psl_builtins.TokenType.STRING_CONST)
        elif token._type == psl_builtins.TokenType.CHAR_CONST:
            self.consume_token(psl_builtins.TokenType.CHAR_CONST)
        return type_node
    
    def formal_parameter(self):
        VAR = RESERVED_KEYWORDS.get("var")._type
        if self._current_token._type == VAR:
            self.consume_token(VAR)
            var_node = self.variable()
            self.consume_token(psl_builtins.TokenType.COLON)
            type_node = self.type_spec()
            node = VarDecl(var_node, type_node)
        elif self._current_token._type == psl_builtins.TokenType.ID:
            var_node = self.variable()
            self.consume_token(psl_builtins.TokenType.COLON)
            type_node = self.type_spec()
            node = VarDecl(var_node, type_node)
       
        elif self._current_token._type == psl_builtins.TokenType.REF:
            self.consume_token(psl_builtins.TokenType.REF)
            var_node = self.variable()
            var_node._reference = True 
            self.consume_token(psl_builtins.TokenType.COLON)
            type_node = self.type_spec()
            node = VarDecl(var_node=var_node, type_node=type_node)
        return node
    
    def formal_parameter_list(self):
        params = []
        VAR = RESERVED_KEYWORDS.get("var")._type
        while self._current_token._type in (VAR, psl_builtins.TokenType.ID, psl_builtins.TokenType.REF):
            param = self.formal_parameter()
            params.append(param)
            self.consume_token(psl_builtins.TokenType.SEMI)
        return params
            

    def variable_declaration(self):
        var_node = self.variable()
        var_node_lst = [var_node] 
        while self._current_token._type == psl_builtins.TokenType.COMMA:
            self.consume_token(psl_builtins.TokenType.COMMA)
            var_node = self.variable()
            var_node_lst.append(var_node)
        self.consume_token(psl_builtins.TokenType.COLON)
        type_node = self.type_spec()
        var_dec = [VarDecl(var_node, type_node) for var_node in var_node_lst]
        return var_dec

    def declarations(self):
        VAR = RESERVED_KEYWORDS.get("var")._type
        decl_node_lst = []
        while self._current_token._type == VAR:
            self.consume_token(VAR)
            decl = self.variable_declaration()
            decl_node_lst.extend(decl)
            self.consume_token(psl_builtins.TokenType.SEMI)  
            while self._current_token._type == psl_builtins.TokenType.ID:
                decl_node_lst.extend(self.variable_declaration())
                self.consume_token(psl_builtins.TokenType.SEMI)   
        PROCEDURE = RESERVED_KEYWORDS.get("procedure")._type; PROC = RESERVED_KEYWORDS.get("proc")._type;
        while self._current_token._type in (PROCEDURE, PROC):
            node = self.procedure_declaration()
            decl_node_lst.append(node)
        
        
        FUNCTION = RESERVED_KEYWORDS.get("function")._type; FUNC = RESERVED_KEYWORDS.get("func")._type;
        while self._current_token._type in (FUNCTION, FUNC):
            node = self.function_declaration()
            decl_node_lst.append(node)
        return decl_node_lst

    def procedure_declaration(self):
        PROCEDURE = RESERVED_KEYWORDS.get("procedure")._type; PROC = RESERVED_KEYWORDS.get("proc")._type;
        params = []
        if self._current_token._type == PROC:
                self.consume_token(PROC)
        elif self._current_token._type == PROCEDURE:
            self.consume_token(PROCEDURE)
        name = self.variable()
        if self._current_token._type == psl_builtins.TokenType.LPAREN:
            self.consume_token(psl_builtins.TokenType.LPAREN)
            params = self.formal_parameter_list()
            self.consume_token(psl_builtins.TokenType.RPAREN)
        self.consume_token(psl_builtins.TokenType.SEMI)
        block = self.block()
        self.consume_token(psl_builtins.TokenType.SEMI)
        node = Procedure(name, block, params)
        return node
    
    def function_declaration(self):
        FUNCTION = RESERVED_KEYWORDS.get("function")._type; FUNC = RESERVED_KEYWORDS.get("func")._type;
        if self._current_token._type == FUNCTION:
            self.consume_token(FUNCTION)
        elif self._current_token._type == FUNC:
            self.consume_token(FUNC)
        func_name = self.variable()
        return_type = None
        if self._current_token._type == psl_builtins.TokenType.LPAREN:
            self.consume_token(psl_builtins.TokenType.LPAREN)
        params = self.formal_parameter_list()
        if self._current_token._type == psl_builtins.TokenType.RPAREN:
            self.consume_token(psl_builtins.TokenType.RPAREN)
        if self._current_token._type == psl_builtins.TokenType.COLON:
            self.consume_token(psl_builtins.TokenType.COLON)
            return_type = self.type_spec()
            self.consume_token(psl_builtins.TokenType.SEMI)
        else:
            self.consume_token(psl_builtins.TokenType.SEMI)
        block = self.block()
        self.consume_token(psl_builtins.TokenType.SEMI)
        node = Function(func_name, block, params, return_type)
        return node
    
    def function_call_params(self):
       
        try:
            params = []
            if self._current_token._type != psl_builtins.TokenType.RPAREN:
                param = self.complex_expr()
                params.append(param)
            else:
                params.append(None)
            while self._current_token._type == psl_builtins.TokenType.COMMA:
                self.consume_token(psl_builtins.TokenType.COMMA)
                param = self.complex_expr()
                
                params.append(param)
        except Exception as e:
            self.error()
        else:
            return params
    
    
    def function_call_statement(self):
        
        var = self._current_token
        if self._current_token._type == psl_builtins.TokenType.FUNC_ID:
            self.consume_token(psl_builtins.TokenType.FUNC_ID)
        self.consume_token(psl_builtins.TokenType.LPAREN)
        params = self.function_call_params()
        self.consume_token(psl_builtins.TokenType.RPAREN)
        return params

    def pass_statement(self):
        token = self._current_token
        self.consume_token(psl_builtins.TokenType.PASS)
        node = Pass(token)
        return node
    
    def if_statement(self):
        """
        if (condition) then                 
            statements
        else if (condition) then
            statements
        else
            staments
        """
        token = self._current_token
        self.consume_token(psl_builtins.TokenType.IF)

        condition = self.complex_expr() 

        self.consume_token(psl_builtins.TokenType.THEN)
        if self._current_token._type == psl_builtins.TokenType.BEGIN:
            block = self.ifelseblock()
        else:
            block = self.statement();
        node = If(condition, block)
        return node
       
    
    def else_if_statement(self):
        """
        if (condition) then                 
            statements
        else if (condition) then
            statements
        else
            staments
        """
        token = self._current_token
        if self._current_token._type == psl_builtins.TokenType.ELSE_IF:
            self.consume_token(psl_builtins.TokenType.ELSE_IF)
        elif self._current_token._type == psl_builtins.TokenType.ELIF:
            self.consume_token(psl_builtins.TokenType.ELIF)
        
        condition = self.complex_expr()
        self.consume_token(psl_builtins.TokenType.THEN)
        if self._current_token._type == psl_builtins.TokenType.BEGIN:
            block = self.ifelseblock()
        else:
            block = self.statement();
        node = ElseIf(condition, block)
        return node
    
    def else_statement(self):
        """
        if (condition) then                 
            statements
        else if (condition) then
            statements
        else
            staments
        endif
        """
        token = self._current_token
        self.consume_token(psl_builtins.TokenType.ELSE)
        if self._current_token._type == psl_builtins.TokenType.BEGIN:
            block = self.ifelseblock()
        else:
            block = self.statement();
        node = Else(block)
        return node

    def ifelseblock(self):
        node = self.block()
        return node
    
    def if_elseif_else_statement(self):
        if self._current_token._type == psl_builtins.TokenType.IF:
            node = self.if_statement()
            return node
        elif self._current_token._type in (psl_builtins.TokenType.ELSE_IF, psl_builtins.TokenType.ELIF):
            node = self.else_if_statement()
            return node
        elif self._current_token._type == psl_builtins.TokenType.ELSE:
            node = self.else_statement()
            return node
        
    
    def if_elseif_else_statement_list(self):
        lst = []
        while self._current_token._type in (psl_builtins.TokenType.IF,\
            psl_builtins.TokenType.ELSE_IF, psl_builtins.TokenType.ELSE, 
            psl_builtins.TokenType.ELIF):
            node = self.if_elseif_else_statement()
            lst.append(node)
        return lst
    
    def compound_if_statement(self):
        """
        if statements, elif/else if statements, else statements
        """
        node_lst = self.if_elseif_else_statement_list()
        comp_if_node = CompoundIf()
        for node in node_lst:
            comp_if_node.statements.append(node)
        return comp_if_node
    
    def control_statement(self):
        """
        break, continue, or stop -- for loop control
        """
        token = self._current_token
        if self._current_token._type == psl_builtins.TokenType.BREAK:
            self.consume_token(psl_builtins.TokenType.BREAK)
        elif self._current_token._type == psl_builtins.TokenType.STOP:
            self.consume_token(psl_builtins.TokenType.STOP)
        elif self._current_token._type == psl_builtins.TokenType.CONTINUE:
            self.consume_token(psl_builtins.TokenType.CONTINUE)
        elif self._current_token._type == psl_builtins.TokenType.SKIP:
            self.consume_token(psl_builtins.TokenType.SKIP)
        node = LoopControl(token)
        return node
    
    def while_loop_statement(self):
        """
        while i < 5 do
            begin
                a++;
            end;
        """
        self.consume_token(psl_builtins.TokenType.WHILE)
        condition = self.complex_expr()
        self.consume_token(psl_builtins.TokenType.DO)
        block = self.block() 
        node = WhileLoop(condition, block)
        return node

    def for_loop_statement(self):
        """
        for i := 10 to 20 do
            begin
                a++;
            end;
        for i := 20 downto 10 do
            begin
                a++;
            end;
        """
        self.consume_token(psl_builtins.TokenType.FOR)
        initial_condition = self.statement()
        loop_condition = None
        if self._current_token._type == psl_builtins.TokenType.TO:
            loop_condition = self._current_token
            self.consume_token(psl_builtins.TokenType.TO)
        elif self._current_token._type == psl_builtins.TokenType.DOWNTO:
            loop_condition = self._current_token
            self.consume_token(psl_builtins.TokenType.DOWNTO)
        stop_condition = self.complex_expr()
        self.consume_token(psl_builtins.TokenType.DO)
        block = self.block()
        node = ForLoop(initial_condition, stop_condition, loop_condition, block)
        return node
    
    def repeat_until_loop_statement(self):
        """
        repeat
        statement1;
        statement2;
        statement3;
        ...
        until x > 4; --- condition
        """
        self.consume_token(psl_builtins.TokenType.REPEAT)
        block = self.block()
        self.consume_token(psl_builtins.TokenType.UNTIL)
        condition = self.complex_expr()
        node = RepeatUntilLoop(condition, block)
        return node
    
    def from_loop_statement(self):
        self.consume_token(psl_builtins.TokenType.FROM)
        initial_condition = self.complex_expr()
        loop_condition = None
        if self._current_token._type == psl_builtins.TokenType.TO:
            loop_condition = self._current_token
            self.consume_token(psl_builtins.TokenType.TO)
        elif self._current_token._type == psl_builtins.TokenType.DOWNTO:
            loop_condition = self._current_token
            self.consume_token(psl_builtins.TokenType.DOWNTO)
        stop_condition = self.complex_expr()
        self.consume_token(psl_builtins.TokenType.DO)
        block = self.block()
        node = FromLoop(initial_condition, stop_condition, loop_condition, block)
        return node

    def statement(self):
        BEGIN = RESERVED_KEYWORDS.get("begin")._type
        if self._current_token._type == BEGIN:
            node = self.compound_statement()
        elif self._current_token._type == psl_builtins.TokenType.ID:
            node = self.assignment_statement() 
        elif self._current_token._type in (psl_builtins.TokenType.PRE_MINUS_MINUS, \
            psl_builtins.TokenType.PRE_PLUS_PLUS): 
            node = self.complex_expr() 
        elif self._current_token._type in (psl_builtins.TokenType.POST_MINUS_MINUS, \
            psl_builtins.TokenType.POST_PLUS_PLUS):
            node = self.complex_expr()
        elif self._current_token._type == psl_builtins.TokenType.FUNC_ID:
            node = self.function_call_statement()
        elif self._current_token._type == psl_builtins.TokenType.IF:
            node = self.compound_if_statement()
        elif self._current_token._type == psl_builtins.TokenType.WHILE:
            node = self.while_loop_statement()
        elif self._current_token._type == psl_builtins.TokenType.FOR:
            node = self.for_loop_statement()
        elif self._current_token._type == psl_builtins.TokenType.REPEAT:
            node = self.repeat_until_loop_statement()
        elif self._current_token._type == psl_builtins.TokenType.FROM:
            node = self.from_loop_statement()
        elif self._current_token._type == psl_builtins.TokenType.LPAREN:
            node = self.complex_expr()
        elif self._current_token._type in (
            psl_builtins.TokenType.BREAK, 
            psl_builtins.TokenType.CONTINUE,
            psl_builtins.TokenType.STOP,
            psl_builtins.TokenType.SKIP):
            node = self.control_statement()
        elif self._current_token._type in (psl_builtins.TokenType.TRUE, psl_builtins.TokenType.FALSE):
            node = self.complex_expr()
        
        elif self._current_token._type == psl_builtins.TokenType.PASS:
            node = self.pass_statement()
        else:
            node = self.empty()
        return node

    def statement_list(self):
        node = self.statement()
        lst = [node]
        while self._current_token._type == psl_builtins.TokenType.SEMI:# \
            
            self.consume_token(psl_builtins.TokenType.SEMI)
            node = self.statement()
            lst.append(node)
        if self._current_token._type == psl_builtins.TokenType.SEMI:
            self.consume_token(psl_builtins.TokenType.SEMI)
        return lst
    
    def compound_statement(self):
        BEGIN = RESERVED_KEYWORDS.get("begin")._type
        END = RESERVED_KEYWORDS.get("end")._type
        self.consume_token(BEGIN)
        root = self.statement_list()
        self.consume_token(END)
        comp = Compound()
        for node_ in root:
            comp.children.append(node_)
        return comp
        

    def block(self):
        decl_node = self.declarations()
        comp_node = self.compound_statement()
        node = Block(decl_node, comp_node)
        return node
    
    def program(self):
        PROGRAM = RESERVED_KEYWORDS.get("PROGRAM")._type
        self.consume_token(PROGRAM)
        prog_name_node = self.variable()
        self.consume_token(psl_builtins.TokenType.SEMI)
        block_node = self.block()
        self.consume_token(psl_builtins.TokenType.DOT)
        node = Program(prog_name_node, block_node)
        return node

    def parse(self):
        return self.program()

class Symbol(object):
    def __init__(self, name, type=None, category=None):
        self._name = name
        self._type = type
        self._category = category
    
    def __str__(self):
        return self._name
    
    __repr__ = __str__

class BuiltInTypeSymbol(Symbol):
    def __init__(self, name):
        super().__init__(name)
    
    def __str__(self):
        return self._name
    
    def __repr__(self):
        return "<{class_name}(name='{name}')>".format(
            class_name=self.__class__.__name__, name=self.__str__()
        )
    
    def __eq__(self, obj):
        if isinstance(obj, self.__class__):
            return self._name == obj._name
        return NotImplemented

    def __ne__(self, obj):
        b = self.__eq__(obj)
        if b is not NotImplemented:
            return not b
        return b
                
    
class VarSymbol(Symbol):
    def __init__(self, name, type, reference=False):
        super().__init__(name, type)
        self._reference = reference
    
    def __str__(self):
        return "<{class_name}(name='{name}', type='{type}', reference='{ref}')>".format(
            class_name=self.__class__.__name__, name=self._name, type=self._type, ref=self._reference
        )
    
    __repr__ = __str__

class ProcedureSymbol(Symbol):
    def __init__(self, proc_name, params=None):
        super().__init__(proc_name)
        self._params = params if params else []
    
    def __str__(self):
        return "<{class_name}(name={name}, params={params})>".format(
            class_name=self.__class__.__name__,
            name=self._name, 
            params=self._params,
        )
    
    __repr__ = __str__

class FunctionSymbol(Symbol):
    def __init__(self, func_name, params=None, return_type=None):
        super().__init__(func_name)
        self._params = params if params else []
        self._return_type = return_type
    
    def __str__(self):
        return f"<{self.__class__.__name__}(name={self._name}, params={self._params}, ret_type={self._return_type})"
    
    __repr__ = __str__

class StandardFunction(Symbol):
    """
    Standard Function symbol 
    """
    def __init__(self, name, return_type=None):
        super().__init__(name)
        self._return_type = return_type
    
    def __str__(self):
        return f"<{self.__class__.__name__}(name={self._name}, ret_type={self._return_type})>"
    
    def __repr__(self):
        return self.__str__()


class ScopedSymbolTable(Symbol):
    def __init__(self, scope_name, scope_level, enclosing_scope):
        self._symbols = {}
        self._scope_name = scope_name
        self._scope_level = scope_level
        self._enclosing_scope = enclosing_scope
    
    def __str__(self):
        header_1 = "Scoped Symbol Table"
        header_2 = "Scope (scoped symbol table) Contents"
        scope_lst = ['\n', header_1, '='*len(header_1)]
        for header, value in (("Scope name", self._scope_name), \
            ("Scope level", self._scope_level), \
            ("Enclosing scope", self._enclosing_scope._scope_name if self._enclosing_scope else None)):
            scope_lst.append("%-15s: %s"%(header, value))
        scope_lst.extend([header_2, '-'*len(header_2)])
        for symbol in self._symbols.values():
            scope_lst.append("%7s: %r"%(symbol._name, symbol))
        scope_lst.append('\n')
        scope_lst = '\n'.join(scope_lst)
        return scope_lst
    
    __repr__ = __str__

    def _init_builtins(self):
        standard_types = list(psl_builtins.StandardTypes)
        for type in standard_types:
            std_type = BuiltInTypeSymbol(type.value)
            self.insert(std_type)
        standard_funcs = list(psl_builtins.StandardFunctions) 
        for func in standard_funcs: 
            return_type = psl_builtins.get_standard_func_return_type(func.value)
            rt = return_type.split('|') if '|' in return_type else return_type
            if isinstance(rt, list): 
                rt_list = []
                for type_name in rt:
                    rt_list.append(BuiltInTypeSymbol(type_name))
                func_symbol = StandardFunction(name=func.value, return_type=rt_list)
            elif isinstance(rt, str): 
                rt_symbol = BuiltInTypeSymbol(rt)
                func_symbol = StandardFunction(name=func.value, return_type=rt_symbol)
            self.insert(func_symbol)
    
    def insert(self, symbol):
        #print("Insert: %s"%symbol)
        self._symbols[symbol._name] = symbol
    
    def lookup(self, name, current_scope_only=False):
        #print("Lookup: %s"%name)
        symbol = self._symbols.get(name)
        if symbol:
            return symbol
        if current_scope_only:
            return symbol
        if symbol is None:
            return self._enclosing_scope.lookup(name)
    
    def inspect(self, name, current_scope_only=False):
        symbol = self._symbols.get(name)
        if symbol:
            return symbol
        if current_scope_only:
            return symbol
        if symbol is None:
            return self._enclosing_scope.inspect(name)

    
        
#SemanticAnalyzer -- semantic analysis
class SemanticAnalyzer(NodeTypeVisitor): 
    def __init__(self, logger):
        self._current_scope = self._init_builtins_scope() 
        self._logger = logger
    
    def _init_builtins_scope(self):
        """
        creates built-in scope
        """
        self._current_scope = ScopedSymbolTable(scope_name="Builtins", scope_level=0, enclosing_scope=None)
        self._current_scope._init_builtins()
        return self._current_scope
    
    def error(self, *args, err_code, token=None, tok1=None, tok2=None, param_check=False):
        self._logger.log(f"{err_code.value} -> {token}")
        if tok1 and tok2:
            raise psl_exception.TypeMismatchError(
                err_code=err_code,
                token=None,
                msg=err_code.value+ " -> " + str(args[0]) + " and " + str(args[1]) + ": " \
                    +   
                    f"'{tok1._value}' ({tok1._linenum}:{tok1._colnum})" + ", " \
                    f"'{tok2._value}' ({tok2._linenum}:{tok2._colnum})"
            )
        elif param_check:
            raise psl_exception.TypeMismatchError(
                err_code=err_code,
                token=None,
                msg=err_code.value+ " -> " + str(args[0]) + " and " + str(args[1])
            )
        elif token:
            raise psl_exception.SemanticError(
                    err_code=err_code,
                    token=token,
                    msg=f"{err_code.value} -> '{token}'"
                )
        else:
            raise psl_exception.TypeMismatchError(
                err_code=err_code,
                token=None,
                msg=err_code.value+ " -> " + str(args[0]) + " and " + str(args[1])
            )
            

    def visit_Program(self, node):
        prog_name = node._name._value
        self._logger.log(msg="Entering scope: %s\n"%prog_name)
        prog_scope = ScopedSymbolTable(scope_name="global", scope_level=self._current_scope._scope_level+1, \
            enclosing_scope=self._current_scope)
        self._current_scope = prog_scope
        self.visit(node._block_node)
        self._logger.log(msg="Leaving scope: %s\n"%prog_name)
        self._logger.log(str(prog_scope))
        self._current_scope = self._current_scope._enclosing_scope
    
    def visit_Block(self, node):
        decl_node = node._decls_node
        for node_ in decl_node:
            self.visit(node_)
        self.visit(node._comp_node)
    
    def visit_Compound(self, node):
        for node_ in node.children:
            self.visit(node_)
    
    def visit_Procedure(self, node):
        params = node._params
        proc_name = node._name._value
        proc_symbol = ProcedureSymbol(proc_name); 
        self._current_scope.insert(proc_symbol)
        self._logger.log(msg="Entering scope: %s\n"%proc_name)
        proc_scope = ScopedSymbolTable(scope_name=proc_name, scope_level=self._current_scope._scope_level+1,\
             enclosing_scope=self._current_scope)
        self._current_scope = proc_scope
        for params_ in params:
            var_name = params_._var_node._value
            type_name = params_._type_node._type
            reference = params_._var_node._reference
            type_symbol = self._current_scope.lookup(type_name)
            var_symbol = VarSymbol(var_name, type_symbol, reference)
            proc_symbol._params.append(var_symbol)
            self._current_scope.insert(var_symbol)
        self.visit(node._block)
        self._logger.log(msg="Leaving scope: %s\n"%proc_name)
        self._logger.log(str(proc_scope))
        self._current_scope = self._current_scope._enclosing_scope
    
    def visit_Function(self, node):
        params = node._params
        func_name = node._name._value
        block = node._block
        ret_type = node._return_type._type if node._return_type else None 
        self._logger.log(msg="Entering scope: %s\n"%func_name)
        ret_type_symbol = self._current_scope.lookup(ret_type) if node._return_type else \
            self._current_scope.lookup(psl_builtins.StandardTypes.NULL.value) 
        func_symbol = FunctionSymbol(func_name)
        if self._current_scope.inspect(func_name, current_scope_only=True) is not None:
            self.error(err_code=psl_exception.ErrCode.FUNC_DECLR_ERROR, token=node._token)
        self._current_scope.insert(func_symbol)
        func_scope = ScopedSymbolTable(
            func_name,
            self._current_scope._scope_level+1,
            self._current_scope
        ) 
        self._current_scope = func_scope 
        for param in params:
            var_node = param._var_node
            var = var_node._value
            type_node = param._type_node
            type_ = type_node._type
            reference = param._var_node._reference 
            type_symbol = self._current_scope.lookup(type_)
            var_symbol = VarSymbol(var, type_symbol, reference)
            func_symbol._params.append(var_symbol)
            self._current_scope.insert(var_symbol) 
        func_symbol._return_type = ret_type_symbol
        self.visit(block)
        if ret_type:
            pass
        self._logger.log(str(func_scope))
        self._logger.log("Leaving scope: %s\n"%func_name)
        self._current_scope = self._current_scope._enclosing_scope
    
    def visit_FunctionCall(self, node):
        func_name = node._name
        try:
            symbol = self._current_scope.lookup(func_name) 
        except Exception:
            self.error(err_code=psl_exception.ErrCode.NAME_ERROR, token=node._token)
        else:
            call_params = node._params
            
            if not isinstance(symbol, StandardFunction):
                func_params = symbol._params
                self.tvisit(node)
                if len(call_params) > len(func_params) and len(func_params) == 0: 
                        pass
                elif len(call_params) > len(func_params):
                    err_code = "{}".format(
                        func_name + "()" + " takes " + str(len(func_params)) + " expected argument(s), but " \
                            + str(len(call_params)) + " were given"
                    )
                    raise psl_exception.SemanticError(err_code=err_code, token=None, msg=err_code)
                elif len(call_params) < len(func_params):
                    err_code = "{}".format(
                        func_name + "()" + " missing " + str(len(func_params) - len(call_params)) + " expected argument: " +
                        repr(func_params[len(call_params)]._var_node._value)
                    )
                    raise psl_exception.SemanticError(err_code=err_code, token=None, msg=err_code)
                else: 
                    if  call_params[0] is None and func_params[0] is not None: 
                
                        err_code = "{}".format(
                            func_name + "()" + " missing " + str(len(func_params)) + " expected argument: " +
                            repr(func_params[len(call_params)-1]._var_node._value))
                        raise psl_exception.SemanticError(err_code=err_code, token=None, msg=err_code)
            elif isinstance(symbol, StandardFunction):
                if len(call_params) < 1:
                    err_code = f"{func_name} missing expected argument"
                    raise psl_exception.SemanticError(err_code=err_code, token=None, msg=err_code)
        
    
    def visit_Return(self, node):
        if node:
            func_name = node._func_name._value
            value = node._value
            try: 
                func_symbol = self._current_scope.inspect(func_name, current_scope_only=True)
            except:
                self.error(
                    err_code=psl_exception.ErrCode.NAME_ERROR,
                    token=func_name
                )
            else:
                func_symbol_ = self._current_scope.inspect(func_name) 
                current_scope = self._current_scope 
                while self._current_scope._scope_name != func_name: 
                    self._current_scope = self._current_scope._enclosing_scope
                    try:
                        self._current_scope.lookup(func_name) 
                    except: 
                        token = node._value._token
                        self.error(
                            err_code=psl_exception.ErrCode.ILLEGAL_USE_OF_RET, 
                            token=f"{func_name} ret {node._value._value} pos->{token._linenum}:{token._colnum}"
                        )
                self.tvisit(node)
                self._current_scope = current_scope
            
    def visit_Var(self, node):
        var_name = node._value
        try:
            val = self._current_scope.lookup(var_name)
        except:
            self.error(err_code=psl_exception.ErrCode.ID_NOT_FOUND, token=node._token)
        return val
    
    def visit_VarDecl(self, node): 
        var_node = node._var_node
        var_name = var_node._value
        type_node = node._type_node
        type_name = type_node._type
        type_symbol = self._current_scope.lookup(type_name)
        if self._current_scope.inspect(var_name, current_scope_only=True) is not None:
            self.error(
                err_code=psl_exception.ErrCode.DECLR_ERROR,
                token=node._var_node._token,
            )
        var_symbol = VarSymbol(var_name, type_symbol)
        self._current_scope.insert(var_symbol)

    def visit_UnaryOp(self, node):
        self.visit(node._right)
        self.tvisit(node._right)
    
    def visit_BinOp(self, node):
        self.visit(node._left)
        self.visit(node._right)
        self.tvisit(node._left)
        self.tvisit(node._right)
    
    def visit_Assign(self, node):
        self.visit(node._left)
        self.visit(node._right)
    
    def visit_MultiAssign(self, node):
        for node_ in node.assignment_nodes:
            self.visit(node_)
            self.tvisit(node_)

    def visit_PreOperation(self, node):
        self.visit(node._right)
        self.tvisit(node._right)
    
    def visit_PostOperation(self, node):
        self.visit(node._left)
        self.tvisit(node._left)

    def visit_CompoundIf(self, node):
        for node_ in node.statements:
            self.visit(node_)
    
    def visit_Pass(self, node):
        pass

    def visit_If(self, node):
        self.visit(node._condition)
        if_scope = ScopedSymbolTable("if%s"%(str(self._current_scope._scope_level+1)), 
        self._current_scope._scope_level+1, 
        self._current_scope
        )
        self._current_scope = if_scope; 
        
        compound_node = node._block
        self.visit(compound_node)
        self._current_scope = self._current_scope._enclosing_scope
    
    def visit_ElseIf(self, node):
        self.visit(node._condition)
        elif_scope = ScopedSymbolTable("elif%s"%(str(self._current_scope._scope_level+1)), 
        self._current_scope._scope_level+1, 
        self._current_scope
        )
        self._current_scope = elif_scope
       
        compound_node = node._block
        self.visit(compound_node)
        self._current_scope = self._current_scope._enclosing_scope

    def visit_Else(self, node):
        else_scope = ScopedSymbolTable(
            scope_name="else%s"%(str(self._current_scope._scope_level+1)),
            scope_level=self._current_scope._scope_level+1,
            enclosing_scope=self._current_scope
        )
        self._current_scope = else_scope
        
        compound_node = node._block
        self.visit(compound_node)
        self._current_scope = self._current_scope._enclosing_scope
    
    def visit_WhileLoop(self, node):
        condition = node._condition
        block = node._block
        self.visit(condition)
        self.visit(block)

    def visit_ForLoop(self, node): 
        initial_condition = node._initial_condition
        stop_condition = node._stop_condition
        block = node._block
        self.visit(initial_condition)
        self.visit(stop_condition)
        self.visit(block)
    
    def visit_RepeatUntilLoop(self, node):
        condition = node._condition
        block = node._block
        self.visit(condition)
        self.visit(block)
    
    def visit_FromLoop(self, node):
        initial_condition = node._initial_condition
        stop_condition = node._stop_condition
        block = node._block
        self.visit(initial_condition)
        self.visit(stop_condition)
        self.visit(block) 
    
    def visit_Boolean(self, node):
        pass
        
    def visit_LoopControl(self, node):
        pass
 
    def visit_NoOp(self, node):
        pass
    
    def visit_Num(self, node):
        pass
    
    def visit_Char(self, node):
        pass
    
    def visit_String(self, node):
        pass
    
    def build_builtintype(self, type_names):
        if isinstance(type_names, list):
            type_lst = []
            for typename in type_names:
                type_lst.append(BuiltInTypeSymbol(typename))
            return type_lst
        elif isinstance(type_names, str):
            return BuiltInTypeSymbol(str)
    
    def compare_symbol(self, tsym1, tsym2, strict_cmp=False, tok1=None, tok2=None, param_check=False):
        """
        The actual engine, compares type symbols and returns the appropriate type
        """
        num_types_names = [
            psl_builtins.StandardTypes.INTEGER_CONST.value,
            psl_builtins.StandardTypes.FLOAT_CONST.value
        ]
        num_types = self.build_builtintype(num_types_names)
        bool_type = BuiltInTypeSymbol(psl_builtins.StandardTypes.BOOLEAN.value)
        
        if strict_cmp:
            if isinstance(tsym1, BuiltInTypeSymbol):
                if isinstance(tsym2, list):
                    if tsym1 in tsym2:
                        return tsym1
                    elif tsym1 == bool_type and (num_types[0] in tsym2 or num_types[1] in tsym2): 
                        if num_types[0] in tsym2 and num_types[1] in tsym2: 
                            return num_types[1]
                        elif num_types[0] in tsym2 and num_types[1] not in tsym2: 
                            return num_types[0]
                        elif num_types[0] not in tsym2 and num_types[1] in tsym2: 
                            return num_types[1]
                elif tsym1 != tsym2:
                    if tsym1 in num_types and tsym2 == bool_type: 
                        return tsym1
                    elif tsym1 == bool_type and tsym2 in num_types: 
                        return tsym2
                    """
                    elif tsym1 == num_types[1] and tsym2 == num_types[0]: #real := integer -> real (type promotion/casting)
                        return tsym1
                    elif tsym1 == num_types[0] and tsym2 == num_types[1]: #integer := real -> integer(type casting)
                        return tsym1
                    """
                    if tok1 and tok2:
                        self.error(
                            *[tsym1, tsym2],
                            err_code=psl_exception.ErrCode.TYPE_MISMATCH,
                            token=None,
                            tok1=tok1,
                            tok2=tok2,
                        )
                    elif param_check:
                        self.error(
                            *[tsym1, tsym2],
                            err_code=psl_exception.ErrCode.ARGS_TYPE_MISMATCH,
                            token=None,
                            param_check=param_check,
                        )
                    else:
                        self.error(
                            *[tsym1, tsym2],
                            err_code=psl_exception.ErrCode.TYPE_MISMATCH,
                            token=None,
                        )
                else:
                    return tsym1

        if tsym1 == tsym2:
            return tsym1 
        if tsym1 != tsym2:
            if tsym1 in num_types and tsym2 in num_types:
               
                if tsym1 == num_types[0]: 
                    return tsym2 
                elif tsym1 == num_types[1]:
                    return tsym1
            elif tsym1 in num_types and tsym2 == bool_type:
                return tsym1
            elif tsym1 == bool_type and tsym2 in num_types:
                return tsym2
            else:
                self.error(
                    *[tsym1, tsym2],
                    err_code=psl_exception.ErrCode.TYPE_MISMATCH,
                    token=None,
                )

    def tvisit_Char(self, node):
        token = node._token
        tok_type = token._type.value 
        symbol = BuiltInTypeSymbol(tok_type)
        return symbol
    
    def tvisit_String(self, node):
        token = node._token
        tok_type = token._type.value 
        symbol = BuiltInTypeSymbol(tok_type)
        return symbol

    def tvisit_Var(self, node):
        var_symbol = self._current_scope.lookup(node._value)
        return var_symbol._type 
    
    def tvisit_VarDecl(self, node):
        pass
    
    def tvisit_BinOp(self, node):
        tsym1 = self.tvisit(node._left)
        tsym2 = self.tvisit(node._right)
        symbol = self.compare_symbol(tsym1, tsym2)
        return symbol
    
    def tvisit_UnaryOp(self, node):
        return self.tvisit(node._right)
    
    def tvisit_Boolean(self, node):
        token = node._token
        tok_type = token._type.value
        symbol = BuiltInTypeSymbol(psl_builtins.StandardTypes.BOOLEAN.value)
        return symbol
    
    def tvisit_Num(self, node):
        token = node._token
        tok_type = token._type.value 
        symbol = BuiltInTypeSymbol(tok_type)
        return symbol
    
    def tvisit_MultiAssign(self, node):
        for assign_node in node.assignment_nodes:
            self.tvisit(assign_node)
    
    def tvisit_Assign(self, node):
        left = node._left
        right = node._right
        left_sym = self.tvisit(left)
        if left_sym is None: 
            token = left._token
            self.error(
                err_code=psl_exception.ErrCode.ILLEGAL_USE_OF_EQ, 
                token=f"{token._value} := {right._value} pos->{token._linenum}:{token._colnum}"
            )
        right_sym = self.tvisit(right)
        op = node._op    
        if op == psl_builtins.TokenType.ASSIGN:
            if hasattr(left, '_token') and hasattr(right, '_token'):
                self.compare_symbol(tsym1=left_sym, tsym2=right_sym, strict_cmp=True, tok1=left._token, tok2=right._token)
            else:
                self.compare_symbol(tsym1=left_sym, tsym2=right_sym, strict_cmp=True, tok1=None, tok2=None)
        elif op in (
            psl_builtins.TokenType.PLUS_ASSIGN,
            psl_builtins.TokenType.MINUS_ASSIGN,
            psl_builtins.TokenType.FLOAT_DIV_ASSIGN,
            psl_builtins.TokenType.INTEGER_DIV_ASSIGN,
            psl_builtins.TokenType.POWER_ASSIGN,
            psl_builtins.TokenType.MOD_ASSIGN,
        ):
            cmp_sym = self.compare_symbol(left_sym, right_sym)
            if hasattr(left, '_token') and hasattr(right, '_token'):
                self.compare_symbol(left_sym, cmp_sym, strict_cmp=True, tok1=left._token, tok2=right._token)
            else:
                self.compare_symbol(left_sym, cmp_sym, strict_cmp=True)
    def tvisit_PreOperation(self, node):
        right = node._right
        return self.tvisit(right)
    
    def tvisit_PostOperation(self, node):
        left = node._left
        return self.tvisit(left)

    def tvisit_Procedure(self, node):
        self.tvisit(node._block)
    
    def tvisit_Function(self, node):
        self.tvisit(node._block)
    
    def tvisit_If(self, node):
        self.tvisit(node._condition)
        self.tvisit(node._block)

    def tvisit_ElseIf(self, node):
        self.tvisit(node._condition)
        self.tvisit(node._block)
    
    def tvisit_Else(self, node):
        self.tvisit(node._block)

    def tvisit_CompoundIf(self, node):
        for statement in node.statements:
            self.tvisit(statement)
    
    def tvisit_FunctionCall(self, node):
        func_name = node._name
        func_symbol = self._current_scope.lookup(func_name) 
        if isinstance(func_symbol, ProcedureSymbol):
            proc_symbol = func_symbol
            proc_def_params = proc_symbol._params
            proc_call_params = node._params
            for i, param in enumerate(proc_def_params):
                var_symbol = param
                tsym1 = var_symbol._type
                tok1 = param
                tsym2 = self.tvisit(proc_call_params[i])
                self.compare_symbol(tsym1=tsym1, tsym2=tsym2, strict_cmp=True)
            return self.build_builtintype(psl_builtins.StandardTypes.NULL.value) 
            
        return_type = func_symbol._return_type
        if isinstance(func_symbol, StandardFunction): 
            params = node._params
            if isinstance(return_type, list):
                if len(params) == 2:
                    sym1 = self.tvisit(params[0])
                    sym2 = self.tvisit(params[1])
                    return_type = self.compare_symbol(tsym1=sym1, tsym2=sym2, strict_cmp=False)
                else:
                    num_types_names = [
                        psl_builtins.StandardTypes.INTEGER_CONST.value,
                        psl_builtins.StandardTypes.FLOAT_CONST.value
                    ]
                    num_types = self.build_builtintype(num_types_names)
                    bool_type = BuiltInTypeSymbol(psl_builtins.StandardTypes.BOOLEAN.value)
                    param_types = []
                    for param in params:
                        param_types.append(self.tvisit(param))
                    if bool_type in param_types:
                        return_type = bool_type 
                    if num_types[0] in param_types: 
                        return_type = num_types[0]  
                    if num_types[1] in param_types:
                        return_type = num_types[1] 
                return return_type
            else:
                if params[0]: 
                    for param in params:
                        self.tvisit(param)
                return return_type

        func_def_params = func_symbol._params 
        func_call_params = node._params if node._params and node._params[0] is not None else []
        
        for i, param in enumerate(func_def_params):
            var_symbol = param 
            tsym1 = var_symbol._type
            try:
                tsym2 = self.tvisit(func_call_params[i])
            except:
                err_code = "{}".format(
                    func_name + "()" + " missing " + str(len(func_def_params)) + " positional argument: " +
                    repr(func_def_params[len(func_call_params)-1]._name))
                raise psl_exception.TypeError(err_code=err_code, token=None, msg=err_code)
            self.compare_symbol(tsym1=tsym1, tsym2=tsym2, strict_cmp=True, param_check=True)
        return return_type

    def tvisit_ProcedureCall(self, node):
        proc_symbol = self._current_scope.lookup(node._name)
        proc_def_params = proc_symbol._params
        proc_call_params = node._params
        for i, param in enumerate(proc_def_params):
            var_symbol = param
            tsym1 = var_symbol._type
            tsym2 = self.tvisit(proc_call_params[i])
            self.compare_symbol(tsym1=tsym1, tsym2=tsym2, strict_cmp=True)
        return self.build_builtintype(psl_builtins.StandardTypes.NULL.value) 
            

    def tvisit_LoopControl(self, node):
        pass
    
    def tvisit_NoOp(self, node):
        pass
    
    def tvisit_WhileLoop(self, node):
        condition = node._condition
        block = node._block
        self.tvisit(condition)
        self.tvisit(block)
    
    def tvisit_ForLoop(self, node):
        initial_condition = node._initial_condition
        stop_condition = node._stop_condition
        loop_condition = node._loop_condition
        block = node._block
        self.tvisit(initial_condition) 
        self.tvisit(block)

    def tvisit_RepeatUntilLoop(self, node):
        self.tvisit(node._condition)
        self.tvisit(node._block)

    def tvisit_FromLoop(self, node):
        block = node._block
        self.tvisit(block)

    def tvisit_Return(self, node):
        var_node = node._func_name
        func_name = var_node._value
        func_symbol = self._current_scope.lookup(func_name)
        func_ret_type = func_symbol._return_type if func_symbol._return_type else None
        node_ret_type = self.tvisit(node._value)
        tok1 = var_node._token
        tok2 = node._token
        self.compare_symbol(func_ret_type, node_ret_type, True, tok1, tok2)

    def tvisit_Pass(self, node):
        pass

    def tvisit_Compound(self, node):
        for node_ in node.children:
            self.tvisit(node_)

    def tvisit_Block(self, node):
        decl_node = node._decls_node
        for node_ in decl_node:
            self.tvisit(node_)
        comp_node = node._comp_node
        self.tvisit(comp_node)

    def tvisit_Program(self, node):
        self.tvisit(node._block_node)


class ScopedMemorySpace(object):
    """
    Virtual memory space for a scope (function, procedure, global scope)
    """
    def __init__(self, scope_name, scope_level, enclosing_mem_space=None, current_scope_only=False):
        self._memory_space = {}
        self._type_decls = {}
        self._scope_name = scope_name
        self._scope_level = scope_level
        self._enclosing_mem_space = enclosing_mem_space
        self._current_scope_only = current_scope_only 
    
    def __str__(self):
        header = self._scope_name
        mem_space = ['\n', header, '-'*len(header)]
        for key, value in self._memory_space.items():
            mem_space.append("%7s: %s"%(key, str(value)))
        mem_space = '\n'.join(mem_space)
        return mem_space
    
    __repr__ = __str__

    def insert(self, name, value):
        """
        Insert value into the mem space
        """
        self._memory_space[name] = value

    def store_var_type(self, name, value):
        """
        Stores the type of a variable in _type_decls dict
        """
        self._type_decls[name] = value
        
    def update_vals(self):
        for k, v in self._type_decls.items():
            val = self._memory_space.get(k)
            if val is not None:
                if v == psl_builtins.TokenType.INTEGER_CONST.value:
                    self.insert(k, int(val))
                elif v == psl_builtins.TokenType.FLOAT_CONST.value:
                    self.insert(k, float(val))
                elif v == psl_builtins.TokenType.BOOLEAN.value:
                    val = self.retrieve(k)
                    if val == True:
                        self.insert(k, 1)
                    elif val == False:
                        self.insert(k, 0)
                
    def retrieve(self, name):
        """
        checks the current mem space for the value
        """
        val = self._memory_space.get(name)
        if self._current_scope_only: 
            return val
        if val is None:
            return self._enclosing_mem_space.retrieve(name)
        return val
    
    def copy(self, memspace):
        """
        copy the prev mem space into the new mem space
        """
        self._memory_space = memspace._memory_space.copy()
    
    def purge(self):
        if psl_builtins.TokenType.CONTROL.value in self._memory_space.keys():
            del self._memory_space[psl_builtins.TokenType.CONTROL.value]

