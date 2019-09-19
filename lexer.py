import pprint
import utility
import psl_builtins
import psl_exception


def get_reserved_keywords():
    token_types = list(psl_builtins.TokenType)
    start_ind = token_types.index(psl_builtins.TokenType.BEGIN)
    end_ind = token_types.index(psl_builtins.TokenType.END)
    reserved = {
        token.value : Token(token, token.value) for token in token_types[start_ind:end_ind+1]
    }
    return reserved

class Token(object):

    def __init__(self, type, value, linenum=None, colnum=None):
        self._type = type
        self._value = value
        self._linenum = linenum
        self._colnum = colnum
    
    def __str__(self):
        """
        Token(integer_const, 17, 40:3)
        """
        return self.__class__.__name__ + "({type}, '{value}', position {linenum}:{colnum})".format(
            type=self._type, value=self._value, linenum=self._linenum, colnum=self._colnum,
            )
    
    def __repr__(self):
        return self.__str__()
    
 
RESERVED_KEYWORDS =    get_reserved_keywords()


class Lexer(object):
    def __init__(self, text):
        self._text = text
        self._pos = 0
        self._current_char = self._text[self._pos]
        self._linenum = 1
        self._colnum = 1
    
    def error(self):
        msg = " -> '{lexeme}' {linenum}:{colnum}".format(
            lexeme=self._current_char, linenum=self._linenum, colnum=self._colnum,
        )
        raise psl_exception.SyntaxError(msg=msg)
    
    def advance(self):
        if self._current_char == '\n':
            self._linenum += 1
            self._colnum = 0
        self._pos += 1
        if self._pos > len(self._text) - 1:
            self._current_char = None
            return Token(psl_builtins.TokenType.EOF, None)
        else:
            self._current_char = self._text[self._pos]
            self._colnum += 1

    
    def skip_whitespace(self):
        while self._current_char is not None and self._current_char.isspace():
            self.advance()
    
    def arbitrary_skip_whitespace(self):
        i = 0
        while self.look_ahead(i) is not None and self.look_ahead(i).isspace() :
            i += 1
            continue
        return i

    def skip_comment(self):
        if self._current_char == '/' and self.peek() == '/':
            while self._current_char is not None and self._current_char != '\n':
                self.advance()
        else:
            while self._current_char is not None:
                self.advance()
                if self._current_char == '}':
                    break
                elif self._current_char == '*' and self.look_ahead(1) == '>':
                    break
            self.advance()
            self.advance()
        self.skip_whitespace()
    
    def _id(self):
        id_ = str()
        while self._current_char is not None and (self._current_char.isalnum() or self._current_char == '_'):
            id_ += self._current_char
            self.advance()
        return RESERVED_KEYWORDS.get(id_, Token(psl_builtins.TokenType.ID, id_, self._linenum, self._colnum)) #default
    
    def peek(self):
        pos = self._pos
        pos += 1
        if pos > len(self._text) - 1:
            #self._current_char = None
            return Token(psl_builtins.TokenType.EOF, None)
        else:
            return self._text[pos]
    
    def look_ahead(self, step):
        pos = self._pos
        pos += step
        if pos > len(self._text) - 1:
            return None
        else:
            return self._text[pos]

    def pack_int(self):
        ints = ''
        while self._current_char is not None and self._current_char.isdigit():
            ints += self._current_char
            self.advance()
        if self._current_char == '.':
            ints += self._current_char
            self.advance() 
        while self._current_char is not None and self._current_char.isdigit():
            ints += self._current_char
            self.advance()  
        if '.' in ints:
            return Token(psl_builtins.TokenType.FLOAT_CONST, float(ints), self._linenum, self._colnum)
        else:
            return Token(psl_builtins.TokenType.INTEGER_CONST, int(ints), self._linenum, self._colnum)


    def pack_string(self):
        str_ = ''
        while self._current_char is not None and self._current_char != '"':
            str_ += self._current_char
            self.advance()
        self.advance()
        return str_
    
    def get_next_token(self):
        """Lexer engine, and Token generator"""
        while self._current_char is not None:

            if self._current_char is not None and self._current_char.isspace():
                self.skip_whitespace()
                continue
            if self._current_char is not None and self._current_char.isdigit():
               
                token = self.pack_int()

                step_1 = self.look_ahead(0) 
                step_2 = self.look_ahead(1) 
                step_3 = self.look_ahead(2) 

               
                if step_1 in ('+', '-') and step_2 in ('+', '-') and step_3 in (' ', ';', '\n', ')'):
                   
                    if step_1 == '+' and step_2 == '+':
                        self.advance()
                        self.advance()
                        
                        return Token(psl_builtins.TokenType.POST_PLUS_PLUS, token, self._linenum, self._colnum)

                    
                    elif step_1 == '-' and step_2 == '-':
                        self.advance()
                        self.advance()
                        return Token(psl_builtins.TokenType.POST_MINUS_MINUS, token, self._linenum, self._colnum)
                else:
                    return token
                
            if self._current_char is not None and (self._current_char.isalnum() or self._current_char == '_'):
                
                token = self._id(); 
                
                
                if token._type == psl_builtins.TokenType.COMM:
                    #skip comment
                    if self.look_ahead(0) == ':' and self.look_ahead(1) == '<' and self.look_ahead(2) == '*':
                        self.skip_comment()
                        continue

                if token._type == psl_builtins.TokenType.ELSE:
                    ind = self.arbitrary_skip_whitespace()
                    if self._text[self._pos+ind] == 'i' and self._text[self._pos+ind+1] == 'f':
                        self.skip_whitespace()
                        if_token = self._id() 
                        token_type = psl_builtins.TokenType.ELSE_IF
                        return Token(token_type, token_type.value, self._linenum, self._colnum)

                
                step_1 = self.look_ahead(0) 
                step_2 = self.look_ahead(1) 
                step_3 = self.look_ahead(2) 

                
                if step_1 in ('+', '-') and step_2 in ('+', '-') and step_3 in (' ', ';', '\n', ')'): 
                   
                   
                    if step_1 == '+' and step_2 == '+':
                        self.advance()
                        self.advance()
                        id_token = token
                        return Token(psl_builtins.TokenType.POST_PLUS_PLUS, id_token, self._linenum, self._colnum)

                    
                    elif step_1 == '-' and step_2 == '-':
                        self.advance()
                        self.advance()
                        id_token = token
                        return Token(psl_builtins.TokenType.POST_MINUS_MINUS, id_token, self._linenum, self._colnum)
                
                else:
                    return token
            
           
            if self._current_char is not None and self._current_char == '+':
                
                unk1 = self.look_ahead(1) 
                unk2 = self.look_ahead(2)
                prev_char = self.look_ahead(-1) 
                

                if  unk1 == '+' and unk2.isdigit() and (prev_char == ' ' or prev_char == '('):
                    self.advance()
                    self.advance() #a++ + 4; ++a + 4
                    num_token = self.pack_int()
                    
                    return Token(psl_builtins.TokenType.PRE_PLUS_PLUS, num_token, self._linenum, self._colnum)
                
                if  unk1 == '+' and unk2.isalnum() and (prev_char == ' ' or prev_char == '('):
                    self.advance()
                    self.advance() #a++ + 4; ++a + 4
                    id_token = self._id()
                    
                    return Token(psl_builtins.TokenType.PRE_PLUS_PLUS, id_token, self._linenum, self._colnum)
                
                elif unk1 == ':' and unk2 == '=':
                    self.advance()
                    self.advance()
                    self.advance()
                    token_type = psl_builtins.TokenType.PLUS_ASSIGN
                    
                    return Token(token_type, token_type.value, self._linenum, self._colnum)
            
            
            if self._current_char is not None and self._current_char == '-':
                unk1 = self.look_ahead(1)
                unk2 = self.look_ahead(2)
                prev_char = self.look_ahead(-1)
                
                
                if unk1 == '-' and unk2.isdigit() and (prev_char == ' ' or prev_char == '('):
                    self.advance()
                    self.advance()
                    num_token = self.pack_int()
                    
                    return Token(psl_builtins.TokenType.PRE_MINUS_MINUS, num_token, self._linenum, self._colnum)
                
                
                if unk1 == '-' and unk2.isalnum() and (prev_char == ' ' or prev_char == '('):
                    self.advance()
                    self.advance()
                    num_token = self._id()
                    
                    return Token(psl_builtins.TokenType.PRE_MINUS_MINUS, num_token, self._linenum, self._colnum)

                
                elif unk1 == ':' and unk2 == '=':
                    self.advance()
                    self.advance()
                    self.advance()
                    token_type = psl_builtins.TokenType.MINUS_ASSIGN
                    return Token(token_type, token_type.value, self._linenum, self._colnum)

            if self._current_char is not None and self._current_char == '*':
                unk1 = self.look_ahead(1)
                unk2 = self.look_ahead(2)
                unk3 = self.look_ahead(3)
                unk4 = self.look_ahead(4)
                
                if unk1 == '*' and unk2 == ':' and unk3 == '=':
                    self.advance()
                    self.advance()
                    self.advance()
                    self.advance()
                    token_type = psl_builtins.TokenType.POWER_ASSIGN
                    return Token(token_type, token_type.value, self._linenum, self._colnum)
                
                if unk1 == '*':
                    self.advance()
                    self.advance()
                    token_type = psl_builtins.TokenType.POWER
                    return Token(token_type, token_type.value, self._linenum, self._colnum) 
                
                if unk1 == ':' and unk2 == '=':
                    self.advance()
                    self.advance()
                    self.advance()
                    token_type = psl_builtins.TokenType.MULT_ASSIGN
                    return Token(token_type, token_type.value, self._linenum, self._colnum)

            
            if self._current_char is not None and self._current_char == '/' and self.peek() == '/':
                self.skip_comment()
                continue 

            if self._current_char is not None and self._current_char == '/':
                
                if self.peek() == ':' and self.look_ahead(2) == '=':
                        self.advance()
                        self.advance()
                        self.advance()
                        token_type = psl_builtins.TokenType.FLOAT_DIV_ASSIGN
                        return Token(token_type, token_type.value, self._linenum, self._colnum)
            
            if self._current_char is not None and self._current_char == '%':
                
                if self.peek() == ':' and self.look_ahead(2) == '=':
                        self.advance()
                        self.advance()
                        self.advance()
                        token_type = psl_builtins.TokenType.MOD_ASSIGN
                        return Token(token_type, token_type.value, self._linenum, self._colnum)

            if self._current_char is not None and self._current_char == '.':
                
                unk = self.look_ahead(1)
                if unk is not None and unk.isdigit(): 
                    token = self.pack_int()
                    return token

           
            if self._current_char is not None and self._current_char == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                token_type = psl_builtins.TokenType.ASSIGN
                return Token(token_type, token_type.value, self._linenum, self._colnum)
            
            
            if self._current_char is not None and self._current_char == '{':
                self.skip_comment()
                continue

           
            if self._current_char is not None and self._current_char == '$' and self.peek() == '<' and self.look_ahead(2) == '*':
                self.skip_comment()
                continue

            
            if self._current_char is not None and self._current_char == '<':
                token_type = psl_builtins.TokenType.LT
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    token_type_ = psl_builtins.TokenType.LE
                    return Token(type=token_type_, value=token_type_.value, linenum=self._linenum, colnum=self._colnum)
                else:
                    self.advance()
                    return Token(token_type, token_type.value, self._linenum, self._colnum)

            
            if self._current_char is not None and self._current_char == '>':
                token_type = psl_builtins.TokenType.GT
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    token_type_ = psl_builtins.TokenType.GE
                    return Token(type=token_type_, value=token_type_.value, linenum=self._linenum, colnum=self._colnum)
                else:
                    self.advance()
                    return Token(token_type, token_type.value, self._linenum, self._colnum)

           
            if self._current_char is not None and self._current_char == '=':
                self.advance()
                token_type = psl_builtins.TokenType.EQ
                return Token(token_type, token_type.value, self._linenum, self._colnum)

           
            if self._current_char is not None and self._current_char == '!':
                token_type = psl_builtins.TokenType.NE
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    token_type_ = psl_builtins.TokenType.NE
                    return Token(type=token_type_, value=token_type_.value, linenum=self._linenum, colnum=self._colnum)
                else:
                    self.advance()
                    return Token(token_type, token_type.value, self._linenum, self._colnum)
            
            if self._current_char is not None and self._current_char == psl_builtins.TokenType.APOS.value:
                self.advance()
                char = self._current_char
                self.advance() #advance from char
                self.advance() #advance from apos
                return Token(psl_builtins.TokenType.CHAR_CONST, char, self._linenum, self._colnum)
            
            if self._current_char is not None and self._current_char == psl_builtins.TokenType.DAPOS.value:
                self.advance()
                str_ = self.pack_string()
                return Token(psl_builtins.TokenType.STRING_CONST, str_, self._linenum, self._colnum)
            
            
            if self._current_char is None:
                #return EOF--end of file 
                token_type = psl_builtins.TokenType.EOF
                return Token(token_type, token_type.value, self._linenum, self._colnum)

            try:
                
                token_type = psl_builtins.TokenType(self._current_char)
                self.advance()
                token = Token(token_type, token_type.value, self._linenum, self._colnum)
                return token
            except Exception:
                self.error()


