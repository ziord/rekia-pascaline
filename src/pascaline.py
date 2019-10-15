import sys
from interpreter import *

   
if __name__ == '__main__':
    if len(sys.argv) >= 2:
        logger = utility.create_logger()
        file = logger.get_file()
        text = utility.reader(file)
        lexer = Lexer(text)
        parser = Parser(lexer)
        interpeter = Interpreter(parser, logger)
        interpeter.interpret()