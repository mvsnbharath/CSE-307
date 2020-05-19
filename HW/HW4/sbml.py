# VenkataSubbaNarasaBharathMeadam
# 112672986

import ply.lex as lex
import ply.yacc as yacc
import sys
import traceback

tokenizeDebug = 0
rulesDebug = 0
parseDebug = 0
stackTrace = 1


# AST Nodes
class Node:
    def __init__(self):
        self.value = -float("inf")

    def eval(self):
        return self.value


# Numbers: (Integers and Floats)
class Integer(Node):
    def __init__(self, value):
        super().__init__()
        self.value = int(value)

    def eval(self):
        try:
            return self.value
        except Exception:
            raise Exception


class Float(Node):
    def __init__(self, value):
        super().__init__()
        self.value = float(value)

    def eval(self):
        try:
            return self.value
        except Exception:
            raise Exception


# Strings
class String(Node):
    def __init__(self, value):
        super().__init__()
        if (value[0] == "\'" and value[-1] == "\'") or (value[0] == "\"" and value[-1] == "\""):
            self.value = value[1:-1]
        else:
            self.value = value

    def eval(self):
        try:
            return self.value
        except Exception:
            raise Exception


# Boolean
class Boolean(Node):
    def __init__(self, value):
        super().__init__()
        if value == 'True':
            self.value = True
        else:
            self.value = False

    def eval(self):
        try:
            return self.value
        except Exception:
            raise Exception


# Variable
class Variable(Node):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def eval(self):
        return global_lookup_dic[self.value]


# VarAssignment
class VarNode(Node):
    def __init__(self, name, value, index=None):
        super().__init__()
        self.name = name.value
        self.value = value
        self.index = index

    def eval(self):
        index = self.index
        if index is not None:
            index = index.eval()
        updateDictionary(self.name, self.value, index)


# Helper Functions for dictionary update
def updateDictionary(key, value, index=None):
    if index is None:
        global_lookup_dic[key] = value.eval()
    else:
        if type(key) == Node:
            key.eval()[index] = value.eval()
        else:
            global_lookup_dic[key][index] = value.eval()


# List
class List(Node):
    def __init__(self, value):
        super().__init__()
        if value is None:
            self.value = []
        else:
            self.value = [value]

    def appendToMyList(self, v):
        self.value.append(v)
        return self

    def eval(self):
        return [x.eval() for x in self.value]


class Indexing(Node):
    def __init__(self, objectToBeIndexed, index):
        super().__init__()
        self.objectToBeIndexed = objectToBeIndexed
        self.index = index

    def eval(self):
        index = self.index.eval()
        objectToBeIndexed = self.objectToBeIndexed

        try:
            if isinstance(objectToBeIndexed, Node):
                objectToBeIndexed = objectToBeIndexed.eval()
            return objectToBeIndexed[index]
        except IndexError:
            raise IndexError


class Searching(Node):
    def __init__(self, a, b):
        super().__init__()
        self.a = a
        self.b = b

    def eval(self):
        try:
            a = self.a.eval()
            b = self.b.eval()
            return a in b
        except TypeError:
            raise TypeError
        except Exception:
            raise Exception


class Concatenation(Node):
    def __init__(self, a, b):
        super().__init__()
        self.a = a
        self.b = b

    def eval(self):
        try:
            b = self.b.eval()
            a = self.a.eval()
            b.insert(0, a)
            return b
        except Exception:
            raise Exception

# Tuple
class Tuple(Node):
    def __init__(self, a):
        super().__init__()
        self.value = tuple(a)

    def eval(self):
        try:
            # Objects inside temp can be of different types
            return tuple([temp.eval() for temp in self.value])
        except Exception:
            raise Exception


class Exponentiation(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def eval(self):
        try:
            return self.left.eval() ** self.right.eval()
        except Exception:
            raise Exception


class Addition(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.both_strings = False

    def typeCheck(self):
        lType = type(self.left.eval())
        rType = type(self.right.eval())
        # Both strings
        if lType == str and rType == str:
            self.both_strings = True
            return

    def eval(self):
        self.typeCheck()
        try:
            if self.both_strings:
                word = self.left.eval().strip('(\")|(\')') + self.right.eval().strip('(\")|(\')')
                return String(word).eval()
            else:
                return self.left.eval() + self.right.eval()
        except Exception:
            raise Exception


class Subtraction(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def eval(self):
        try:
            return self.left.eval() - self.right.eval()
        except TypeError:
            raise TypeError


class Multiplication(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def eval(self):
        try:
            return self.left.eval() * self.right.eval()
        except TypeError:
            raise TypeError


class Division(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def eval(self):
        try:
            return self.left.eval() / self.right.eval()
        except TypeError:
            raise TypeError
        except ZeroDivisionError:
            raise TypeError


class Div(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def eval(self):
        try:
            return self.left.eval() // self.right.eval()
        except TypeError:
            raise TypeError
        except ZeroDivisionError:
            raise TypeError


class Mod(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def eval(self):
        try:
            return self.left.eval() % self.right.eval()
        except TypeError:
            raise TypeError
        except ZeroDivisionError:
            raise TypeError


# Boolean Operators
class Negation(Node):
    def __init__(self, child):
        super().__init__()
        self.child = child
        self.child.parent = self

    def typeCheck(self):
        # Boolean
        if type(self.child.eval()) == bool:
            return
        else:
            raise TypeError

    def eval(self):
        self.typeCheck()
        try:
            return not self.child.eval()
        except Exception:
            raise Exception


class Conjunction(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def typeCheck(self):
        lType = type(self.left.eval())
        rType = type(self.right.eval())

        # Both Boolean
        if lType == bool and rType == bool:
            return
        else:
            raise TypeError

    def eval(self):
        self.typeCheck()
        try:
            return self.left.eval() and self.right.eval()
        except Exception:
            raise Exception


class Disjunction(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def typeCheck(self):
        lType = type(self.left.eval())
        rType = type(self.right.eval())

        # Both Boolean
        if lType == bool and rType == bool:
            return
        else:
            raise TypeError

    def eval(self):
        self.typeCheck()
        try:
            return self.left.eval() or self.right.eval()
        except Exception:
            raise Exception


class In(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def eval(self):
        try:
            return self.left.eval() in self.right.eval()
        except Exception:
            raise Exception


# Comparison Operators
class Comparisons(Node):
    def __init__(self, left, right, operator):
        super().__init__()
        self.left = left
        self.right = right
        self.operator = operator

    def typeCheck(self):
        lType = type(self.left.eval())
        rType = type(self.right.eval())
        # Both strings

        if lType == str and rType == str:
            return
        # Both Numbers
        elif (lType == int or lType == float) and (rType == int or rType == float):
            return
        else:
            raise TypeError

    def eval(self):
        self.typeCheck()
        try:
            if self.operator == '<':
                return self.left.eval() < self.right.eval()
            elif self.operator == '<=':
                return self.left.eval() <= self.right.eval()
            elif self.operator == '==':
                return self.left.eval() == self.right.eval()
            elif self.operator == '<>':
                return self.left.eval() != self.right.eval()
            elif self.operator == '>=':
                return self.left.eval() >= self.right.eval()
            elif self.operator == '>':
                return self.left.eval() > self.right.eval()
        except Exception:
            raise Exception


class While(Node):
    def __init__(self, condition, block):
        super().__init__()
        self.condition = condition
        self.block = block

    def eval(self):
        while self.condition.eval():
            self.block.eval()


class IfElse(Node):
    def __init__(self, condition, if_block, else_block):
        super().__init__()
        self.condition = condition
        self.if_block = if_block
        self.else_block = else_block

    def eval(self):
        condition = self.condition.eval()

        if condition:
            self.if_block.eval()
        elif self.else_block is not None:
            self.else_block.eval()
        else:
            pass


class PrintNode(Node):
    def __init__(self, v):
        super().__init__()
        self.v = v

    def eval(self):
        print(self.v.eval())


class BlockNode(Node):
    def __init__(self, v):
        super().__init__()
        self.value = v

    def eval(self):
        for statement in self.value:
            statement.eval()


# Todo: Add reserved words separately or create a separate list
# Todo: change the order
# Todo: True/False here or above
# Reserved words
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'print': 'PRINT',
    'in': 'IN',
    'div': 'DIV',
    'mod': 'MOD',
    'not': 'NEGATION',
    'andalso': 'CONJUNCTION',
    'orelse': 'DISJUNCTION'
}

# Tokens
# TODO: See we have to define Booleans or True,False as Tokens,similar for others
tokens = ['INTEGER',
          'REAL',
          'BOOLEAN',
          'STRINGS',
          'VARIABLE',
          'LIST_LEFT_PARENTHESIS',
          'LIST_RIGHT_PARENTHESIS',
          'LEFT_CURLY_PARENTHESIS',
          'RIGHT_CURLY_PARENTHESIS',
          'LEFT_PARENTHESIS',
          'RIGHT_PARENTHESIS',
          'EXPONENTIATION',
          'MULTIPLICATION',
          'DIVISION',
          'PLUS',
          'MINUS',
          'CONCATENATION',
          'LESSTHAN',
          'LESSTHANEQUALTO',
          'ASSIGNMENT',
          'EQUALS',
          'NOTEQUALS',
          'GREATERTHANEQUALTO',
          'GREATERTHAN',
          'COMMA',
          'POUND',
          'SEMICOLON'
          ]

tokens = tokens + list(reserved.values())

t_LIST_LEFT_PARENTHESIS = r'\['
t_LIST_RIGHT_PARENTHESIS = r'\]'
t_LEFT_PARENTHESIS = r'\('
t_RIGHT_PARENTHESIS = r'\)'
t_LEFT_CURLY_PARENTHESIS = r'\{'
t_RIGHT_CURLY_PARENTHESIS = r'\}'
t_EXPONENTIATION = r'\*\*'
t_MULTIPLICATION = r'\*'
t_DIVISION = r'\/'
t_PLUS = r'\+'
t_MINUS = r'-'
t_CONCATENATION = r'::'
t_LESSTHAN = r'<'
t_LESSTHANEQUALTO = r'<='
t_ASSIGNMENT = '='
t_EQUALS = r'=='
t_NOTEQUALS = r'<>'
t_GREATERTHANEQUALTO = r'>='
t_GREATERTHAN = r'>'
t_COMMA = r','
t_POUND = r'\#'
t_SEMICOLON = r'\;'

# Ignore whitespace
t_ignore = ' \t'

global_lookup_dic = {}


def t_BOOLEAN(t):
    r'(True)|(False)'
    t.value = Boolean(t.value)
    return t


def t_VARIABLE(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    # Check for reserved words
    t.type = reserved.get(t.value, 'VARIABLE')
    if t.type is 'VARIABLE':
        t.value = Variable(t.value)
    return t


def t_REAL(t):
    r'(([0-9]*\.[0-9]*)|(\.[0-9]+))([Ee][+-]?[0-9]+)?'
    t.value = Float(t.value)
    return t


def t_INTEGER(t):
    r'[0-9][0-9]*'
    t.value = Integer(t.value)
    return t


def t_STRINGS(t):
    r'\'[^\']*\'|\"[^\"]*\"'
    t.value = String(t.value)
    return t


# Count newlines
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


# Report lexing errors
def t_error(t):
    raise SyntaxError


precedence = (('left', 'DISJUNCTION'),
              ('left', 'CONJUNCTION'),
              ('left', 'NEGATION'),
              ('left', 'LESSTHAN', 'LESSTHANEQUALTO', 'EQUALS', 'NOTEQUALS', 'GREATERTHANEQUALTO', 'GREATERTHAN'),
              ('right', 'CONCATENATION'),
              ('left', 'IN'),
              ('left', 'PLUS', 'MINUS'),
              ('left', 'MULTIPLICATION', 'DIVISION', 'DIV', 'MOD'),
              ('nonassoc', 'UMINUS'),
              ('right', 'EXPONENTIATION'),
              ('left', 'LEFT_CURLY_PARENTHESIS', 'RIGHT_CURLY_PARENTHESIS'),
              ('left', 'LIST_LEFT_PARENTHESIS', 'LIST_RIGHT_PARENTHESIS'),
              ('left', 'POUND'),
              ('left', 'LEFT_PARENTHESIS', 'RIGHT_PARENTHESIS')
              )


def p_block(t):
    """ block : LEFT_CURLY_PARENTHESIS RIGHT_CURLY_PARENTHESIS """
    t[0] = BlockNode([])


def p_block_2(t):
    """ block : LEFT_CURLY_PARENTHESIS statement_list RIGHT_CURLY_PARENTHESIS """
    t[0] = BlockNode(t[2])


def p_block_3(t):
    """ statement : block"""
    t[0] = t[1]


def p_statement_list_1(t):
    """ statement_list : statement """
    t[0] = [t[1]]


def p_statement_list_2(t):
    """ statement_list : statement_list statement"""
    t[0] = t[1] + [t[2]]


def p_print_statement(t):
    """statement : PRINT LEFT_PARENTHESIS expr RIGHT_PARENTHESIS SEMICOLON """
    t[0] = PrintNode(t[3])


def p_if_statement(t):
    """statement : IF LEFT_PARENTHESIS expr RIGHT_PARENTHESIS block"""
    t[0] = IfElse(t[3], t[5], None)


def p_if_else_statement(t):
    """
    statement : IF LEFT_PARENTHESIS expr RIGHT_PARENTHESIS block ELSE block
    """
    t[0] = IfElse(t[3], t[5], t[7])


def p_while_statement(t):
    """statement : WHILE LEFT_PARENTHESIS expr RIGHT_PARENTHESIS block"""
    t[0] = While(t[3], t[5])


# Assignment
def p_statement_assignment(t):
    """statement : VARIABLE ASSIGNMENT expr SEMICOLON"""
    t[0] = VarNode(t[1], t[3])


def p_statement_assignment_2(t):
    """ statement : expr  LIST_LEFT_PARENTHESIS expr  LIST_RIGHT_PARENTHESIS ASSIGNMENT expr SEMICOLON """
    t[0] = VarNode(t[1], t[6], t[3])


def p_expr(t):
    """expr : INTEGER
            | REAL
            | STRINGS
            | VARIABLE
            | BOOLEAN
            | list
            | tuple
            | index"""
    t[0] = t[1]


def p_parentheses_expr(t):
    """tuple : LEFT_PARENTHESIS expr RIGHT_PARENTHESIS"""
    t[0] = t[2]


# List
def p_empty_list(t):
    """expr : LIST_LEFT_PARENTHESIS LIST_RIGHT_PARENTHESIS"""
    t[0] = List(None)


def p_expr_list(t):
    """list : LIST_LEFT_PARENTHESIS args LIST_RIGHT_PARENTHESIS"""
    t[0] = t[2]


def p_args(t):
    """ args : args COMMA expr"""
    t[1].appendToMyList(t[3])
    t[0] = t[1]


def p_args_2(t):
    """ args : expr"""
    t[0] = List(t[1])


def p_expr_list_index(t):
    'index : expr LIST_LEFT_PARENTHESIS expr LIST_RIGHT_PARENTHESIS'
    t[0] = Indexing(t[1], t[3])


# Tuple
def p_tuple(t):
    """ tuple : LEFT_PARENTHESIS args RIGHT_PARENTHESIS"""
    t[0] = Tuple(t[2].value)


def p_expr_tuple_index(t):
    """index : POUND expr LEFT_PARENTHESIS expr RIGHT_PARENTHESIS"""
    t[0] = Indexing(t[4], t[2])


def p_expr_tuple_index_2(t):
    """index :  POUND expr expr"""
    t[0] = Indexing(t[3], t[2])


def p_searching(t):
    """expr : expr IN expr"""
    t[0] = Searching(t[1], t[3])


def p_expr_concatenation(p):
    """ expr : expr CONCATENATION expr"""
    p[0] = Concatenation(p[1], p[3])


# Arithmetic Operations
def p_expr_addition(p):
    """ expr : expr PLUS expr"""
    p[0] = Addition(p[1], p[3])


def p_expr_subtraction(p):
    """ expr : expr MINUS expr"""
    p[0] = Subtraction(p[1], p[3])


def p_expr2uminus(p):
    """ expr : MINUS expr %prec UMINUS """
    p[0] = Integer(-1 * p[2].eval())


def p_expr_multiplication(p):
    """  expr : expr MULTIPLICATION expr """
    p[0] = Multiplication(p[1], p[3])


def p_expr_division(p):
    """  expr : expr DIVISION expr """
    p[0] = Division(p[1], p[3])


def p_expr_div(p):
    """  expr : expr DIV expr """
    p[0] = Div(p[1], p[3])


def p_expr_mod(p):
    """  expr : expr MOD expr """
    p[0] = Mod(p[1], p[3])


def p_expr_exponentiation(p):
    """  expr : expr EXPONENTIATION expr """
    p[0] = Exponentiation(p[1], p[3])


# Boolean Operations
def p_expr_negation(p):
    """  expr : NEGATION expr """
    p[0] = Negation(p[2])


def p_expr_conjunction(p):
    """  expr : expr CONJUNCTION expr """
    p[0] = Conjunction(p[1], p[3])


def p_expr_disjunction(p):
    """ expr : expr DISJUNCTION expr """
    p[0] = Disjunction(p[1], p[3])


# Comparisons
def p_expr_less_than(p):
    """expr : expr LESSTHAN expr
            | expr LESSTHANEQUALTO expr
            | expr EQUALS expr
            | expr NOTEQUALS expr
            | expr GREATERTHANEQUALTO expr
            | expr GREATERTHAN expr """
    p[0] = Comparisons(p[1], p[3], p[2])


def p_error(t):
    raise SyntaxError


def printStackTrace():
    if stackTrace:
        print(traceback.format_exc())


# Build lexer
lexer = lex.lex(debug=tokenizeDebug)
# Build parser
parser = yacc.yacc(debug=parseDebug)


def tokenize(inp):
    lexer.input(inp)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)


def parse(inp):
    result = parser.parse(inp, debug=rulesDebug)
    return result


def evaluate(inp):
    try:
        result = parse(inp)
        if result is not None:
            temp = result.eval()
            if type(temp) is not type(None):
                if (type(temp)) == str:
                    print('\'' + temp + '\'')
                else:
                    print(temp)
    except SyntaxError:
        print("SYNTAX ERROR")
        printStackTrace()
    except TypeError:
        print("SEMANTIC ERROR")
        printStackTrace()
    except IndexError:
        print("SEMANTIC ERROR")
        printStackTrace()
    except Exception:
        print("SEMANTIC ERROR")
        printStackTrace()


def main():
    fd = open(sys.argv[1], 'r')
    code = fd.read().replace('\n', '')
    # tokenize(code)
    evaluate(code)
    fd.close()


if __name__ == "__main__":
    main()
