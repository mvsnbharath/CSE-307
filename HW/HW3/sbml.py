import ply.lex as lex
import ply.yacc as yacc
import sys


# AST Nodes
class Node:
    def __init__(self):
        self.parent = None

    def parentCount(self):
        count = 0
        current = self.parent
        while current is not None:
            count += 1
            current = current.parent
        return count


# Numbers: (Integers and Floats)
class Integer(Node):
    def __init__(self, num):
        super().__init__()
        self.value = int(num)

    def eval(self):
        try:
            return self.value
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "Integer: " + str(self.value)
        return res


class Float(Node):
    def __init__(self, num):
        super().__init__()
        self.value = float(num)

    def eval(self):
        try:
            return self.value
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "Real: " + str(self.value)
        return res


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

    def __str__(self):
        res = "\t" * self.parentCount() + "Boolean: " + str(self.value)
        return res


# Strings
class String(Node):
    def __init__(self, string):
        super().__init__()
        if (string[0] == "\'" and string[-1] == "\'") or (string[0] == "\"" and string[-1] == "\""):
            self.value = string[1:-1]
        else:
            self.value = string

    def eval(self):
        try:
            return "\'" + self.value + "\'"
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "String: " + self.value
        return res


# List
class List(Node):
    def __init__(self, value):
        super().__init__()
        if value is None:
            self.value = []
        elif type(value) == list:
            self.value = value
        else:
            self.value = [value.eval()]

    def eval(self):
        try:
            return self.value
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "List: " + str(self.value)
        return res


class AppendValueToList(Node):
    def __init__(self, listToAppend, value):
        super().__init__()
        self.listToAppend = listToAppend.eval()
        self.value = value.eval()

    def eval(self):
        try:
            self.listToAppend.append(self.value)
            return self.listToAppend
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "List: " + str(self.listToAppend)
        return res


class Indexing(Node):
    def __init__(self, operand, index):
        super().__init__()
        self.isOperandString = False
        if type(operand.eval()) == str:
            self.operand = operand.eval().strip('\'')
            self.isOperandString = True
        else:
            self.operand = operand.eval()
        self.index = index

    def eval(self):
        try:
            if self.isOperandString:
                return String(self.operand[self.index]).eval()
            else:
                return self.operand[self.index]
        except IndexError:
            raise IndexError
        except Exception:
            raise Exception


class Searching(Node):
    def __init__(self, a, b):
        super().__init__()
        self.a = a.eval()
        self.b = b.eval()

        if type(a.eval()) == str:
            self.a = self.a.strip('\'')
        if type(b.eval()) == str:
            self.b = self.b.strip('\'')

    def eval(self):
        try:
            return self.a in self.b
        except TypeError:
            raise TypeError
        except Exception:
            raise Exception


# Tuple
class Tuple(Node):
    def __init__(self, a, b):
        super().__init__()
        if b is None:
            self.value = (a,)
        else:
            self.value = (a, b)

    def eval(self):
        try:
            return self.value
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "Tuple: " + str(self.value)
        return res


class AppendNodeToTuple(Node):
    def __init__(self, my_tuple, value):
        super().__init__()
        self.my_tuple = my_tuple
        self.value = value

    def eval(self):
        try:
            temp_list = list(self.my_tuple.eval())
            temp_list.append(self.value)
            return tuple(temp_list)
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "Tuple: " + str(self.value)
        return res


class TupleIndex(Node):
    def __init__(self, my_tuple, index):
        super().__init__()
        self.my_tuple = my_tuple.eval()
        self.index = index.eval()

    def eval(self):
        try:
            return self.my_tuple[self.index - 1]
        except TypeError:
            raise TypeError
        except Exception:
            raise Exception


class Exponentiation(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def eval(self):
        try:
            return self.left.eval() ** self.right.eval()
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "Exponentiation"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Addition(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self
        self.both_strings = False

    def typeCheck(self):
        lType = type(self.left.eval())
        rType = type(self.right.eval())
        # Both strings
        if lType == str and rType == str:
            self.both_strings = True
            return
        # Both lists
        elif lType == list and rType == list:
            return
        # Both Numbers
        elif (lType == int or lType == float) and (rType == int or rType == float):
            return
        else:
            raise TypeError

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

    def __str__(self):
        res = "\t" * self.parentCount() + "Addition"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Subtraction(Node):
    def __init__(self, left, right):
        super().__init__()
        # print(left, right)
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typeCheck(self):
        lType = type(self.left.eval())
        rType = type(self.right.eval())

        # Both Numbers
        if (lType == int or lType == float) and (rType == int or rType == float):
            return
        else:
            raise TypeError

    def eval(self):
        self.typeCheck()
        try:
            return self.left.eval() - self.right.eval()
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "Subtraction"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Multiplication(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typeCheck(self):
        lType = type(self.left.eval())
        rType = type(self.right.eval())

        # Both Numbers
        if (lType == int or lType == float) and (rType == int or rType == float):
            return
        else:
            raise TypeError

    def eval(self):
        self.typeCheck()
        try:
            return self.left.eval() * self.right.eval()
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "Multiplication"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Division(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typeCheck(self):
        lType = type(self.left.eval())
        rType = type(self.right.eval())

        # Both Numbers
        if (lType == int or lType == float) and (rType == int or rType == float):
            # Div by 0 is Semantic Error
            if self.right.eval() == 0:
                raise TypeError
            return
        else:
            raise TypeError

    def eval(self):
        self.typeCheck()
        try:
            return self.left.eval() / self.right.eval()
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "Division"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Div(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typeCheck(self):
        lType = type(self.left.eval())
        rType = type(self.right.eval())

        # Both Numbers
        if (lType == int or lType == float) and (rType == int or rType == float):
            # Div by 0 is Semantic Error
            if self.right.eval() == 0:
                raise TypeError
            return
        else:
            raise TypeError

    def eval(self):
        self.typeCheck()
        try:
            return self.left.eval() // self.right.eval()
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "div"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Mod(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typeCheck(self):
        lType = type(self.left.eval())
        rType = type(self.right.eval())

        # Both Numbers
        if (lType == int or lType == float) and (rType == int or rType == float):
            if self.right.eval() == 0:
                raise TypeError
            return
        else:
            raise TypeError

    def eval(self):
        self.typeCheck()
        try:
            return self.left.eval() % self.right.eval()
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "mod"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


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

    def __str__(self):
        res = "\t" * self.parentCount() + "not"
        res += "\n" + str(self.child)
        return res


class Conjunction(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

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

    def __str__(self):
        res = "\t" * self.parentCount() + "andalso"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Disjunction(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

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

    def __str__(self):
        res = "\t" * self.parentCount() + "orelse"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class In(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def eval(self):
        try:
            return self.left.eval() in self.right.eval()
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "In"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Concatenation(Node):
    def __init__(self, a, b):
        super().__init__()
        self.a = a.eval()
        self.b = b.eval()

    def eval(self):
        try:
            return [self.a] + self.b
        except Exception:
            raise Exception

    def __str__(self):
        res = "\t" * self.parentCount() + "Concatenation"
        res += "\n" + str(self.a)
        res += "\n" + str(self.b)
        return res


# Comparison Operators
class Comparisons(Node):
    def __init__(self, left, right, operator):
        super().__init__()
        self.left = left
        self.right = right
        self.operator = operator
        self.left.parent = self
        self.right.parent = self


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

    def __str__(self):
        res = "\t" * self.parentCount() + str(self.operator)
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


# Tokens
tokens = ('INTEGER',
          'REAL',
          'BOOL',
          'STRINGS',
          'LIST_LEFT_PARENTHESIS',
          'LIST_RIGHT_PARENTHESIS',
          'LEFT_PARENTHESIS',
          'RIGHT_PARENTHESIS',
          'EXPONENTIATION',
          'MULTIPLICATION',
          'DIVISION',
          'DIV',
          'MOD',
          'PLUS',
          'MINUS',
          'IN',
          'CONCATENATION',
          'NEGATION',
          'CONJUNCTION',
          'DISJUNCTION',
          'LESSTHAN',
          'LESSTHANEQUALTO',
          'EQUALS',
          'NOTEQUALS',
          'GREATERTHANEQUALTO',
          'GREATERTHAN',
          'COMMA',
          'POUND'
          )

t_LIST_LEFT_PARENTHESIS = r'\['
t_LIST_RIGHT_PARENTHESIS = r'\]'
t_LEFT_PARENTHESIS = r'\('
t_RIGHT_PARENTHESIS = r'\)'
t_EXPONENTIATION = r'\*\*'
t_MULTIPLICATION = r'\*'
t_DIVISION = r'\/'
t_DIV = r'div'
t_MOD = r'mod'
t_PLUS = r'\+'
t_MINUS = r'-'
t_IN = r'in'
t_CONCATENATION = r'::'
t_NEGATION = r'not'
t_CONJUNCTION = r'andalso'
t_DISJUNCTION = r'orelse'
t_LESSTHAN = r'<'
t_LESSTHANEQUALTO = r'<='
t_EQUALS = r'=='
t_NOTEQUALS = r'<>'
t_GREATERTHANEQUALTO = r'>='
t_GREATERTHAN = r'>'
t_COMMA = r','
t_POUND = r'\#'

# Ignore whitespace
t_ignore = ' \t'


def t_REAL(t):
    r'(([0-9]*\.[0-9]*)|(\.[0-9]+))([Ee][+-]?[0-9]+)?'
    t.value = Float(t.value)
    return t


def t_INTEGER(t):
    r'[0-9][0-9]*'
    t.value = Integer(t.value)
    return t


def t_BOOL(t):
    r'(True|False)'
    t.value = Boolean(t.value)
    return t


# r'\'[^\'\"]\'|\"[^\'\"]*\"'
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
    raise ValueError


# Build lexer
lexer = lex.lex(debug=1)


def tokenize(inp):
    lexer.input(inp)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)


# Parsing rules

names = {}

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
              ('left', 'LIST_LEFT_PARENTHESIS', 'LIST_RIGHT_PARENTHESIS'),
              ('left', 'POUND'),
              ('left', 'LEFT_PARENTHESIS', 'RIGHT_PARENTHESIS')
              )


def p_expr(t):
    """expr : INTEGER
            | REAL
            | STRINGS
            | BOOL
            | list
            | tuple"""
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
    """args : expr"""
    t[0] = List(t[1])


def p_expr__args(t):
    """args : args COMMA expr"""
    t[0] = AppendValueToList(t[1], t[3])


def p_index(t):
    """index : LIST_LEFT_PARENTHESIS expr LIST_RIGHT_PARENTHESIS"""
    t[0] = t[2].eval()


def p_indexing(t):
    """expr : expr index"""
    t[0] = Indexing(t[1], t[2])


def p_searching(t):
    """expr : expr IN expr"""
    t[0] = Searching(t[1], t[3])


def p_expr_concatenation(p):
    'expr : expr CONCATENATION expr'
    p[0] = Concatenation(p[1], p[3])


# Tuple
def p_tuple(t):
    """tuple : LEFT_PARENTHESIS tuple_args RIGHT_PARENTHESIS"""
    t[0] = t[2]


def p_single_tuple(t):
    """tuple_args : expr COMMA"""
    t[0] = Tuple(t[1].eval(), None)


def p_tuple_args(t):
    """tuple_args : tuple_args COMMA expr"""
    t[0] = AppendNodeToTuple(t[1], t[3].eval())


def p_tuple_args_2(t):
    """tuple_args : expr COMMA expr"""
    t[0] = Tuple(t[1].eval(), t[3].eval())


def p_tuple_Index(t):
    """expr : POUND INTEGER tuple"""
    t[0] = TupleIndex(t[3], t[2])


# Arithmetic Operations
def p_expr_addition(p):
    'expr : expr PLUS expr'
    p[0] = Addition(p[1], p[3])


def p_expr_subtraction(p):
    'expr : expr MINUS expr'
    p[0] = Subtraction(p[1], p[3])


def p_expr2uminus(p):
    'expr : MINUS expr %prec UMINUS'
    p[0] = Integer(-1 * p[2].eval())


def p_expr_multiplication(p):
    'expr : expr MULTIPLICATION expr'
    p[0] = Multiplication(p[1], p[3])


def p_expr_division(p):
    'expr : expr DIVISION expr'
    p[0] = Division(p[1], p[3])


def p_expr_div(p):
    'expr : expr DIV expr'
    p[0] = Div(p[1], p[3])


def p_expr_mod(p):
    'expr : expr MOD expr'
    p[0] = Mod(p[1], p[3])


def p_expr_exponentiation(p):
    'expr : expr EXPONENTIATION expr'
    p[0] = Exponentiation(p[1], p[3])


# Boolean Operations
def p_expr_negation(p):
    'expr : NEGATION expr'
    p[0] = Negation(p[2])


def p_expr_conjunction(p):
    'expr : expr CONJUNCTION expr'
    p[0] = Conjunction(p[1], p[3])


def p_expr_disjunction(p):
    'expr : expr DISJUNCTION expr'
    p[0] = Disjunction(p[1], p[3])


# Comparisons
def p_expr_less_than(p):
    """expr : expr LESSTHAN expr
            | expr LESSTHANEQUALTO expr
            | expr EQUALS expr
            | expr NOTEQUALS expr
            | expr GREATERTHANEQUALTO expr
            | expr GREATERTHAN expr"""
    p[0] = Comparisons(p[1], p[3], p[2])


def p_error(t):
    raise ValueError


parser = yacc.yacc(debug=1)


def parse(inp):
    result = parser.parse(inp, debug=0)
    return result


def evaluate(inp):
    try:
        result = parse(inp)
        if result is not None:
            print(result.eval())
    except ValueError:
        print("SYNTAX ERROR")
    except TypeError:
        print("SEMANTIC ERROR")
    except IndexError:
        print("SEMANTIC ERROR")
    except Exception:
        print("SEMANTIC ERROR")


def main():
    fd = open(sys.argv[1], 'r')
    lines = fd.read().splitlines()
    for line in lines:
        #tokenize(line)
        evaluate(line)
    fd.close()


if __name__ == "__main__":
    main()
