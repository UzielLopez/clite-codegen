# %%
import ply.lex as lex
import ply.yacc as yacc
from arbol import Literal, Variable

literals = ['+','-','*','/', '%', '(', ')']
tokens = ['ID', 'INTLIT']

t_ignore  = ' \t'

def t_ID(t):
     r'[a-zA-Z_][a-zA-Z_0-9]*'
     return t

def t_GE(t):
    r'>='
    return t

def t_LE(t):
    r'<='
    return t

def t_INTLIT(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

# %%

def p_Relation(p):
    '''
    Relation : Addition
             | Addition RelOp Add
    '''
    if len(p) > 2:
        p[0] = BinaryOp(p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_Addition(p):
    '''
    Addition : Term
             | Addition AddOp Term
    '''
    if len(p) > 2:
        p[0] = BinaryOp(p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_Term(p):
    '''
    Term : Term MulOp Factor
         | Factor
    '''
    if len(p) > 2:
        p[0] = BinaryOp(p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_AddOp(p):
    '''
    AddOp : '+'
          | '-'
    '''
    p[0] = p[1]

def p_MulOp(p):
    '''
    MulOp : '*'
          | '/'
          | '%'
    '''
    p[0] = p[1]

def p_RelOp(p):
    '''
    RelOp : '<'
          | LE
          | '>'
          | GE 
    '''
    p[0] = p[1]
      
def p_Factor(p):
    '''
    Factor : Primary
    '''
    p[0] = p[1]

def p_Primary(p):
    '''
    Primary : INTLIT 
            | '(' Relation ')'
    '''
    if len(p) == 2:
        p[0] = Literal(p[1], 'INT')
    else:
        p[0] = p[2]

        
def p_error(p):
    print("Syntax error in input!", p)


# %%
from arbol import Visitor, Literal, BinaryOp, Variable
from llvmlite import ir

intType = ir.IntType(32)

class IRGenerator(Visitor):
    def __init__(self, builder):
        self.stack = []
        self.builder = builder

    def visit_literal(self, node: Literal) -> None:
        self.stack.append(intType(node.value))
    
    def visit_variable(self, node: Variable) -> None:
        pass

    def visit_rel_op(self, node: Variable) -> None:
        pass

    def visit_binary_op(self, node: BinaryOp) -> None:
        node.lhs.accept(self)
        node.rhs.accept(self)
        rhs = self.stack.pop()
        lhs = self.stack.pop()
        if node.op == '+':
            self.stack.append(self.builder.add(lhs, rhs))
        elif node.op == '*':
            self.stack.append(self.builder.mul(lhs, rhs))
        elif node.op == '-':
            self.stack.append(self.builder.sub(lhs, rhs))

module = ir.Module(name="prog")

# int main() {
fnty = ir.FunctionType(intType, [])
func = ir.Function(module, fnty, name='main')

entry = func.append_basic_block('entry')
builder = ir.IRBuilder(entry)

data = '10 + 3'
lexer = lex.lex()
parser = yacc.yacc()
ast = parser.parse(data)
print(ast)

visitor = IRGenerator(builder)
ast.accept(visitor)
builder.ret(visitor.stack.pop())

print(module)

# %%