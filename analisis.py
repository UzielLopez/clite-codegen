# %%
import ply.lex as lex
import ply.yacc as yacc
from arbol import (Literal, Variable, BinaryOp, Declaration,
                   Declarations, Assignment, Statements,
                   IfElse, WhileStatement, ReturnStatement, Parameter, Function,
                   Functions, FunctionCallStatement, ForStatement)
from llvmlite import ir

from ir_generator import IRGenerator

literals = ['+','-','*','/', '%', '(', ')', '{', '}', '<', '>', '=', ';', ',', '!']
reserved = {
    'else' : 'ELSE',
    'float' : 'FLOAT',
    'if' : 'IF',
    'int' : 'INT',
    'bool' : 'BOOL',
    'char' : 'CHAR',
    'return' : 'RETURN',
    'while' : 'WHILE',
    'for' : 'FOR'
}

tokens = list(reserved.values()) + ['ID', 'INTLIT', 'FLOATLIT','LE', 'GE', 'EQ', 'NEQ', 'AND', 'OR']

t_ignore  = ' \t'

t_LE = r'<='
t_GE = r'>='
t_EQ = r'=='
t_NEQ = r'!='
t_AND = r'&&'
t_OR = r'\|\|'

def t_ID(t):
     r'[a-zA-Z_][a-zA-Z_0-9]*'
     t.type = reserved.get(t.value,'ID')
     return t

def t_FLOATLIT(t):
    r'(\+|\-)?(\d+)?(((0x|X)([0-9a-fA-F])?(\.?([0-9a-fA-F])?(p|P)(\+|\-)?\d+|(\.([0-9a-fA-F])?)))|((\.?(\d+)?(e|E)(\+|\-)?(\d+)|(\.(\d+)?))))(l|L|f|F)?'
    # NOTE: técnicamente Python solo va a poder castear los floats en C que puuedan existir en Python con el mismo formato
    return t

def t_INTLIT(t):
    r'[0-9]+'
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)


# ========================
def p_Functions(p):
    '''
    Functions : Function Functions
              | empty
    '''
    if len(p) > 2:
        p[0] = Functions(p[1], p[2])

def p_Function(p):
    '''
    Function : Type ID '(' ParameterList ')' '{' Declarations Statements '}'
             | Type ID '(' ')' '{' Declarations Statements '}'
    '''
    if len(p) > 9:
        p[0] = Function(p[1], p[2], p[4], p[7], p[8])
    else:
        p[0] = Function(p[1], p[2], [], p[6], p[7])

def p_ParameterList(p):
    '''
    ParameterList : Parameter
                  | ParameterList ',' Parameter
    '''
    if len(p) > 3:
        p[1].append(p[3])
        p[0] = p[1]
    else:
        p[0] = [p[1]]

def p_Parameter(p):
    '''
    Parameter : Type ID
    '''
    if p[1] == "INT":
        type = ir.IntType(32)
    
    elif p[1] == "FLOAT":
        type = ir.floatType(32)

    p[0] = Parameter(type, p[2])


def p_empty(p):
    '''
    empty :
    '''
    pass

def p_Declarations(p):
    '''
    Declarations : Declaration Declarations
                 | empty
    '''
    if len(p) > 2:
        p[0] = Declarations(p[1], p[2])
    
def p_Declaration(p):
    '''
    Declaration : Type ID ';'
    '''
    p[0] = Declaration(p[2], p[1])

def p_Type(p):
    '''
    Type : INT
         | BOOL
         | FLOAT
         | CHAR
    '''
    p[0] = p[1].upper()

def p_Statements(p):
    '''
    Statements : Statement Statements
               | empty
    '''
    if len(p) > 2:
        p[0] = Statements(p[1], p[2])


def p_Statement(p):
    '''
    Statement : ';'
              | Expression ';'
              | Block
              | Assignment
              | IfStatement
              | ForStatement
              | WhileStatement
              | ReturnStatement
    '''
    p[0] = p[1]

def p_Block(p):
    '''
    Block : '{' Statements '}'
    '''
    p[0] = p[2]

def p_Assignment(p):
    '''
    Assignment : ID '=' Expression ';'
    '''
    p[0] = Assignment(p[1], p[3])

def p_IfStatement(p):
    '''
    IfStatement : IF '(' Expression ')' Statement 
                | IF '(' Expression ')' Statement ELSE Statement
    '''
    if len(p) > 6:
        p[0] = IfElse(p[3], p[5], p[7])
    else:
        p[0] = IfElse(p[3], p[5], None)

def p_ForStatement(p):
    '''
    ForStatement : FOR '(' ID '=' Expression ';' Expression ';' ID '=' Expression ')' Statement
    '''
    # Como es una simplificación del lenguaje C, en este loop solo
    # puede haber una variable de control, la cual debe ser la ID
    # de esta producción. Se espera que esa misma variable esté
    # presente en las dos otras secciones de del for statement
    p[0] = ForStatement(p[3], p[5], p[7], p[11], p[13])

def p_WhileStatement(p):
    '''
    WhileStatement : WHILE '(' Expression ')' Statement
    '''
    p[0] = WhileStatement(p[3], p[5])

def p_ReturnStatement(p):
    '''
    ReturnStatement : RETURN Expression ';'
                    | RETURN ';'
    '''
    if len(p) > 3:
        p[0] = ReturnStatement(p[2])
    else:
        p[0] = ReturnStatement(None)

def p_Expression(p):
    '''
    Expression : Conjunction
               | FunctionCallStatement
               | Expression OR Conjunction
    '''
    if len(p) > 2:
        p[0] = BinaryOp(p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_FunctionCallStatement(p):
    '''
    FunctionCallStatement : ID '(' ArgumentsList ')'
                          | ID '(' ')'
    '''
    if len(p) > 4:
        p[0] = FunctionCallStatement(p[1], p[3])
    else:
        p[0] = FunctionCallStatement(p[1], [])

def p_ArgumentsList(p):
    '''
    ArgumentsList : Expression
                  | ArgumentsList ',' Expression
    '''
    if len(p) > 3:
        p[1].append(p[3])
        p[0] = p[1]
    else:
        p[0] = [p[1]]

def p_Conjunction(p):
    '''
    Conjunction : Equality
                | Conjunction AND Equality
    '''
    if len(p) > 2:
        p[0] = BinaryOp(p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_Equality(p):
    '''
    Equality : Relation
             | Relation EquOp Relation
    '''
    if len(p) > 2:
        p[0] = BinaryOp(p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_EquOp(p):
    '''
    EquOp : EQ
          | NEQ
    '''
    p[0] = p[1]

def p_Relation(p):
    '''
    Relation : Addition
             | Addition RelOp Addition
    '''
    if len(p) > 2:
        p[0] = BinaryOp(p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_RelOp(p):
    '''
    RelOp : '<'
          | LE
          | '>'
          | GE 
    '''
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

def p_AddOp(p):
    '''
    AddOp : '+'
          | '-'
    '''
    p[0] = p[1]

def p_Term(p):
    '''
    Term : Factor
         | Term MulOp Factor
    '''
    if len(p) > 2:
        p[0] = BinaryOp(p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_MulOp(p):
    '''
    MulOp : '*'
          | '/'
          | '%'
    '''
    p[0] = p[1]

def p_Factor(p):
    '''
    Factor : Primary
           | UnaryOp Primary
    '''
    #TODO: Recuerda que esto es mejor handlearlo con las operaciones
    # de ir
    if len(p) > 2:
        if p[1] == '-':
            p[2].value = -p[2].value
        else:
            p[2].value = int(not(-p[2].value))
        p[0] = p[2]
    else:
        p[0] = p[1]

def p_UnaryOp(p):
    '''
    UnaryOp : '-'
            | '!'
    '''
    p[0] = p[1]

def p_Primary_FloatLit(p):
    'Primary : FLOATLIT'
    p[0] = Literal(p[1], 'FLOAT')

def p_Primary_IntLit(p):
    'Primary : INTLIT'
    p[0] = Literal(p[1], 'INT')

def p_Primary_Id(p):
    'Primary : ID'
    p[0] = Variable(p[1])

def p_Primary_Expression(p):
    '''
    Primary : '(' Expression ')'
    '''
    p[0] = p[2]

# %%

module = ir.Module(name="prog")

data =  '''
        int factorial(int n) {
            int f;
            int i;
            f = 1;
            i = 1;
            while ((i <= n) && (1 != 8)) {
                f = f * i;
                i = i + 1;
            }
            return f;
        }

        int main() {
            int r;
            float f;
            f = 1.2 + 0.4;
            r = (factorial(5)) + (factorial(1));
            return r;
        }
        '''
lexer = lex.lex()
parser = yacc.yacc()
ast = parser.parse(data)
print("ast => ",ast)

visitor = IRGenerator(module)
ast.accept(visitor)

print(module)

# %%
import runtime as rt
from ctypes import CFUNCTYPE, c_int

engine = rt.create_execution_engine()
mod = rt.compile_ir(engine, str(module))
func_ptr = engine.get_function_address("main")

cfunc = CFUNCTYPE(c_int)(func_ptr)
res = cfunc()
print(f"main() devolvió:", res)

