# %%
import ply.lex as lex
import ply.yacc as yacc
from arbol import (Literal, Variable, Visitor, BinaryOp, Declaration,
                   Declarations, Assignment, Statements, Program, IfElse)
from llvmlite import ir

literals = ['+','-','*','/', '%', '(', ')', '{', '}', '<', '>', '=', ';', ',', '!']
reserved = {
    'else' : 'ELSE',
    'float' : 'FLOAT',
    'if' : 'IF',
    'int' : 'INT',
    'float' : 'FLOAT',
    'bool' : 'BOOL',
    'char' : 'CHAR',
    'main' : 'MAIN',
    'return' : 'RETURN',
    'while' : 'WHILE'
}

tokens = list(reserved.values()) + ['ID', 'INTLIT', 'LE', 'GE', 'EQ', 'NEQ', 'AND', 'OR']

t_ignore  = ' \t'

t_LE = r'<='
t_GE = r'>='
t_EQ = r'=='
t_NEQ = r'!='
t_AND = r'&&'
t_OR = r'\|\|'

def t_ID(t):
     r'[a-zA-Z_][a-zA-Z_0-9]*'
     t.type = reserved.get(t.value,'ID')    # Check for reserved words
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


# ========================
def p_Program(p):
    '''
    Program : INT MAIN '(' ')' '{' Declarations Statements '}'
    '''
    p[0] = Program( p[6], p[7] )

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
    p[0] = Declaration(p[2], p[1].upper())

def p_Type(p):
    '''
    Type : INT
         | BOOL
         | FLOAT
         | CHAR
    '''
    p[0] = p[1]

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
              | Block
              | Assignment
              | IfStatement
              | WhileStatement
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

def p_WhileStatement(p):
    '''
    WhileStatement : WHILE '(' Expression ')' Statement
    '''

def p_Expression(p):
    '''
    Expression : Conjunction
               | Expression OR Conjunction
    '''
    if len(p) > 2:
        p[0] = BinaryOp(p[2], p[1], p[3])
    else:
        p[0] = p[1]

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

def p_Primary_IntLit(p):
    'Primary : INTLIT'
    p[0] = Literal(p[1], 'INT')

def p_Primary_Id(p):
    'Primary : ID'
    p[0] = Variable(p[1], 'INT')

def p_Primary_Expression(p):
    '''
    Primary : '(' Expression ')'
    '''
    p[0] = p[2]

# %%
intType = ir.IntType(32)

class IRGenerator(Visitor):
    def __init__(self, builder: ir.IRBuilder):
        self.stack = []
        self.symbolTable = dict()
        self.builder = builder
        self._tmp_count = 0

    def visit_program(self, node: Program) -> None:
        node.decls.accept(self)
        node.stats.accept(self)

    def visit_if_else(self, node: IfElse) -> None:
        node.expr.accept(self)
        expr = self.stack.pop()

        with builder.if_else(expr) as (then, otherwise):
            with then:
                node.thenSt.accept(self)
            # emit instructions for when the predicate is true
            with otherwise:
                if node.elseSt:
                    node.elseSt.accept(self)

            # emit instructions for when the predicate is false

        # emit instructions following the if-else block
        

    def visit_declaration(self, node: Declaration) -> None:
        if node.type == 'INT':
            variable = self.builder.alloca(intType, name=node.name)
            self.symbolTable[node.name] = variable
    
    def visit_declarations(self, node: Declarations) -> None:
        node.declaration.accept(self)
        if node.declarations != None:
            node.declarations.accept(self)
        
    def visit_assignment(self, node: Assignment) -> None:
        node.rhs.accept(self)
        rhs = self.stack.pop()
        self.builder.store(rhs, self.symbolTable[node.lhs])

    # TODO: Implementar estos métodos para que mimiquen el comprotamiento de
    # declarations y que se handlee el tipo de nodo visitado y se genere el código
    # conforme cada caso (tipo con el visit_binary_op)

    def visit_statements(self, node: Statements) -> None:
        node.statement.accept(self)
        if node.statements != None:
            node.statements.accept(self)

    def visit_literal(self, node: Literal) -> None:
        self.stack.append(intType(node.value))
    
    def visit_variable(self, node: Variable) -> None:
        self.stack.append(self.builder.load(self.symbolTable[node.name]))

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
        elif node.op == '/':
            self.stack.append(self.builder.sdiv(lhs, rhs))
        elif node.op == '%':
            self.stack.append(self.builder.srem(lhs, rhs))
        elif node.op == '>' or node.op == '<' or node.op == '>=' \
            or node.op == '<=' or node.op == '==' or node.op == '!=':
            self.stack.append(self.builder.icmp_signed(node.op, lhs, rhs))
        elif node.op == '&&':
            self.stack.append(self.builder.and_(lhs, rhs)) # TODO: Esto hace un and a nivel de bits. Es lo que queremos?
        elif node.op == '||':
            self.stack.append(self.builder.or_(lhs, rhs))

module = ir.Module(name="prog")

fnty = ir.FunctionType(intType, [])
func = ir.Function(module, fnty, name='main')

entry = func.append_basic_block('entry')
builder = ir.IRBuilder(entry)


data =  '''
        int main() {
            int x;
            int y;
            int z;
            
            if (x < 10)
                x = y * 5;
            else
                x = x * 7;
            
            z = 10;
            x = 2;

            if (x < 10)
                x = y * 5;
            else
                x = x * 7;
            
        }
        '''
lexer = lex.lex()
parser = yacc.yacc()
ast = parser.parse(data)
print(ast)

visitor = IRGenerator(builder)
ast.accept(visitor)
# builder.ret(visitor.stack.pop())

print(module)

# %%