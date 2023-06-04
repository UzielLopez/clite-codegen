# %%
import ply.lex as lex
import ply.yacc as yacc
from arbol import (Literal, Variable, Visitor, BinaryOp, Declaration,
                   Declarations, Assignment, Statements, Program,
                   IfElse, WhileStatement)
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
     t.type = reserved.get(t.value,'ID')    # Check for reserved words
     return t

def t_INTLIT(t):
    r'[0-9]+'
    return t

def t_FLOATLIT(t):
    r'(\+|\-)?(\d+)?(((0x|X)([0-9a-fA-F])?(\.?([0-9a-fA-F])?(p|P)(\+|\-)?\d+|(\.([0-9a-fA-F])?)))|((\.?(\d+)?(e|E)(\+|\-)?(\d+)|(\.(\d+)?))))(l|L|f|F)?'
    # NOTE: técnicamente Python solo va a poder castear los floats en C que puuedan existir en Python con el mismo formato
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
    p[0] = WhileStatement(p[3], p[5])

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

def p_Primary_FloatLit(p):
    'Primary : FLOATLIT'
    print("parceó un floatlit")
    p[0] = Literal(p[1], 'FLOAT')

def p_Primary_IntLit(p):
    'Primary : INTLIT'
    p[0] = Literal(p[1], 'INT')

def p_Primary_Id(p):
    'Primary : ID'
    # TODO: Aquí hay un problema. Estamos asumiendo el tipo de la variable, pero técnicamente no la conocemos cuando
    # se identifica de una. Piensa en otra forma de handlear esto.
    p[0] = Variable(p[1], 'INT')

def p_Primary_Expression(p):
    '''
    Primary : '(' Expression ')'
    '''
    p[0] = p[2]

# %%
intType = ir.IntType(32)
floatType = ir.FloatType()

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

        with self.builder.if_else(expr) as (then, otherwise):
            with then:
                node.thenSt.accept(self)
            with otherwise:
                if node.elseSt:
                    node.elseSt.accept(self)

    def visit_declaration(self, node: Declaration) -> None:
        if node.type == 'INT':
            variable = self.builder.alloca(intType, name=node.name)
            self.symbolTable[node.name] = variable
        if node.type == 'FLOAT':
            variable = self.builder.alloca(floatType, name=node.name)
            self.symbolTable[node.name] = variable
    
    def visit_declarations(self, node: Declarations) -> None:
        node.declaration.accept(self)
        if node.declarations != None:
            node.declarations.accept(self)
        
    def visit_assignment(self, node: Assignment) -> None:
        node.rhs.accept(self)
        rhs = self.stack.pop()
        self.builder.store(rhs, self.symbolTable[node.lhs])

    def visit_statements(self, node: Statements) -> None:
        node.statement.accept(self)
        if node.statements != None:
            node.statements.accept(self)
    
    def visit_while_statement(self, node: WhileStatement) -> None:

        whileHead = func.append_basic_block('while-head')
        whileBody = func.append_basic_block('while-body')
        whileExit = func.append_basic_block('while-exit')

        self.builder.branch(whileHead)
        #TODO: c Branch espera que la condición se evalue a un
        #IntType(1), por lo que para que las expresiones que no dan
        # como resultado un bool debes de asegurarte que en las expresiones
        # que evaluan cosas que dan como resultado un bool en verdad
        # los operandos sean bool. tal vez tengas que castear
        self.builder.position_at_start(whileHead)
        node.head.accept(self) # Esto va a ser, at most, una expression
        head_expression = self.stack.pop()
        self.builder.cbranch(head_expression, whileBody, whileExit)
        self.builder.position_at_start(whileBody)
        node.body.accept(self)
        self.builder.branch(whileHead)
        self.builder.position_at_start(whileExit)

    def visit_literal(self, node: Literal) -> None:
        # TODO: Muévele aquí cuando ya hayas implementado los otros tipos 
        if node.type == 'INT':
            self.stack.append(intType(node.value))
        elif node.type == 'FLOAT':
            self.stack.append(floatType(node.value))
    
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
            int f;
            int i;
            int n;
            float e;

            n = 5;
            f = 1;
            i = 1;
            e = 2.5;

            while (i <= n) {
                f = i;
                i = i + 1;
            }
            
            f = 2;
            i = 5;

            if( 1 && 1){
                i = 55;
            }

        }
        '''
lexer = lex.lex()
parser = yacc.yacc()
ast = parser.parse(data)
print("ast => ",ast)

visitor = IRGenerator(builder)
ast.accept(visitor)
# builder.ret(visitor.stack.pop())

print(module)

# %%


