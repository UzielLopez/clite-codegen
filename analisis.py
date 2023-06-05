# %%
import ply.lex as lex
import ply.yacc as yacc
from arbol import (Literal, Variable, Visitor, BinaryOp, Declaration,
                   Declarations, Assignment, Statements, Program,
                   IfElse, WhileStatement, ReturnStatement, Parameter, Function,
                   Functions, FunctionCallStatement, ForStatement)
from llvmlite import ir

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
        type = intType
    
    elif p[1] == "FLOAT":
        type = floatType

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
intType = ir.IntType(32)
floatType = ir.FloatType()

class IRGenerator(Visitor):
    def __init__(self, module):
        self.stack = []
        self.symbolTable = dict()
        self.parametersTable = dict()
        self.functionTable = dict()
        self.builder = None
        self.func = None
        self.module = module
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
        name = f"{self.func.name}.{node.name}"
        if node.type == 'INT':
            variable = self.builder.alloca(intType, name=name)
            self.symbolTable[name] = variable
        if node.type == 'FLOAT':
            variable = self.builder.alloca(floatType, name=name)
            self.symbolTable[name] = variable
    
    def visit_declarations(self, node: Declarations) -> None:
        node.declaration.accept(self)
        if node.declarations != None:
            node.declarations.accept(self)
    
    def visit_functions(self, node: Functions) -> None:
        node.function.accept(self)
        if node.functions != None:
            node.functions.accept(self)

    def visit_function(self, node: Function) -> None:

        if node.return_type == 'INT':
            return_type = intType
        
        elif node.return_type == 'FLOAT':
            return_type = floatType
        
        parameters = [parameter.type for parameter in node.parameter_list]
        function_type = ir.FunctionType(return_type, parameters)

        self.func = ir.Function(self.module, function_type, node.name)
        self.functionTable[node.name] = self.func
        # Nombrar los parámetros de entrada de la función
        for i in range(len(parameters)):
            parameter_name = f"{self.func.name}.{node.parameter_list[i].name}"
            self.func.args[i].name = parameter_name
            print("que chuchas es esto?: ", self.func.args[i])
            self.parametersTable[parameter_name] = self.func.args[i]

        entry = self.func.append_basic_block("entry")
        self.builder = ir.IRBuilder(entry)

        if node.declarations:
            node.declarations.accept(self)
        if node.statements:
            node.statements.accept(self)
        
    def visit_assignment(self, node: Assignment) -> None:
        #TODO: Si assignment es un binaryOp, ignoralo, no hagas nada o haz
        # que se handlee de tal forma que en el stack no quede insertada la operacion
        # pero si se agregue al codegen?
        node.rhs.accept(self)
        rhs = self.stack.pop()
        # Está cool que si esta linea falla eso significa que se está
        # haciendo referencia a algo out of scope c:
        name = f"{self.func.name}.{node.lhs}"
        self.builder.store(rhs, self.symbolTable[name])

    def visit_statements(self, node: Statements) -> None:
        node.statement.accept(self)
        if node.statements != None:
            node.statements.accept(self)
    
    def visit_while_statement(self, node: WhileStatement) -> None:

        while_head = self.func.append_basic_block('while-head')
        while_body = self.func.append_basic_block('while-body')
        while_exit = self.func.append_basic_block('while-exit')

        self.builder.branch(while_head)
        #TODO: c Branch espera que la condición se evalue a un
        #IntType(1), por lo que para que las expresiones que no dan
        # como resultado un bool debes de asegurarte que en las expresiones
        # que evaluan cosas que dan como resultado un bool en verdad
        # los operandos sean bool. tal vez tengas que castear
        self.builder.position_at_start(while_head)
        node.head.accept(self) # Esto va a ser, at most, una expression
        head_expression = self.stack.pop()
        self.builder.cbranch(head_expression, while_body, while_exit)
        self.builder.position_at_start(while_body)
        node.body.accept(self)
        self.builder.branch(while_head)
        self.builder.position_at_start(while_exit)
    
    def visit_for_statement(self, node: ForStatement) -> None:
        # Handlear el assignment de la init_expression a la variable control_variable
        node.init_expression.accept(self)
        init_expression = self.stack.pop()
        control_variable_name = f"{self.func.name}.{node.control_variable}"
        self.builder.store(init_expression, self.symbolTable[control_variable_name])

        # Handlear la cond_expression como el head del while
        for_condition = self.func.append_basic_block('for-condition')
        for_body = self.func.append_basic_block('for-body')
        for_exit = self.func.append_basic_block('for-exit')

        self.builder.branch(for_condition)
        self.builder.position_at_start(for_condition)

        node.cond_expression.accept(self)
        cond_expression = self.stack.pop()
        self.builder.cbranch(cond_expression, for_body, for_exit)

        self.builder.position_at_start(for_body)
        # Hacer la operaciíon de loop_expression
        node.loop_expression.accept(self)
        loop_expression = self.stack.pop()
        self.builder.store(loop_expression, self.symbolTable[control_variable_name])
        # Visitar statement
        node.statement.accept(self)
        self.builder.branch(for_condition)
        self.builder.position_at_start(for_exit)
    
    def visit_function_call_statement(self, node: FunctionCallStatement):
        
        arg_n = len(node.arguments_list)
        args = []
        for i in range(arg_n):
            node.arguments_list[i].accept(self)
            args.append(self.stack.pop())
        
        self.stack.append(self.builder.call(self.functionTable[node.function_to_call], args))
                
    def visit_return_statement(self, node: ReturnStatement):
        
        if node.expression:
            node.expression.accept(self)
            return_expression = self.stack.pop()
            self.builder.ret(return_expression)
        else:
            self.builder.ret_void()

    def visit_literal(self, node: Literal) -> None:
        if node.type == 'INT':
            self.stack.append(intType(node.value))
        elif node.type == 'FLOAT':
            self.stack.append(floatType(node.value))
    
    def visit_variable(self, node: Variable) -> None:
        name = f"{self.func.name}.{node.name}"
        if name in self.parametersTable:
            self.stack.append(self.parametersTable[name])
        else:
            self.stack.append(self.builder.load(self.symbolTable[name]))

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

data =  '''
        int factorial(int n) {
            int f;
            int i;
            f = 1;
            i = 1;
            while (i <= n) {
                f = f * i;
                i = i + 1;
            }
            return f;
        }

        int main() {
            int r;
            r = factorial(5);
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

