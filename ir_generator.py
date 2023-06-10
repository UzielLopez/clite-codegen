from arbol import (Literal, Variable, Visitor, BinaryOp, Declaration,
                   Declarations, Assignment, Statements, Program,
                   IfElse, WhileStatement, ReturnStatement, Function,
                   Functions, FunctionCallStatement, ForStatement,
                   Factor)
from llvmlite import ir


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
        self.intType = ir.IntType(32)
        self.floatType = ir.FloatType()
        self.boolType = ir.IntType(1)

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
            variable = self.builder.alloca(self.intType, name=name)
            self.symbolTable[name] = variable
        if node.type == 'FLOAT':
            variable = self.builder.alloca(self.floatType, name=name)
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
            return_type = self.intType
        
        elif node.return_type == 'FLOAT':
            return_type = self.floatType
        
        parameters = [parameter.type for parameter in node.parameter_list]
        function_type = ir.FunctionType(return_type, parameters)

        self.func = ir.Function(self.module, function_type, node.name)
        self.functionTable[node.name] = self.func
        # Nombrar los parámetros de entrada de la función
        for i in range(len(parameters)):
            parameter_name = f"{self.func.name}.{node.parameter_list[i].name}"
            self.func.args[i].name = parameter_name
            self.parametersTable[parameter_name] = self.func.args[i]

        entry = self.func.append_basic_block("entry")
        self.builder = ir.IRBuilder(entry)

        if node.declarations:
            node.declarations.accept(self)
        if node.statements:
            node.statements.accept(self)
        
    def visit_assignment(self, node: Assignment) -> None:
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

    def visit_factor(self, node: Factor) -> None: #needs float
        node.value.accept(self)
        value = self.stack.pop()

        if node.type == '!':
            if node.value.type == 'INT':
                self.stack.append(self.builder.not_(value))
        elif node.type == '-':
            if node.value.type == 'INT':
                self.stack.append(self.builder.neg(value))
            elif node.value.type == 'FLOAT':
                self.stack.append(self.builder.fneg(value))
    
    def visit_while_statement(self, node: WhileStatement) -> None:

        while_head = self.func.append_basic_block('while-head')
        while_body = self.func.append_basic_block('while-body')
        while_exit = self.func.append_basic_block('while-exit')

        self.builder.branch(while_head)
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
            self.stack.append(self.intType(node.value))
        elif node.type == 'FLOAT':
            self.stack.append(self.floatType(node.value))
    
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

        # NOTE: al ser una versión simplificada de C, decidimos solo hacer operaciones
        # cuando los dos operandos son del mismo tipo. No casteamos nada al momento de hacer la operación.
        rhs_type = None
        if isinstance(rhs, ir.Function):
            rhs_type = rhs.function_type.return_type
            
        else:
            rhs_type = rhs.type
        
        lhs_type = None
        if isinstance(lhs, ir.Function):
            lhs_type = lhs.function_type.return_type
        else:
            lhs_type = lhs.type
         
        if lhs_type != rhs_type:
            # Siempre hacemos el casteo a float. 
            # Ya después en visit_assignment se hace toma
            # la decisión si se queda como float o se pasa a entero
            if lhs_type == self.floatType:
                rhs = rhs.sitofp(self.floatType)
            else:
                print("conversion")
                lhs = lhs.sitofp(self.floatType)

            #raise Exception(f"Operations with missmatched operands are not implemented. lhs_type={lhs_type} and rhs_type={rhs_type}")
        
        if lhs_type == self.intType or lhs_type == self.boolType:
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
                self.stack.append(self.builder.or_(lhs, rhs)) # TODO: Esto hace un and a nivel de bits. Es lo que queremos?

        elif lhs_type == self.floatType:
            if node.op == '+':
                self.stack.append(self.builder.fadd(lhs, rhs))
            elif node.op == '*':
                self.stack.append(self.builder.fmul(lhs, rhs))
            elif node.op == '-':
                self.stack.append(self.builder.fsub(lhs, rhs))
            elif node.op == '/':
                self.stack.append(self.builder.fdiv(lhs, rhs))
            elif node.op == '%':
                self.stack.append(self.builder.frem(lhs, rhs))
            elif node.op == '>' or node.op == '<' or node.op == '>=' \
                or node.op == '<=' or node.op == '==' or node.op == '!=':
                self.stack.append(self.builder.icmp_signed(node.op, lhs, rhs))
            elif node.op == '&&':
                self.stack.append(self.builder.and_(lhs, rhs))
            elif node.op == '||':
                self.stack.append(self.builder.or_(lhs, rhs))