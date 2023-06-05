from __future__ import annotations
from abc import ABC, abstractmethod
from typing import Any

class ASTNode(ABC):
    @abstractmethod
    def accept(self, visitor: Visitor) -> None:
        pass
class Program(ASTNode):
    def __init__(self, decls: Any, stats: Any) -> None:
        self.decls = decls
        self.stats = stats

    def accept(self, visitor: Visitor):
        visitor.visit_program(self)

class Literal(ASTNode):
    def __init__(self, value: Any, type: str) -> None:
        self.type = type

        if(type == "INT"):
            self.value = int(value)

        elif(type == "FLOAT"):
            self.value = float(value)

    def accept(self, visitor: Visitor):
        visitor.visit_literal(self)

class Variable(ASTNode):
    def __init__(self, name: Any) -> None:
        self.name = name

    def accept(self, visitor: Visitor):
        visitor.visit_variable(self)

class BinaryOp(ASTNode):
    def __init__(self, op: str, lhs: ASTNode, rhs: ASTNode) -> None:
        self.lhs = lhs
        self.rhs = rhs
        self.op = op

    def accept(self, visitor: Visitor):
        visitor.visit_binary_op(self)

class IfElse(ASTNode):
    def __init__(self, expr: Any, thenSt: Any, elseSt: Any) -> None:
        self.expr = expr
        self.thenSt = thenSt
        self.elseSt = elseSt
    
    def accept(self, visitor: Visitor) -> None:
        visitor.visit_if_else(self)

class WhileStatement(ASTNode):
    #head puede ser cualuier expresión
    def __init__(self, head: any, body: any):
        self.head = head
        self.body = body
    
    def accept(self, visitor: Visitor) -> None:
        visitor.visit_while_statement(self)

class ForStatement(ASTNode):
    def __init__(self, control_variable: str, init_expression: any, cond_expression: any, loop_expression: any, statement: any) -> None:
        self.control_variable = control_variable
        self.init_expression = init_expression
        self.cond_expression = cond_expression
        self.loop_expression = loop_expression
        self.statement = statement
    
    def accept(self, visitor: Visitor) -> None:
        visitor.visit_for_statement(self)
        
class ReturnStatement(ASTNode):
    def __init__(self, expression: any) -> None:
        self.expression = expression
    
    def accept(self, visitor: Visitor) -> None:
        visitor.visit_return_statement(self)

class FunctionCallStatement(ASTNode):
    def __init__(self, function_to_call: str, arguments_list: list) -> None:
        self.function_to_call = function_to_call
        self.arguments_list = arguments_list
    
    def accept(self, visitor: Visitor) -> None:
        visitor.visit_function_call_statement(self)

class Declaration(ASTNode):
    def __init__(self, name: str, type: str) -> None:
        self.name = name
        self.type = type

    def accept(self, visitor: Visitor):
        visitor.visit_declaration(self)

class Declarations(ASTNode):
    def __init__(self, declaration: Declaration, declarations: Declarations) -> None:
        self.declaration = declaration
        self.declarations = declarations

    def accept(self, visitor: Visitor):
        visitor.visit_declarations(self)

class Assignment(ASTNode):
    def __init__(self, lhs: str, rhs: ASTNode) -> None:
        self.lhs = lhs
        self.rhs = rhs
    
    def accept(self, visitor: Visitor):
        visitor.visit_assignment(self)

class Statements(ASTNode):
    def __init__(self, statement: any, statements: Statements) -> None:
        self.statement = statement # Puede ser de clase IfElse, Assignment o WhileStatement
        self.statements = statements

    def accept(self, visitor: Visitor) -> None:
        visitor.visit_statements(self)

class Parameter(ASTNode):
    def __init__(self, type: any, name: str) -> None:
        self.type = type
        self.name = name
    
    def accept(self, visitor: Visitor) -> None:
        visitor.visit_parameter(self)

class Function(ASTNode):
    def __init__(self, return_type: str, name: str, parameter_list: list, declarations: Declarations, statements: Statements) -> None:
        self.return_type = return_type
        self.name = name
        self.parameter_list = parameter_list
        self.declarations = declarations
        self.statements = statements
    def accept(self, visitor: Visitor) -> None:
        visitor.visit_function(self)

class Functions(ASTNode):
    def __init__(self, function: Function, functions: Functions) -> None:
        self.function = function
        self.functions = functions
    
    def accept(self, visitor: Visitor) -> None:
        visitor.visit_functions(self)
        
class Visitor(ABC):
    @abstractmethod
    def visit_literal(self, node: Literal) -> None:
        pass
    @abstractmethod
    def visit_variable(self, node: Variable) -> None:
        pass
    @abstractmethod
    def visit_binary_op(self, node: BinaryOp) -> None:
        pass
    @abstractmethod
    def visit_program(self, node: Program) -> None:
        pass
    @abstractmethod
    def visit_if_else(self, node: IfElse) -> None:
        pass
    @abstractmethod
    def visit_while_statement(self, node: WhileStatement) -> None:
        pass
    @abstractmethod
    def visit_for_statement(self, node: ForStatement) -> None:
        pass
    @abstractmethod
    def visit_return_statement(self, node: ReturnStatement) -> None:
        pass
    @abstractmethod
    def visit_function_call_statement(self, node: FunctionCallStatement) -> None:
        pass
    @abstractmethod
    def visit_declaration(self, node: Declaration) -> None:
        pass
    @abstractmethod
    def visit_declarations(self, node: Declarations) -> None:
        pass
    @abstractmethod
    def visit_assignment(self, node: Assignment) -> None:
        pass
    @abstractmethod
    def visit_statements(self, node: Statements) -> None:
        pass
    @abstractmethod
    def visit_function(self, node: Function) -> None:
        pass
    @abstractmethod
    def visit_functions(self, node: Functions) -> None:
        pass
