package semantics;

import bg.tu_varna.kst_sit.ci_ep.ast.*;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.ArrayInitNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.AssignableNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.CharacterLiteralNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.StringLiteralNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.*;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.operators.binary_operators.additive_operators.AdditionNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.operators.binary_operators.additive_operators.SubtractionNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.operators.binary_operators.logical_operators.AndNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.operators.binary_operators.logical_operators.OrNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.operators.binary_operators.multiplicative_operators.DivisionNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.operators.binary_operators.multiplicative_operators.ModNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.operators.binary_operators.multiplicative_operators.MultiplicationNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.operators.binary_operators.relational_operators.*;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.operators.unary_operators.MinusNode;
import bg.tu_varna.kst_sit.ci_ep.ast.assignable.expression.operators.unary_operators.NotNode;
import bg.tu_varna.kst_sit.ci_ep.ast.global_definition.FunctionDefinitionNode;
import bg.tu_varna.kst_sit.ci_ep.ast.global_definition.VariableDefinitionNode;
import bg.tu_varna.kst_sit.ci_ep.ast.statement.*;
import bg.tu_varna.kst_sit.ci_ep.ast.type.PrimitiveTypeNode;
import bg.tu_varna.kst_sit.ci_ep.ast.type.TypeNode;
import bg.tu_varna.kst_sit.ci_ep.ast.type.VoidTypeNode;
import bg.tu_varna.kst_sit.ci_ep.exceptions.SemanticException;
import bg.tu_varna.kst_sit.ci_ep.lexer.Lexer;
import bg.tu_varna.kst_sit.ci_ep.parser.Parser;
import bg.tu_varna.kst_sit.ci_ep.semantics.SemanticUtils;
import bg.tu_varna.kst_sit.ci_ep.semantics.scope.FunctionScope;
import bg.tu_varna.kst_sit.ci_ep.semantics.scope.GlobalScope;
import bg.tu_varna.kst_sit.ci_ep.semantics.scope.Scope;
import bg.tu_varna.kst_sit.ci_ep.semantics.scope.StatementScope;
import bg.tu_varna.kst_sit.ci_ep.semantics.symbol.FunctionSymbol;
import bg.tu_varna.kst_sit.ci_ep.semantics.symbol.Symbol;
import bg.tu_varna.kst_sit.ci_ep.semantics.symbol.VariableSymbol;
import bg.tu_varna.kst_sit.ci_ep.semantics.type.ArrayType;
import bg.tu_varna.kst_sit.ci_ep.semantics.type.ExpressionType;
import bg.tu_varna.kst_sit.ci_ep.semantics.type.Type;
import bg.tu_varna.kst_sit.ci_ep.semantics.visitor.SemanticVisitor;
import bg.tu_varna.kst_sit.ci_ep.source.SourceImpl;
import lexer.LexerImpl;
import parser.ParserImpl;
import token.TokenType;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class SemanticAnalyzer implements SemanticVisitor {

    private Scope currentScope;
    private Map<VariableNode, Symbol> varSymbolMap;

    public SemanticAnalyzer() {
        varSymbolMap = new HashMap<>();
    }

    @Override
    public Map<VariableNode, Symbol> getVarSymbolMap() {
        return varSymbolMap;
    }

    @Override
    public void visit(ProgramBodyNode node) {
        currentScope = new GlobalScope();
        node.getChildNodes().forEach(n -> n.accept(this));
    }

    @Override
    public void visit(FunctionDefinitionNode node) {
        List<Type> paramsType = null;
        //Maybe not needed to convert formal parameters to Type
        if (node.getFormalParameters() != null) {
            paramsType = node.getFormalParameters()
                    .getParameters()
                    .stream()
                    .map(n -> SemanticUtils.convertTypeNode(n.getType(), currentScope))
                    .collect(Collectors.toList());
        }
        String methodName = node.getToken().getText();
        TypeNode type = node.getReturnType();
        Type returnType = (Type)SemanticUtils.getSymbol(type, currentScope);
        if (type.isArray()) returnType = new ArrayType(returnType);
        FunctionSymbol symbol = new FunctionSymbol(methodName, returnType, paramsType, currentScope);
        currentScope.addSymbol(symbol);
        currentScope = new FunctionScope(currentScope, symbol);
        if (node.getFormalParameters() != null) node.getFormalParameters().accept(this);
        node.getBlock().accept(this);
        //go back to global scope
        currentScope = currentScope.getEnclosingScope();
    }

    @Override
    public void visit(ActualParameterNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
    }

    @Override
    public void visit(FormalParameterNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
    }

    @Override
    public void visit(PrimitiveTypeNode node) {
        //
    }

    @Override
    public void visit(VoidTypeNode node) {
        //
    }

    @Override
    public void visit(BlockNode node) {
        currentScope = new StatementScope(currentScope);
        node.getChildNodes().forEach(n -> n.accept(this));
        currentScope = currentScope.getEnclosingScope();
    }

    @Override
    public void visit(VariableDefinitionNode node) {
        VariableNode left = node.getSimpleAssignment().getVariable();
        String leftName = left.getToken().getText();
        TypeNode type = node.getType();
        Type t = (Type) SemanticUtils.getSymbol(type, currentScope);
        if (type.isArray()) t = new ArrayType(t);
        currentScope.addSymbol(new VariableSymbol(leftName, t, currentScope));
        node.getSimpleAssignment().accept(this);
    }

    @Override
    public void visit(AssignmentNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        VariableNode variable = node.getVariable();
        AssignableNode assignable = node.getAssignable();
        if (!variable.equals(assignable)) {
            throw new SemanticException("Type mismatch!", assignable.getToken());
        }
    }

    @Override
    public void visit(IfStatementNode node) {
        ExpressionNode expression = node.getExpression();
        expression.accept(this);
        if (expression.getType() != ExpressionType.BOOLEAN.ordinal()) {
            throw new SemanticException("Expression must be of boolean type!", expression.getToken());
        }
        node.getIfBlock().accept(this);
        if (node.getElseBlock() != null) node.getElseBlock().accept(this);
    }

    @Override
    public void visit(WhileStatementNode node) {
        ExpressionNode expression = node.getExpression();
        expression.accept(this);
        if (expression.getType() != ExpressionType.BOOLEAN.ordinal()) {
            throw new SemanticException("Expression must be of boolean type!", expression.getToken());
        }
        node.getBlock().accept(this);
    }

    @Override
    public void visit(ReturnStatementNode node) {
        AssignableNode assignable = node.getExpression();
        int expressionType;
        boolean assignableIsArray = false;
        if (assignable != null) {
            assignable.accept(this);
            expressionType = assignable.getType();
            assignableIsArray = assignable.isArray();
        } else {
            expressionType = ExpressionType.VOID.ordinal();
        }
        FunctionSymbol functionSymbol = SemanticUtils.getFunctionSymbol(currentScope);
        boolean funcIsArray = functionSymbol.getType() instanceof ArrayType;
        String funcType = functionSymbol.getType().getName();

        if (!(ExpressionType.getType(funcType) == expressionType) && (funcIsArray == assignableIsArray)) {
            throw new SemanticException("Expected return type: " + funcType + " Got: " + ExpressionType.values()[expressionType].name().toLowerCase(), node.getToken());
        }
    }

    @Override
    public void visit(PrintStatementNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
    }

    @Override
    public void visit(ReadStatementNode node) {
        List<VariableNode> params = node.getParameters();
        params.forEach(n -> {
            n.accept(this);
            Symbol symbol = SemanticUtils.getSymbol(n, currentScope);
            if (symbol.getType().getName().equals(ExpressionType.BOOLEAN.name().toLowerCase()) ||
                symbol.getType().getName().equals(ExpressionType.VOID.name().toLowerCase())
            ) {
                throw new SemanticException("Params wrong type!", n.getToken());
            }
        });
    }

    @Override
    public void visit(FunctionCall node) {
        FunctionSymbol funcSymbol = (FunctionSymbol)SemanticUtils.getSymbol(node, currentScope);
        node.setType(ExpressionType.getType(funcSymbol.getType().getName()));
        node.setIsArray(funcSymbol.getType() instanceof ArrayType);
        node.getChildNodes().forEach(n -> n.accept(this));
        List<Type> funcParams = funcSymbol.getParams();
        List<AssignableNode> actualParams = node.getActualParameters().getParameters();
        if (funcParams.size() != actualParams.size()) throw new SemanticException("Params are not the same count!", node.getToken());
        for (int i = 0, size = actualParams.size(); i < size; i++) {
            if (!(funcParams.get(i) instanceof ArrayType == actualParams.get(i).isArray() &&
                    ExpressionType.getType(funcParams.get(i).getName()) == actualParams.get(i).getType())) {
                throw new SemanticException("Parameter " + (i + 1) + " type doesn't match", actualParams.get(i).getToken());
            }
        }
    }

    @Override
    public void visit(NotNode node) {
        node.getOperand().accept(this);
        SemanticUtils.handleUnaryOperators(node, ExpressionType.BOOLEAN.ordinal());
        node.setType(ExpressionType.BOOLEAN.ordinal());
    }

    @Override
    public void visit(MinusNode node) {
        /* ToDo */

    }

    @Override
    public void visit(AdditionNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        SemanticUtils.handleBinaryOperators(node, ExpressionType.INT.ordinal());
        node.setType(ExpressionType.INT.ordinal());
    }

    @Override
    public void visit(SubtractionNode node) {
        /* ToDo */
    }

    @Override
    public void visit(MultiplicationNode node) {
        /* ToDo */
    }

    @Override
    public void visit(DivisionNode node) {
        /* ToDo */
    }

    @Override
    public void visit(ModNode node) {
        /* ToDo */
    }

    @Override
    public void visit(AndNode node) {
        /* ToDo */
    }

    @Override
    public void visit(OrNode node) {
        /* ToDo */
    }

    @Override
    public void visit(EqualsNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        SemanticUtils.handleBinaryOperators(node, ExpressionType.INT.ordinal(), ExpressionType.BOOLEAN.ordinal());
        node.setType(ExpressionType.BOOLEAN.ordinal());
    }

    @Override
    public void visit(NotEqualNode node) {
        /* ToDo */
    }

    @Override
    public void visit(GreaterNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        SemanticUtils.handleBinaryOperators(node, ExpressionType.INT.ordinal());
        node.setType(ExpressionType.BOOLEAN.ordinal());
    }

    @Override
    public void visit(GreaterOrEqualNode node) {
        /* ToDo */
    }

    @Override
    public void visit(LessNode node) {
        /* ToDo */
    }

    @Override
    public void visit(LessOrEqualNode node) {
        /* ToDo */
    }

    @Override
    public void visit(IntegerNode node) {
        node.setType(ExpressionType.INT.ordinal());
    }

    @Override
    public void visit(BooleanNode node) {
        /* ToDo */
    }

    @Override
    public void visit(ArrayLengthNode node) {
        VariableNode variable = node.getVariable();
        variable.accept(this);
        if (!variable.isArray()) throw new SemanticException("length accepts only array type variables!", variable.getToken());
        node.setType(ExpressionType.INT.ordinal());
    }

    @Override
    public void visit(TypedVariableNode node) {
        TypeNode type = node.getType();
        Type varType = (Type) SemanticUtils.getSymbol(type, currentScope);
        VariableNode var = node.getVariable();
        if (type.isArray()) varType = new ArrayType(varType);
        String varName = var.getToken().getText();
        Symbol varSymbol = new VariableSymbol(varName, varType, currentScope);
        currentScope.addSymbol(varSymbol);
        varSymbolMap.put(var, varSymbol);
    }

    @Override
    public void visit(VariableNode node) {
        Symbol varSymbol = SemanticUtils.getSymbol(node, currentScope);
        String varType = varSymbol.getType().getName();
        boolean isArrayType = varSymbol.getType() instanceof ArrayType;
        node.setType(ExpressionType.getType(varType));
        //to be an array var then it must not have an index and be defined as ArrayType in the symbol table
        node.setIsArray(node.getIndex() == null && isArrayType);
        if (node.getIndex() != null) {
            if (!isArrayType) throw new SemanticException(varSymbol.getName() + " is not array!", node.getToken());
            node.getIndex().accept(this);
            if (node.getIndex().getType() != ExpressionType.INT.ordinal()) {
                throw new SemanticException("Index of array variable must be of type int!", node.getIndex().getToken());
            }
        }
        varSymbolMap.put(node, varSymbol);
    }

    @Override
    public void visit(ArrayInitNode node) {
        node.setType(ExpressionType.getType(node.getToken().getText()));
        node.setIsArray(true);
        ExpressionNode expression = node.getSize();
        expression.accept(this);
        if (expression.getType() != ExpressionType.INT.ordinal()) {
            throw new SemanticException("Array size must be of type int!", expression.getToken());
        }
    }

    @Override
    public void visit(CharacterLiteralNode node) {
        node.setType(ExpressionType.CHAR.ordinal());
        node.setIsArray(false);
    }

    @Override
    public void visit(StringLiteralNode node) {
        /* ToDo */
        node.setIsArray(true);
    }

    public static void main(String[] args) throws IOException {
        Lexer<TokenType> lexer = new LexerImpl(new SourceImpl("resources/HelloWorld.txt"));
        Parser<TokenType, AST> parser = new ParserImpl(lexer);
        ProgramBodyNode root = (ProgramBodyNode) parser.entryRule();
        SemanticVisitor semanticVisitor = new SemanticAnalyzer();
        semanticVisitor.visit(root);
        Map<VariableNode, Symbol> map = semanticVisitor.getVarSymbolMap();
    }
}
