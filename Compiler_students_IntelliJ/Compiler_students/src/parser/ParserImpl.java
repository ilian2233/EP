/*
MIT License

Copyright (c) 2018 Computing and Engineering Department, Technical University of Varna

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package parser;

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
import bg.tu_varna.kst_sit.ci_ep.ast.global_definition.GlobalDefinitionNode;
import bg.tu_varna.kst_sit.ci_ep.ast.global_definition.VariableDefinitionNode;
import bg.tu_varna.kst_sit.ci_ep.ast.statement.*;
import bg.tu_varna.kst_sit.ci_ep.ast.type.PrimitiveTypeNode;
import bg.tu_varna.kst_sit.ci_ep.ast.type.TypeNode;
import bg.tu_varna.kst_sit.ci_ep.ast.type.VoidTypeNode;
import bg.tu_varna.kst_sit.ci_ep.exceptions.SyntaxException;
import bg.tu_varna.kst_sit.ci_ep.lexer.Lexer;
import bg.tu_varna.kst_sit.ci_ep.lexer.token.Token;
import bg.tu_varna.kst_sit.ci_ep.parser.Parser;
import bg.tu_varna.kst_sit.ci_ep.semantics.symbol.Symbol;
import bg.tu_varna.kst_sit.ci_ep.source.SourceImpl;
import bg.tu_varna.kst_sit.ci_ep.utils.CompilerTestHelper;
import lexer.LexerImpl;
import token.TokenType;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class ParserImpl extends Parser<TokenType, AST> {

    public ParserImpl(Lexer<TokenType> lexer) {
        super(lexer);
    }

    private void accept(TokenType tokenType) {
        if (currentToken.getTokenType() != tokenType) {
            throw new SyntaxException("Token doesn't match! Expected " +
                    tokenType.value + ", Got " + currentToken.getTokenType().value, currentToken);
        }
        currentToken = lexer.nextToken();
    }

    @Override
    public AST entryRule() {
        accept(TokenType.PROGRAM);
        accept(TokenType.LBRACKET);
        programBody();
        accept(TokenType.RBRACKET);
        return currentNode;
    }

    void programBody() {
        List<GlobalDefinitionNode> globalDefinitions = new ArrayList<>();
        while(
                TokenType.isPrimitiveType(currentToken.getTokenType()) ||
                        (currentToken.getTokenType() == TokenType.IDENTIFIER && !currentToken.getText().equals("main"))
        ) {
            if (currentToken.getTokenType() == TokenType.IDENTIFIER) {
                functionDefinition();
            } else {
                variableDefinition();
                accept(TokenType.SEMICOLON);
            }
            globalDefinitions.add((GlobalDefinitionNode) currentNode);
        }
        mainFunction();
        globalDefinitions.add((GlobalDefinitionNode) currentNode);
        currentNode = new ProgramBodyNode(null, globalDefinitions);
    }

    void functionDefinition() {
        Token token = currentToken;
        accept(TokenType.IDENTIFIER);
        accept(TokenType.LPAREN);
        FormalParameterNode formalParameters = null;
        if (TokenType.isPrimitiveType(currentToken.getTokenType())) {
            formalParameters();
            formalParameters = (FormalParameterNode) currentNode;
        }

        accept(TokenType.RPAREN);
        accept(TokenType.ARROW);
        TypeNode typeNode;
        if (currentToken.getTokenType() == TokenType.VOID) {
            typeNode = new VoidTypeNode(currentToken);
            accept(TokenType.VOID);
        } else {
            type();
            typeNode = (TypeNode) currentNode;
        }
        block();
        BlockNode blockNode = (BlockNode)currentNode;
        currentNode = new FunctionDefinitionNode(token, formalParameters, typeNode, blockNode);
    }

    void functionCall() {
        accept(TokenType.AT);
        Token token = currentToken;
        accept(TokenType.IDENTIFIER);
        accept(TokenType.LPAREN);
        ActualParameterNode actualParameters = null;
        if (TokenType.isLiteralTerminal(currentToken.getTokenType())) {
            actualParameters();
            actualParameters = (ActualParameterNode) currentNode;
        }
        accept(TokenType.RPAREN);
        currentNode = new FunctionCall(token, actualParameters);
    }

    void type() {
        Token token = currentToken;
        boolean isArray = false;
        if (TokenType.isPrimitiveType(currentToken.getTokenType())) {
            accept(currentToken.getTokenType());
            if (currentToken.getTokenType() == TokenType.LSQUARE) {
                isArray = true;
                accept(TokenType.LSQUARE);
                accept(TokenType.RSQUARE);
            }
        } else {
            throw new SyntaxException("Expected return type. Got " + currentToken.getTokenType().value, currentToken);
        }
        currentNode = new PrimitiveTypeNode(token, isArray);
    }

    void formalParameters() {
        List<TypedVariableNode> formalParameters = new ArrayList<>();
        type();
        formalParameters.add(new TypedVariableNode(null, (TypeNode) currentNode, new VariableNode(currentToken, null)));
        accept(TokenType.IDENTIFIER);
        while (currentToken.getTokenType() == TokenType.COMMA) {
            accept(TokenType.COMMA);
            type();
            formalParameters.add(new TypedVariableNode(null, (TypeNode) currentNode, new VariableNode(currentToken, null)));
            accept(TokenType.IDENTIFIER);
        }
        currentNode = new FormalParameterNode(null, formalParameters);
    }

    void actualParameters() {
        List<AssignableNode> params = new ArrayList<>();
        assignable();
        params.add((AssignableNode) currentNode);
        while(currentToken.getTokenType() == TokenType.COMMA) {
            accept(TokenType.COMMA);
            assignable();
            params.add((AssignableNode) currentNode);
        }
        currentNode = new ActualParameterNode(null, params);
    }

    void variableDefinition() {
        type();
        TypeNode type = (TypeNode)currentNode;
        assignment();
        currentNode = new VariableDefinitionNode(null, type, (AssignmentNode) currentNode);
    }

    void assignment() {
        variable();
        VariableNode variable = (VariableNode) currentNode;
        Token token = currentToken;
        accept(TokenType.BECOMES);
        if (TokenType.isPrimitiveType(currentToken.getTokenType())) {
            arrayInitialization();
        } else if (TokenType.CHAR_LITERAL == currentToken.getTokenType()) {
            characterLiteral();
        } else if (TokenType.STRING_LITERAL == currentToken.getTokenType()) {
            stringLiteral();
        } else {
            expression();
        }
        AssignableNode assignable = (AssignableNode) currentNode;
        currentNode = new AssignmentNode(token, variable, assignable);
    }

    void arrayInitialization() {
        Token token = currentToken;
        ExpressionNode expression = null;
        if (TokenType.isPrimitiveType(currentToken.getTokenType())) {
            switch (currentToken.getTokenType())
            {
                case INT -> accept(TokenType.INT);
                case CHAR -> accept(TokenType.CHAR);
                case BOOLEAN -> accept(TokenType.BOOLEAN);
                default -> throw new SyntaxException("Expected primitive type. Got " + currentToken.getTokenType(), currentToken);
            }
            accept(TokenType.LSQUARE);
            expression();
            expression = (ExpressionNode) currentNode;
            accept(TokenType.RSQUARE);
        } else {
            System.out.println("Expected array initialization. Got " + currentToken.getTokenType());
        }
        currentNode = new ArrayInitNode(token, expression);
    }

    void block() {
        List<Statement> statements = new ArrayList<>();
        accept(TokenType.LBRACKET);
        while (TokenType.isStatementTerminal(currentToken.getTokenType())) {
            statement();
            statements.add((Statement) currentNode);
        }
        accept(TokenType.RBRACKET);
        currentNode = new BlockNode(null, statements);
    }

    void expression() {
        simpleExpression();
        Token<TokenType> token = currentToken;
        ExpressionNode left = (ExpressionNode) currentNode;
        //currentNode points to the simpleExpression no need to assign
        if (TokenType.isRelationalOperator(currentToken.getTokenType())) {
            ExpressionNode right;
            ExpressionNode relationalOperator = null;
            switch (currentToken.getTokenType())
            {
                case EQUALS -> accept(TokenType.EQUALS);
                case NOTEQUALS -> accept(TokenType.NOTEQUALS);
                case GREATER -> accept(TokenType.GREATER);
                case GREATER_EQ -> accept(TokenType.GREATER_EQ);
                case LESS -> accept(TokenType.LESS);
                case LESS_EQ -> accept(TokenType.LESS_EQ);
            }
            simpleExpression();
            right = (ExpressionNode) currentNode;
            switch (token.getTokenType()) {
                case EQUALS -> relationalOperator = new EqualsNode(token, left, right);
                case NOTEQUALS -> relationalOperator = new NotEqualNode(token, left, right);
                case GREATER -> relationalOperator = new GreaterNode(token, left, right);
                case GREATER_EQ -> relationalOperator = new GreaterOrEqualNode(token, left, right);
                case LESS -> relationalOperator = new LessNode(token, left, right);
                case LESS_EQ -> relationalOperator = new LessOrEqualNode(token, left, right);
            }
            currentNode = relationalOperator;
        }
    }

    void simpleExpression() {
        signedTerm();
        ExpressionNode left = (ExpressionNode) currentNode;
        //PLUS, MINUS, OR
        while (TokenType.isOperatorGroupOne(currentToken.getTokenType())) {
            Token<TokenType> token = currentToken;
            switch (currentToken.getTokenType())
            {
                case PLUS -> accept(TokenType.PLUS);
                case MINUS -> accept(TokenType.MINUS);
                case OR -> accept(TokenType.OR);
            }
            signedTerm(); // Technically could also be simple expression.
            ExpressionNode right = (ExpressionNode) currentNode;
            ExpressionNode additiveOperator = switch (token.getTokenType()) {
                case PLUS -> new AdditionNode(token, left, right);
                case MINUS -> new SubtractionNode(token, left, right);
                case OR -> new OrNode(token, left, right);
                default -> null;
            };
            currentNode = left = additiveOperator;
        }
    }

    void signedTerm() {
        Token<TokenType> token = null;
        if (TokenType.isUnaryOperator(currentToken.getTokenType())) {
            token = currentToken;
            switch (currentToken.getTokenType())
            {
                case NOT -> accept(TokenType.NOT);
                case MINUS -> accept(TokenType.MINUS);
                default -> throw new SyntaxException("TODO Text", currentToken);
            }
        }
        term();
        ExpressionNode operand = (ExpressionNode) currentNode;
        if (token != null) {
            switch (token.getTokenType()) {
                case NOT -> operand = new NotNode(token, operand);
                case MINUS -> operand = new MinusNode(token, operand);
            }
        }
        currentNode = operand;
    }

    void term() {
        factor();
        ExpressionNode left = (ExpressionNode) currentNode;
        //MUL, DIV, MOD, AND
        while (TokenType.isOperatorGroupTwo(currentToken.getTokenType())) {
            Token<TokenType> token = currentToken;
            switch (currentToken.getTokenType())
            {
                case MUL -> accept(TokenType.MUL);
                case DIV -> accept(TokenType.DIV);
                case MOD -> accept(TokenType.MOD);
                default -> throw new SyntaxException("Placeholder text", currentToken);
            }
            factor();
            ExpressionNode right = (ExpressionNode) currentNode;
            ExpressionNode multiplicativeOperator = switch (token.getTokenType()) {
                case MUL -> new MultiplicationNode(token, left, right);
                case DIV -> new DivisionNode(token, left, right);
                case MOD -> new ModNode(token, left, right);
                case AND -> new AndNode(token, left, right);
                default -> null;
            };
            currentNode = left = multiplicativeOperator;
        }
    }

    void factor() {
        switch (currentToken.getTokenType()) {
            case IDENTIFIER -> variable();
            case NUMBER -> {
                currentNode = new IntegerNode(currentToken);
                accept(TokenType.NUMBER);
            }
            case TRUE, FALSE -> {
                currentNode = new BooleanNode(currentToken);
                if (currentToken.getTokenType() == TokenType.TRUE) {
                    accept(TokenType.TRUE);
                } else {
                    accept(TokenType.FALSE);
                }
            }
            case LENGTH -> arrayLength();
            case LPAREN -> {
                accept(TokenType.LPAREN);
                expression();
                accept(TokenType.RPAREN);
            }
            case AT -> functionCall();
            default -> throw new SyntaxException("Expected factor. Got " + currentToken.getTokenType().value, currentToken);
        }
    }

    void variable() {
        Token token = currentToken;
        accept(TokenType.IDENTIFIER);
        ExpressionNode expression = null;
        if (currentToken.getTokenType() == TokenType.LSQUARE) {
            accept(TokenType.LSQUARE);
            simpleExpression();
            expression = (ExpressionNode) currentNode;
            accept(TokenType.RSQUARE);
        }
        currentNode = new VariableNode(token, expression);
    }

    void mainFunction() {
        Token token = currentToken;
        if (!currentToken.getText().equals("main"))
        {
            throw new SyntaxException(
                    "Expected main function to be named 'main'. Got " + currentToken.getText(), currentToken);
        }

        accept(TokenType.IDENTIFIER);
        accept(TokenType.LPAREN);
        accept(TokenType.RPAREN);
        accept(TokenType.ARROW);
        TypeNode typeNode = new VoidTypeNode(currentToken);
        accept(TokenType.VOID);
        block();
        currentNode = new FunctionDefinitionNode(token, null, typeNode, (BlockNode) currentNode);
    }

    void statement() {
        if (TokenType.isCompoundStatementTerminal(currentToken.getTokenType())) {
            compoundStatement();
        } else {
            simpleStatement();
            accept(TokenType.SEMICOLON);
        }
    }

    void simpleStatement() {
        switch (currentToken.getTokenType()) {
            case INT, CHAR, BOOLEAN -> variableDefinition();
            case IDENTIFIER -> assignment();
            case AT -> functionCall();
            case RETURN -> returnStatement();
            case PRINT -> printStatement();
            case READ -> readStatement();
            default -> throw new SyntaxException("Expected simpleStatement. Got " + currentToken.getTokenType().value, currentToken);
        }
    }

    void compoundStatement() {
        if (currentToken.getTokenType() == TokenType.IF) {
            ifStatement();
        } else {
            whileStatement();
        }
    }

    void ifStatement() {
        Token token = currentToken;
        accept(TokenType.IF);
        accept(TokenType.LPAREN);
        expression();
        ExpressionNode expressionNode = (ExpressionNode) currentNode;
        accept(TokenType.RPAREN);
        block();
        BlockNode ifStatement = (BlockNode) currentNode;
        BlockNode elseStatement = null;
        if (currentToken.getTokenType() == TokenType.ELSE) {
            accept(TokenType.ELSE);
            block();
            elseStatement = (BlockNode) currentNode;
        }
        currentNode = new IfStatementNode(token, expressionNode, ifStatement, elseStatement);
    }

    void whileStatement() {
        Token token = currentToken;
        accept(TokenType.WHILE);
        accept(TokenType.LPAREN);
        expression();
        ExpressionNode expressionNode = (ExpressionNode) currentNode;
        accept(TokenType.RPAREN);
        block();
        BlockNode blockNode = (BlockNode) currentNode;
        currentNode = new WhileStatementNode(token, expressionNode, blockNode);
    }

    void returnStatement() {
        Token token = currentToken;
        accept(TokenType.RETURN);
        AssignableNode assignable = null;
        if (TokenType.isLiteralTerminal(currentToken.getTokenType()) ||
                TokenType.isFactorTerminal(currentToken.getTokenType()) ||
                TokenType.isUnaryOperator(currentToken.getTokenType())) {
            assignable();
            assignable = (AssignableNode) currentNode;
        }
        currentNode = new ReturnStatementNode(token, assignable);
    }

    void printStatement() {
        Token token = currentToken;
        accept(TokenType.PRINT);
        accept(TokenType.LPAREN);
        actualParameters();
        ActualParameterNode actualParameters = (ActualParameterNode) currentNode;
        accept(TokenType.RPAREN);
        currentNode = new PrintStatementNode(token, actualParameters);
    }

    void readStatement() {
        Token token = currentToken;
        List<VariableNode> variables = new ArrayList<>();
        accept(TokenType.READ);
        accept(TokenType.LPAREN);
        while (currentToken.getTokenType() == TokenType.IDENTIFIER) {
            variable();
            variables.add((VariableNode) currentNode);
        }
        accept(TokenType.RPAREN);
        currentNode = new ReadStatementNode(token, variables);
    }

    void assignable() {
        if (TokenType.isFactorTerminal(currentToken.getTokenType()) || TokenType.isUnaryOperator(currentToken.getTokenType())) {
            expression();
        } else if (TokenType.isPrimitiveType(currentToken.getTokenType())) {
            arrayInitialization();
        } else if (TokenType.CHAR_LITERAL == currentToken.getTokenType()) {
            characterLiteral();
        } else {
            stringLiteral();
        }
    }

    void characterLiteral() {
        currentNode = new CharacterLiteralNode(currentToken);
        accept(TokenType.CHAR_LITERAL);
    }

    void stringLiteral() {
        currentNode = new StringLiteralNode(currentToken);
        accept(TokenType.STRING_LITERAL);
    }

    void arrayLength() {
        Token token = currentToken;
        accept(TokenType.LENGTH);
        accept(TokenType.LPAREN);
        variable();
        accept(TokenType.RPAREN);
        currentNode = new ArrayLengthNode(token, (VariableNode) currentNode);
    }

    public static void main(String[] args) throws IOException {
        Lexer<TokenType> lexer = new LexerImpl(new SourceImpl("resources/HelloWorld.txt"));
        Parser<TokenType, AST> parser = new ParserImpl(lexer);
        System.out.println(CompilerTestHelper.getASTasString(parser));
    }
}
