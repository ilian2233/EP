package code_gen;

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
import bg.tu_varna.kst_sit.ci_ep.ast.type.VoidTypeNode;
import bg.tu_varna.kst_sit.ci_ep.code_generator.CodeGenVisitor;
import bg.tu_varna.kst_sit.ci_ep.code_generator.CodeGeneratorHelper;
import bg.tu_varna.kst_sit.ci_ep.code_generator.Context;
import bg.tu_varna.kst_sit.ci_ep.code_generator.OperationCode;
import bg.tu_varna.kst_sit.ci_ep.lexer.Lexer;
import bg.tu_varna.kst_sit.ci_ep.parser.Parser;
import bg.tu_varna.kst_sit.ci_ep.semantics.scope.Scope;
import bg.tu_varna.kst_sit.ci_ep.semantics.symbol.Symbol;
import bg.tu_varna.kst_sit.ci_ep.semantics.type.ArrayType;
import bg.tu_varna.kst_sit.ci_ep.semantics.type.ExpressionType;
import bg.tu_varna.kst_sit.ci_ep.semantics.visitor.SemanticVisitor;
import bg.tu_varna.kst_sit.ci_ep.source.SourceImpl;
import bg.tu_varna.kst_sit.ci_ep.utils.CompilerTestHelper;
import lexer.LexerImpl;
import parser.ParserImpl;
import semantics.SemanticAnalyzer;
import token.TokenType;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class CodeGenerator implements CodeGenVisitor {

    private int[] code = new int[10000];
    private int pointer = 0;
    private Map<VariableNode, Symbol> map;
    private CodeGeneratorHelper helper = new CodeGeneratorHelper();

    public CodeGenerator(Map<VariableNode, Symbol> map) {
        this.map = map;
    }

    public int[] getGeneratedCode() {
        return Arrays.copyOf(code, pointer + 1);
    }

    @Override
    public void visit(ProgramBodyNode node) {
        node.getChildNodes().forEach(n -> {
            if (n instanceof FunctionDefinitionNode) {
                code[pointer++] = OperationCode.GOTO.ordinal();
                int jump = pointer;
                code[pointer] = pointer + 1;
                pointer++;
                n.accept(this);
                if (!n.getToken().getText().equals("main")) {
                    code[jump] = pointer;
                }
            } else {
                n.accept(this);
            }
        });
        //the last code is return since the main method has void type
        code[--pointer] = OperationCode.HALT.ordinal();
    }

    @Override
    public void visit(FunctionDefinitionNode node) {
        String funcName = node.getToken().getText();
        helper.addFunctionAddress(funcName, pointer);
        helper.addContext(funcName);
        if (node.getFormalParameters() != null) {
            node.getFormalParameters().getParameters().forEach(n -> {
                helper.addVariable(funcName, map.get(n.getVariable()));
            });
        }
        node.getBlock().accept(this);
        if (node.getReturnType().getToken().getText().equals("void")) {
            code[pointer++] = OperationCode.RETURN.ordinal();
        }
        helper.removeContext(funcName);
    }

    @Override
    public void visit(ActualParameterNode node) {
        node.getParameters().forEach(n -> {
            n.accept(this);
        });
    }

    @Override
    public void visit(FormalParameterNode node) {
        node.getParameters().forEach(n-> {

        });
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
        node.getChildNodes().forEach(n -> n.accept(this));
    }

    @Override
    public void visit(VariableDefinitionNode node) {
        VariableNode var = node.getSimpleAssignment().getVariable();
        AssignableNode assignable = node.getSimpleAssignment().getAssignable();

        Symbol variableSymbol = map.get(var);
        Scope scope = variableSymbol.getScope();
        String contextName = helper.getScopeName(scope);

        helper.addVariable(contextName, variableSymbol);
        node.getSimpleAssignment().accept(this);
    }

    @Override
    public void visit(AssignmentNode node) {

        VariableNode left = node.getVariable();
        AssignableNode right = node.getAssignable();

        //prepare the left side of the assignment
        Symbol variableSymbol = map.get(left);
        Scope scope = variableSymbol.getScope();
        String contextName = helper.getScopeName(scope);
        int position = helper.getPositionInContext(contextName, variableSymbol);

        //check if assignable is array
        //arrayref = arrayref2
        if (right.isArray()) {
            right.accept(this);
            if (contextName.equals("global")) {
                code[pointer++] = OperationCode.GSTORE.ordinal();
            } else {
                code[pointer++] = OperationCode.ASTORE.ordinal();
            }
            code[pointer++] = position;
        } else {
            //indexed variable
            //a[x] = b;
            if (left.getIndex() != null) {
                if (contextName.equals("global")) {
                    code[pointer++] = OperationCode.GLOAD.ordinal();
                } else {
                    code[pointer++] = OperationCode.ALOAD.ordinal();
                }
                code[pointer++] = position;
                //load the index of the array
                left.getIndex().accept(this);
                //push the value on the operand stack
                right.accept(this);
                code[pointer++] = helper.getArrayStoreOp(variableSymbol.getType().getName());
                //a = b
            } else {
                right.accept(this);
                if (contextName.equals("global")) {
                    code[pointer++] = OperationCode.GSTORE.ordinal();
                } else {
                    code[pointer++] = OperationCode.ISTORE.ordinal();
                }
                code[pointer++] = position;
            }
        }
    }

    @Override
    public void visit(IfStatementNode node) {
        node.getExpression().accept(this);
        code[pointer++] = OperationCode.IF_FALSE.ordinal();
        int falseExpression = pointer;
        code[pointer++] = -1;
        node.getIfBlock().accept(this);
        code[falseExpression] = pointer;
        if (node.getElseBlock() != null) {
            //if you were in the if statement block then skip the else block
            code[pointer++] = OperationCode.GOTO.ordinal();
            int trueExpression = pointer;
            code[pointer++] = -1;
            //if you fail if statement then continue in the else block
            code[falseExpression] = pointer;
            node.getElseBlock().accept(this);
            code[trueExpression] = pointer;
        }

    }

    @Override
    public void visit(WhileStatementNode node) {
        int gotoExpression = pointer;
        node.getExpression().accept(this);
        code[pointer++] = OperationCode.IF_FALSE.ordinal();
        int jumpAddr = pointer;
        code[pointer++] = -1;
        node.getBlock().accept(this);
        //go back to while loop
        code[pointer++] = OperationCode.GOTO.ordinal();
        code[pointer++] = gotoExpression;
        code[jumpAddr] = pointer;
    }

    @Override
    public void visit(ReturnStatementNode node) {
        if (node.getExpression() != null) {
            node.getExpression().accept(this);
            if (node.getExpression().isArray()) {
                code[pointer++] = OperationCode.ARETURN.ordinal();
                return;
            }
            code[pointer++] = OperationCode.IRETURN.ordinal();
        } else {
            code[pointer++] = OperationCode.RETURN.ordinal();
        }
    }

    @Override
    public void visit(PrintStatementNode node) {
        node.getActualParameters()
                .getParameters()
                .forEach(n -> {
                    n.accept(this);
                    code[pointer++] = OperationCode.ICONST.ordinal();
                    code[pointer++] = helper.findType(n.getType(), n.isArray());
                });
        code[pointer++] = OperationCode.ICONST.ordinal();
        code[pointer++] = node.getActualParameters().getParameters().size();
        code[pointer++] = OperationCode.PRINT.ordinal();
    }

    @Override
    public void visit(ReadStatementNode node) {
        node.getParameters().forEach(n -> {
            Symbol symbol = map.get(n);
            Scope scope = symbol.getScope();
            String contextName = helper.getScopeName(scope);
            int position = helper.getPositionInContext(contextName, symbol);
            int context = contextName.equals("global") ? Context.GLOBAL_CONTEXT : Context.LOCAL_CONTEXT;
            if (position == -1) {
                throw new RuntimeException("Incorrect argument for function \'read\': " + symbol.getName());
            }
            int type = helper.findType(n.getType(), n.isArray());
            code[pointer++] = OperationCode.ICONST.ordinal();
            code[pointer++] = context;
            code[pointer++] = OperationCode.ICONST.ordinal();
            code[pointer++] = position;
            code[pointer++] = OperationCode.ICONST.ordinal();
            code[pointer++] = type;
        });
        //push the count of vars on the stack
        code[pointer++] = OperationCode.ICONST.ordinal();
        code[pointer++] = node.getParameters().size();

        //perform read operation
        code[pointer++] = OperationCode.READ.ordinal();
    }

    @Override
    public void visit(FunctionCall node) {
        List<AssignableNode> params = node.getActualParameters().getParameters();
        int paramsCount = 0;
        if (params != null) {
            paramsCount = params.size();
            params.forEach(p -> p.accept(this));
        }
        code[pointer++] = OperationCode.ICONST.ordinal();
        code[pointer++] = paramsCount;
        code[pointer++] = OperationCode.CALL.ordinal();
        code[pointer++] = helper.getFunctionAddress(node.getToken().getText());
    }

    @Override
    public void visit(NotNode node) {
        node.getOperand().accept(this);
        code[pointer++] = OperationCode.NOT.ordinal();
    }

    @Override
    public void visit(MinusNode node) {
        node.getOperand().accept(this);
        code[pointer++] = OperationCode.INEG.ordinal();
    }

    @Override
    public void visit(AdditionNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.IADD.ordinal();
    }

    @Override
    public void visit(SubtractionNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.ISUB.ordinal();
    }

    @Override
    public void visit(MultiplicationNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.IMUL.ordinal();
    }

    @Override
    public void visit(DivisionNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.IDIV.ordinal();
    }

    @Override
    public void visit(ModNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.IREM.ordinal();
    }

    @Override
    public void visit(AndNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.IAND.ordinal();
    }

    @Override
    public void visit(OrNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.IOR.ordinal();
    }

    @Override
    public void visit(EqualsNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.ICMPEQ.ordinal();
    }

    @Override
    public void visit(NotEqualNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.ICMPNE.ordinal();
    }

    @Override
    public void visit(GreaterNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.ICMPGT.ordinal();
    }

    @Override
    public void visit(GreaterOrEqualNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.ICMPGE.ordinal();
    }

    @Override
    public void visit(LessNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.ICMPLT.ordinal();
    }

    @Override
    public void visit(LessOrEqualNode node) {
        node.getChildNodes().forEach(n -> n.accept(this));
        code[pointer++] = OperationCode.ICMPLE.ordinal();
    }

    @Override
    public void visit(IntegerNode node) {
        int num = Integer.parseInt(node.getToken().getText());
        code[pointer++] = OperationCode.ICONST.ordinal();
        code[pointer++] = num;
    }

    @Override
    public void visit(BooleanNode node) {
        int num = node.getToken().getText().equals("true") ? 1 : 0;
        code[pointer++] = OperationCode.ICONST.ordinal();
        code[pointer++] = num;
    }

    @Override
    public void visit(ArrayLengthNode node) {
        node.getVariable().accept(this);
        code[pointer++] = OperationCode.ARRAYLENGTH.ordinal();
    }

    @Override
    public void visit(TypedVariableNode node) {

    }


    //|=====================================================================
//        |								gload(load into operand stack)
//|
//        |
//        |=====================================================================|
//        |Reference type	|arr		|	index == null		|	index != null |
//            |				|___________|_______________________|_________________|
//            |				|			|	aload(load ref)		|				  |
//            |				|int		|						|	iaload		  |
//            |				|char		|						|	caload		  |
//            |				|boolean	|						|	baload		  |
//            |=====================================================================|
//            |Primitive type	|			|										  |
//            |				|int		|	iload 								  |
//            |				|char		|	iload 								  |
//            |				|boolean	|	iload 								  |
//            |=====================================================================|
//
//
    @Override
    public void visit(VariableNode node) {
        Symbol symbol = map.get(node);
        Scope scope = symbol.getScope();
        String contextName = helper.getScopeName(scope);
        int position = helper.getPositionInContext(contextName, symbol);

        if (contextName.equals("global")) {
            code[pointer++] = OperationCode.GLOAD.ordinal();
        } else if (symbol.getType() instanceof ArrayType){
            code[pointer++] = OperationCode.ALOAD.ordinal();
        } else {
            code[pointer++] = OperationCode.ILOAD.ordinal();
        }
        code[pointer++] = position;

        if (node.getIndex() != null) {
            //push index into the operand stack
            node.getIndex().accept(this);
            int operation = -1;
            switch (symbol.getType().getName()) {
                case "int":         operation = OperationCode.IALOAD.ordinal(); break;
                case "char":        operation = OperationCode.CALOAD.ordinal(); break;
                case "boolean":     operation = OperationCode.BALOAD.ordinal(); break;
            }
            code[pointer++] = operation;
        }
    }

    @Override
    public void visit(ArrayInitNode node) {
        node.getSize().accept(this);
        String typeName = ExpressionType.getName(node.getType());
        code[pointer++] = OperationCode.NEWARRAY.ordinal();
        code[pointer++] = helper.getType(typeName);
    }

    @Override
    public void visit(CharacterLiteralNode node) {
        code[pointer++] = OperationCode.ICONST.ordinal();
        code[pointer++] = node.getToken().getText().charAt(0);
    }

    @Override
    public void visit(StringLiteralNode node) {
        String str = node.getToken().getText();
        code[pointer++] = OperationCode.ICONST.ordinal();
        code[pointer++] = str.length();

        code[pointer++] = OperationCode.NEWARRAY.ordinal();
        code[pointer++] = CodeGeneratorHelper.T_CHAR;
        //array ref is already pushed
        for (int i = 0; i < str.length(); i++) {
            code[pointer++] = OperationCode.ICONST.ordinal();
            code[pointer++] = node.getToken().getText().charAt(i);
        }
        code[pointer++] = OperationCode.ICONST.ordinal();
        code[pointer++] = str.length();

        code[pointer++] = OperationCode.CASTOREALL.ordinal();
    }

    public static void main(String[] args) throws IOException {
        Lexer<TokenType> lexer = new LexerImpl(new SourceImpl("resources/Fib.txt"));
        Parser<TokenType, AST> parser = new ParserImpl(lexer);
        ProgramBodyNode root = (ProgramBodyNode) parser.entryRule();
        SemanticVisitor semanticVisitor = new SemanticAnalyzer();
        semanticVisitor.visit(root);
        Map<VariableNode, Symbol> map = semanticVisitor.getVarSymbolMap();
        CodeGenerator codeGen = new CodeGenerator(map);
        codeGen.visit(root);
        int[] code = codeGen.getGeneratedCode();
        System.out.println(CompilerTestHelper.getCodeAsString(code));
        //CompilerTestHelper.runVM(code);
    }
}
