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

package lexer;

import bg.tu_varna.kst_sit.ci_ep.exceptions.LexicalException;
import bg.tu_varna.kst_sit.ci_ep.lexer.Lexer;
import bg.tu_varna.kst_sit.ci_ep.lexer.token.Token;
import bg.tu_varna.kst_sit.ci_ep.source.Source;
import bg.tu_varna.kst_sit.ci_ep.source.SourceImpl;
import bg.tu_varna.kst_sit.ci_ep.utils.CompilerTestHelper;
import token.TokenImpl;
import token.TokenType;

import java.io.IOException;

public class LexerImpl extends Lexer<TokenType> {

    private int line;
    private int position;

    public LexerImpl(Source source) {
        super(source);
    }

    @Override
    public Token<TokenType> nextToken() {
        currentChar = source.getCurrentChar();
        line = source.getLineNumber();
        position = source.getPosition() + 1;
        while (currentChar != Source.EOF) {
            switch (currentChar) {
                //space and tabs
                case ' ' : case '\t' : handleSpaceAndTabs(); continue;

                //2 character operators
                case '-' : return handleTwoCharOp('>', TokenType.MINUS, TokenType.ARROW);
                case '=' : return handleTwoCharOp('=', TokenType.BECOMES, TokenType.EQUALS);
                case '>' : return handleTwoCharOp('=', TokenType.GREATER, TokenType.GREATER_EQ);
                case '<' : return handleTwoCharOp('=', TokenType.LESS, TokenType.LESS_EQ);
                case '!' : return handleTwoCharOp('=', TokenType.NOT,TokenType.NOTEQUALS);
                case '&' : return handleTwoCharOp('&', TokenType.AND, TokenType.OTHER);
                case '|' : return handleTwoCharOp('|', TokenType.OR, TokenType.OTHER);
                case '/' : return handleSlash();
                case '\'': return handleCharLiteral();
                case '"' : return handleStringLiteral();

                //1 character operators
                case '+' : return retTokenAndAdvance(TokenType.PLUS);
                case '[' : return retTokenAndAdvance(TokenType.LSQUARE); 
				case ']' : return retTokenAndAdvance(TokenType.RSQUARE);
                case '{' : return retTokenAndAdvance(TokenType.LBRACKET);
                case '}' : return retTokenAndAdvance(TokenType.RBRACKET);
                case '(' : return retTokenAndAdvance(TokenType.LPAREN);
                case ')' : return retTokenAndAdvance(TokenType.RPAREN);
                case ';' : return retTokenAndAdvance(TokenType.SEMICOLON);
                case '*' : return retTokenAndAdvance(TokenType.MUL);
                case '%' : return retTokenAndAdvance(TokenType.MOD);
                case ',' : return retTokenAndAdvance(TokenType.COMMA);
                case '@' : return retTokenAndAdvance(TokenType.AT);

                default  :
                    if (isLetter(currentChar)) { return handleIdentifier(); }
                    if (isDigit(currentChar)) { return handleDigit(); }
                    return retTokenAndAdvance(TokenType.OTHER, currentChar + "");
            }
        }
        return null;
    }

    private Token<TokenType> retTokenAndAdvance(TokenType token) {
        source.next();
        return new TokenImpl(token, position, line);
    }

    private Token<TokenType> retTokenAndAdvance(TokenType token, String text) {
        source.next();
        return new TokenImpl(token, text, position, line);
    }

    private Token<TokenType> retToken(TokenType token) {
        return new TokenImpl(token, position, line);
    }

    private Token<TokenType> retToken(TokenType token, String text) {
        return new TokenImpl(token, text, position, line);
    }

    private void handleSpaceAndTabs() {
        while (currentChar == ' ' || currentChar == '\t') {
            currentChar = source.next();
        }
        line = source.getLineNumber();
        position = source.getPosition() + 1;
    }

    private Token<TokenType> handleTwoCharOp(char followingChar, TokenType firstMatchedToken, TokenType secondMatchedToken) {
        if (source.next() == followingChar) {
            return retTokenAndAdvance(secondMatchedToken);
        }
        return retToken(firstMatchedToken);
    }

    private Token<TokenType> handleSlash() {
        if (source.next() == '/') {
            int currentLineNum = source.getLineNumber();
            while (currentLineNum == source.getLineNumber()) {
                source.next();
            }
            return nextToken();
        }
        return retToken(TokenType.DIV);
    }

    private Token<TokenType> handleCharLiteral() {
        char ch = source.next();
        if (ch == '\'') { return retTokenAndAdvance(TokenType.OTHER); }
        if (ch == '\\') ch = handleSpecialChars();
        currentChar = source.next();
        if (currentChar == '\'') {
            return retTokenAndAdvance(TokenType.CHAR_LITERAL, "" + ch);
        }
        return retTokenAndAdvance(TokenType.OTHER);
    }

    private char handleSpecialChars() {
        switch (source.next()) {
            case 'n'    : return '\n';
            case 't'    : return '\t';
            case 'b'    : return '\b';
            case 'r'    : return '\r';
            case 'f'    : return '\f';
            case '\''   : return '\'';
            case '"'    : return '"';
            case '\\'   : return '\\';
            default     : throw new LexicalException("Incorrect char escape: " + currentChar, line, position);
        }
    }

    private Token<TokenType> handleStringLiteral() {
        StringBuilder sb = new StringBuilder();
        while((currentChar = source.next()) != Source.EOF && currentChar != '"') {
            if (currentChar == '\\') currentChar = handleSpecialChars();
            sb.append(currentChar);
        }
        //string is not closed
        if (currentChar == Source.EOF) {
            throw new LexicalException("String quote not closed!", line, position);
        }
        return retTokenAndAdvance(TokenType.STRING_LITERAL, sb.toString());
    }

    private Token<TokenType> handleIdentifier() {
        StringBuilder sb = new StringBuilder();
        sb.append(currentChar);
        currentChar = source.next();
        while(isLetter(currentChar) || isDigit(currentChar)) {
            sb.append(currentChar);
            currentChar = source.next();
        }
        String res = sb.toString();
        if (TokenType.isKeyword(res)) {
            //its a keyword so find it and return it
            return retToken(TokenType.valueOf(res.toUpperCase()));
        }
        return retToken(TokenType.IDENTIFIER, res);
    }

    private Token<TokenType> handleDigit() {
        StringBuilder sb = new StringBuilder();
        while (isDigit(currentChar)) {
            sb.append(currentChar);
            currentChar = source.next();
        }
        String digit = sb.toString();
        // check if it is an integer
        try {
            Integer.parseInt(digit);
        } catch (NumberFormatException e) {
            throw new LexicalException("Not a valid integer " + digit + "." , line, position, e);
        }
        return retToken(TokenType.NUMBER, digit);
    }

    private boolean isLetter(char ch) {
        return Character.isLetter(ch);
    }

    private boolean isDigit(char ch) {
        return Character.isDigit(ch);
    }

    public static void main(String[] args) throws IOException {
        Lexer<TokenType> lexer = new LexerImpl(new SourceImpl("resources/HelloWorld.txt"));
        System.out.println(CompilerTestHelper.getTokensAsString(lexer));
    }

}
