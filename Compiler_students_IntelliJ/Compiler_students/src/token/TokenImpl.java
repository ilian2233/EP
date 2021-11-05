package token;

import bg.tu_varna.kst_sit.ci_ep.lexer.token.Token;

public class TokenImpl implements Token<TokenType> {

    private TokenType tokenType;
    private String text;
    private int line;
    private int position;

    public TokenImpl(TokenType tokenType, String text, int position, int lineNumber) {
        this.tokenType = tokenType;
        this.text = text;
        this.position = position;
        this.line = lineNumber;
    }

    public TokenImpl(TokenType tokenType, int position, int lineNumber) {
        this(tokenType, tokenType.value, position, lineNumber);
    }

    public TokenType getTokenType() {
        return tokenType;
    }

    @Override
    public String getLexeme() {
        return tokenType.name();
    }

    public String getText() {
        return text;
    }

    public int getPosition() {
        return position;
    }

    public int getLine() {
        return line;
    }

}
