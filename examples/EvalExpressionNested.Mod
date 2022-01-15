(*
expression = term {("+" | "-") term}.
term = factor {("*" | "/") factor}.
factor = id | "(" expression ")".
*)

MODULE EvalExpression;
    CONST zero = 48; nine = 57; 
          times = 42; plus = 43; minus = 45; div = 47; 
          space = 20; number = 1; end = 2; lparen = 40; rparen = 41;
    VAR c, sym, value, val0: INTEGER;

    PROCEDURE W(i: INTEGER); BEGIN Write(i); WriteLn END W;

    PROCEDURE GetNumber;
    BEGIN
        value := 0;
        WHILE (c >= zero) & (c <= nine) DO
            value := 10 * value + (c - zero);
            ReadByte(c);
        END;
    END GetNumber;

    PROCEDURE GetSym;
    BEGIN
        IF c = times THEN sym := times; ReadByte(c)
        ELSIF c = plus THEN sym := plus; ReadByte(c)
        ELSIF c = minus THEN sym := minus; ReadByte(c)
        ELSIF c = div THEN sym := div; ReadByte(c)
        ELSIF c = lparen THEN sym := lparen; ReadByte(c)
        ELSIF c = rparen THEN sym := rparen; ReadByte(c) 
        ELSIF (c >= zero) & (c <= nine) THEN GetNumber; sym := number
        ELSIF c < space THEN sym := end
        ELSE ReadByte(c); GetSym (* unknown input, skip *)
        END;
    END GetSym;

    PROCEDURE expression(VAR val0: INTEGER);
        VAR val1, val2: INTEGER; op: INTEGER;
        PROCEDURE term(VAR val0: INTEGER);
            VAR val1, val2: INTEGER; op: INTEGER;
            PROCEDURE factor(VAR val0: INTEGER);
                VAR val1, val2: INTEGER; op: INTEGER;
            BEGIN
                IF sym = number THEN
                    val0 := value; GetSym
                ELSIF sym = lparen THEN
                    GetSym; expression(val0);
                    IF sym = rparen THEN GetSym ELSE ASSERT(FALSE) END
                ELSIF sym # end THEN
                    ASSERT(FALSE);
                END
            END factor;
        BEGIN factor(val1);
            WHILE (sym = times) OR (sym = div) DO
                op := sym; GetSym; factor(val2);
                IF op = times THEN val1 := val1 * val2 ELSE val1 := val1 DIV val2 END;
            END;
            val0 := val1
        END term;
    BEGIN term(val1);
        WHILE (sym = plus) OR (sym = minus) DO
            op := sym; GetSym; term(val2);
            IF op = plus THEN INC(val1, val2) ELSE DEC(val1, val2) END
        END;
        val0 := val1
    END expression;

BEGIN
    WHILE TRUE DO
        val0 := 0;
        ReadByte(c); GetSym;
        expression(val0);
        W(val0)
    END
END EvalExpression.
