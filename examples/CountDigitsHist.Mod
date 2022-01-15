MODULE CountDigitsHist;
    CONST zero = 48; nine = 57; star = 42; tab = 9; tilde = 7 * 16 + 14;
          len = nine - zero + 1;
    TYPE Digits = ARRAY len OF INTEGER;
    VAR digits, d2: Digits; i, c: INTEGER;

    PROCEDURE Count(VAR digits: Digits);
        VAR ci, cc: INTEGER;
    BEGIN
        ci := 0; WHILE ci < len DO digits[ci] := 0; INC(ci) END;
        ReadByte(cc);
        WHILE cc # -1 DO
            IF (cc >= zero) & (cc <= nine) THEN
                INC(digits[cc - zero]);
            END;
            ReadByte(cc)
        END;
    END Count;

    PROCEDURE WriteStars(n: INTEGER);
        CONST MaxStars = 80;
        VAR i: INTEGER; overflow: BOOLEAN;
    BEGIN
        overflow := n > MaxStars;
        IF overflow THEN n := MaxStars END;
        i := 0; WHILE i < n DO WriteByte(star); INC(i) END;
        IF overflow THEN WriteByte(tilde) END
    END WriteStars;

BEGIN
    i := 0; WHILE i < len DO digits[i] := 0; INC(i) END;
    ReadByte(c);
    WHILE c # -1 DO
        IF (c >= zero) & (c <= nine) THEN
            INC(digits[c - zero]);
        END;
        ReadByte(c)
    END;
    (* Count(digits); *)
d2:=digits;
    i := 0;
    WHILE i < len DO
        Write(i); Write(d2[i]); WriteByte(tab); WriteStars(digits[i]); WriteLn;
        INC(i)
    END;
END CountDigitsHist.

