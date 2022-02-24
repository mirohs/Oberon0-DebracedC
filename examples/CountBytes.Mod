MODULE CountBytes;
    VAR c, n: INTEGER; a, b: BOOLEAN;
BEGIN
    a := TRUE; b := TRUE;
    n := 0;
    ReadByte(c);
    WHILE c # -1 DO
        INC(n);
        ReadByte(c)
    END;
    Write(n); WriteLn;
    ASSERT(a & b OR (c < n) OR b);
END CountBytes.

