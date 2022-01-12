MODULE CountBytes;
    VAR c, n: INTEGER;
BEGIN
    n := 0;
    ReadByte(c);
    WHILE c # -1 DO
        INC(n);
        ReadByte(c)
    END;
    Write(n); WriteLn
END CountBytes.

