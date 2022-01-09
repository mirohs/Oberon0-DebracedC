MODULE M;
    VAR x, y, z: INTEGER;
BEGIN
    x := 1;
    IF x > 10 THEN x := 2
    ELSIF x > 5 THEN x := 1
    ELSE x := 0 END;
    Write(x);
    WriteLn;

    x := 100;
    WHILE x > 0 DO
        Write(x); WriteLn;
        x := x - 10
    END
END M.
