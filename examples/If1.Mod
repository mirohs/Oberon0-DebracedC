MODULE M;
    VAR x: INTEGER;
BEGIN
    x := 360;
    IF x > 10 THEN x := 100
    ELSIF x > 5 THEN x := 50
    ELSE x := 10 END;
    Write(10 * x + 500 DIV 10);
    WriteLn;
END M.
