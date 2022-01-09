MODULE M;
    VAR x: INTEGER;
    PROCEDURE W(x: INTEGER);
    BEGIN
        Write(x);
        WriteLn;
    END;
    PROCEDURE Double(VAR x: INTEGER);
    BEGIN
        x := 2 * x;
        W(x)
    END;
    PROCEDURE Mult(x, y: INTEGER);
    BEGIN
        Read(x);
        Read(y);
        W(x * y)
    END;
    PROCEDURE MultInteractive;
        VAR x, y: INTEGER;
    BEGIN
        Read(x);
        Read(y);
        W(x * y)
    END;
BEGIN
    x := 123;
    Double(x);
    W(x);
    (*Mult(3, 4)*)
    MultInteractive;
    MultInteractive
END M.
