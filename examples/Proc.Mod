MODULE M;
    VAR x: INTEGER;
    PROCEDURE W(x: INTEGER);
    BEGIN
        Write(x);
        WriteLn;
    END W;
    PROCEDURE Double(VAR x: INTEGER);
    BEGIN
        x := 2 * x;
        W(x)
    END Double;
    PROCEDURE Mult(x, y: INTEGER);
    BEGIN
        Read(x);
        Read(y);
        W(x * y)
    END Mult;
    PROCEDURE MultInteractive;
        VAR x, y: INTEGER;
    BEGIN
        Read(x);
        Read(y);
        W(x * y)
    END MultInteractive;
BEGIN
    x := 123;
    Double(x);
    ASSERT(x = 246);
    W(x);
    (*Mult(3, 4)*)
    MultInteractive;
    MultInteractive
END M.
