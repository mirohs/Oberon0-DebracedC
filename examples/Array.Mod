MODULE M;
    CONST n = 3;
    VAR i: INTEGER; a, b: ARRAY n OF INTEGER; c: ARRAY n + 10 - 20 DIV 2 OF INTEGER;
    PROCEDURE F;
        VAR a, b: ARRAY n OF INTEGER;
    BEGIN
        a[0] := 100;
        a[1] := 200;
        a[2] := 300;
        b := a;
        INC(a[0]);
        Write(a[0]); WriteLn;
        Write(b[0]); WriteLn;
        Write(b[1]); WriteLn;
        Write(b[2]); WriteLn;
    END F;
BEGIN
    i := 0;
    a[i] := 100 + i;
    i := 1;
    a[i] := 100 + i;

    a[0] := 100;
    a[1] := 200;
    a[2] := 300;
    b := a;
    INC(a[0]);
    Write(a[0]); WriteLn;
    Write(b[0]); WriteLn;
    Write(b[1]); WriteLn;
    Write(b[2]); WriteLn;
    F;
    c := b;
END M.
