MODULE M;
    CONST n = 2;
    TYPE ARR = ARRAY n OF INTEGER;
    VAR i: INTEGER; a: ARR;
BEGIN
    i := 0;
    a[i] := 10;
    INC(i);
    a[i] := 100;
    Write(a[0]); Write(a[1]); WriteLn
END M.
