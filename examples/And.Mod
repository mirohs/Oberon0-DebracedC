MODULE M;
    VAR a, b, c: BOOLEAN;
BEGIN
    a := FALSE;
    b := TRUE;
    c := a & b OR TRUE;
    Write(c);
    WriteLn
END M.
