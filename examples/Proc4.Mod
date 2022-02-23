MODULE M;
    VAR a: INTEGER;
    PROCEDURE P(x: INTEGER; VAR y: INTEGER);
    BEGIN 
        y := 10 * x
    END P;
BEGIN
    P(1, a);
    Write(a); WriteLn
END M.
