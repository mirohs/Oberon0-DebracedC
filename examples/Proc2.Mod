(* p. 78 *)

MODULE M;

    VAR a, b: INTEGER;

    PROCEDURE P(x: INTEGER; VAR y: INTEGER);
    BEGIN 
        x := y;
        y := x;
        P(x, y); (* endless recursion *)
        P(y, x)
    END P;

BEGIN
    a := 1; b := 2;
    P(a, b)
END M.
