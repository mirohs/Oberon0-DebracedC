MODULE M;
    VAR a: INTEGER;

    PROCEDURE W(x: INTEGER);
    BEGIN Write(x); WriteLn END;

    PROCEDURE Fac(VAR f: INTEGER; x: INTEGER);
    BEGIN
        IF x > 1 THEN f := f * x; Fac(f, x - 1) END
    END;

    PROCEDURE Fac2(x: INTEGER);
    BEGIN
        IF x > 1 THEN a := a * x; Fac(a, x - 1) END
    END;
BEGIN
    a := 1;
    Fac(a, 5);
    W(a);
    a := 1;
    Fac2(5);
    W(a)
END M.
