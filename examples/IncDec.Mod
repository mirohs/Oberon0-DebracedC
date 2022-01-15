MODULE M;
    VAR i: INTEGER;

    PROCEDURE f;
        VAR i: INTEGER;
    BEGIN
        i := 10;
        INC(i);
        Write(i); WriteLn;
        DEC(i);
        Write(i); WriteLn;

        i := 10;
        WHILE i <= 20 DO
            Write(i); WriteLn;
            INC(i)
        END;

        INC(i, 20);
        Write(i); WriteLn;
        INC(i, -20);
        Write(i); WriteLn;
        DEC(i, 20);
        Write(i); WriteLn;
        DEC(i, -20);
        Write(i); WriteLn();

        i := 100;
        INC(i, i + i);
        Write(i); WriteLn;
        i := 100;
        INC(i, -i);
        Write(i); WriteLn;
        i := 100;
        DEC(i, i + i);
        Write(i); WriteLn;
        i := 100;
        DEC(i, -i);
        Write(i); WriteLn;
    END f;

BEGIN
    i := 10;
    INC(i);
    Write(i); WriteLn;
    DEC(i);
    Write(i); WriteLn;

    i := 10;
    WHILE i <= 20 DO
        Write(i); WriteLn;
        INC(i)
    END;

    f;
    (* ASSERT((i < i) OR (i > i)); *)

    INC(i, 20);
    Write(i); WriteLn;
    INC(i, -20);
    Write(i); WriteLn;
    DEC(i, 20);
    Write(i); WriteLn;
    DEC(i, -20);
    Write(i); WriteLn();

    i := 100;
    INC(i, i + i);
    Write(i); WriteLn;
    i := 100;
    INC(i, -i);
    Write(i); WriteLn;
    i := 100;
    DEC(i, i + i);
    Write(i); WriteLn;
    i := 100;
    DEC(i, -i);
    Write(i); WriteLn;
END M.

