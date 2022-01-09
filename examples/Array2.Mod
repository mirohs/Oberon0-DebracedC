MODULE M;
    CONST n = 26;
    VAR i, j: INTEGER; a: ARRAY n OF INTEGER;
BEGIN
    i := 0;
    j := 123;
    i := (i + 1) * j - j * (i + i);
    i := 0;
    WHILE i < n DO
        a[i] := i + 65;
        i := i + 1
    END;
    i := 0;
    WHILE i < n DO
        WriteByte(a[i]);
        i := i + 1
    END;
(*
        WriteLn;
    ReadByte(i);
    WriteByte(i);
*)
    WriteLn;
END M.
