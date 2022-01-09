MODULE M;
    VAR i, j: INTEGER;
        a: BOOLEAN;
BEGIN
    (* i := (i + 1) * (j + 1) + i + j; *)
    a := i < j;
    IF ~a & ~a THEN
    (* IF (i < j) & ~(i < j) THEN *)
        Write(10)
    ELSE
        Write(20)
    END;
    WriteLn;

(*
    i := 2; j := 3; x := (i + i) * (j + j);
    IF x = (2 + 2) * (3 + 3) THEN Write(1); WriteLn ELSE Write (0); WriteLn END;
    IF x = 24 THEN Write(1); WriteLn ELSE Write (0); WriteLn END;

    i := 2; j := 3; x := (i + i) * (j + (i + i) * (j + j));
    IF x = (2 + 2) * (3 + 24) THEN Write(1); WriteLn ELSE Write (0); WriteLn END;
    IF x = 4 * 27 THEN Write(1); WriteLn ELSE Write (0); WriteLn END;

    a := TRUE; (* b := FALSE; (* c := a OR b; *) *)
    (* IF x = 4 * 27 THEN Write(1); WriteLn ELSE Write (0); WriteLn END; *)
*)
END M.

