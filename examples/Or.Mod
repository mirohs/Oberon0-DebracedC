MODULE M;
    VAR a, b, c, d: BOOLEAN;
BEGIN
    a := FALSE;
    b := TRUE;
    (* c := a & b OR a & b; *)
    (* d := a & b & c; *)
    (* a := (a OR b) & (c OR d); *)
    (* a := a & (b OR c) & d; *)
    a := ~a;
    Write(a);
    WriteLn
END M.
