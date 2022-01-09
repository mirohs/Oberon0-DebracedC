MODULE M;
    TYPE Rec = RECORD x, y: INTEGER END;
         A = RECORD a: INTEGER; r: Rec END;
    VAR r, s: Rec; a: A;
BEGIN
    r.x := 10;
    r.y := 20;
    s := r;
    INC(s.x); INC(s.y);
    Write(s.x); WriteLn;
    Write(s.y); WriteLn;
    a.r.x := 1;
    a.r.y := 2;
    Write(a.r.x); WriteLn;
    Write(a.r.y); WriteLn;
END M.

(*
G_header: size = 0, entry = 0
8 instructions generated
  0 MOVI 13  0 4096
  1 PSH  14 13  4
  2 MOVI  0  0 10
  3 STW   0 15 -20
  4 MOVI  0  0 20
  5 STW   0 15 -24
  6 POP  14 13  4
  7 RET  14
RISC OUTPUT BEGIN
RISC OUTPUT END
 496:      0  497:      0  498:      0  499:      0  500:      0  501:      0  502:      0  503:      0 
 504:      0  505:      0  506:      0  507:      0  508:      0  509:      0  510:     10  511:     20 
*)
