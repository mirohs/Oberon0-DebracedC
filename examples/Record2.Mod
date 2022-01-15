MODULE M;
    TYPE Rec = RECORD
            x, y: INTEGER;
            z: RECORD a: INTEGER END;
         END;
         E = RECORD x: INTEGER END;
    (*VAR i: INTEGER; r: RECORD x, y: INTEGER END;*)
    VAR i: INTEGER; r: Rec; e0: E;
    PROCEDURE f;
         TYPE E = RECORD x: INTEGER END; int = INTEGER;
        VAR r: RECORD a, b: INTEGER END; e, f: E; i: int;
    BEGIN
        i := 123;

        r.a := 12;
        r.b := 34;
        Write(r.a); WriteLn;
        Write(r.b); WriteLn;
    END f;
    PROCEDURE g(VAR r: Rec);
    BEGIN
        r.x := 5;
        r.y := -6;
    END g;
    PROCEDURE h;
        VAR r: Rec;
    BEGIN
        r.x := 1;
        r.y := 2;
        Write(r.x); WriteLn;
        Write(r.y); WriteLn;
        g(r);
        Write(r.x); WriteLn;
        Write(r.y); WriteLn;
    END h;
BEGIN
    i := 100;
    r.x := i;
    r.y := i + 10;
    Write(r.x); WriteLn;
    Write(r.y); WriteLn;
    f;
    g(r);
    Write(r.x); WriteLn;
    Write(r.y); WriteLn;
    h;
END M.

(*
G_header: size = 0, entry = 59
87 instructions generated
  0 PSH  14 13  4
  1 PSH  12 13  4
  2 MOV  12  0 13
  3 SUBI 13 13  8
  4 MOVI  0  0 12
  5 STW   0 12 -8
  6 MOVI  0  0 34
  7 STW   0 12 -4
  8 LDW   0 12 -8
  9 WRD   0  0  0
 10 WRL   0  0  0
 11 LDW   0 12 -4
 12 WRD   0  0  0
 13 WRL   0  0  0
 14 MOV  13  0 12
 15 POP  12 13  4
 16 POP  14 13  4
 17 RET  14
 18 PSH  14 13  4
 19 PSH  12 13  4
 20 MOV  12  0 13
 21 SUBI 13 13  0
 22 LDW   0 12  8
 23 MOVI  1  0  5
 24 STW   1  0  0
 25 LDW   0 12  8
 26 MOVI  1  0 -6
 27 STW   1  0  4
 28 MOV  13  0 12
 29 POP  12 13  4
 30 POP  14 13  8
 31 RET  14
 32 PSH  14 13  4
 33 PSH  12 13  4
 34 MOV  12  0 13
 35 SUBI 13 13  8
 36 MOVI  0  0  1
 37 STW   0 12 -8
 38 MOVI  0  0  2
 39 STW   0 12 -4
 40 LDW   0 12 -8
 41 WRD   0  0  0
 42 WRL   0  0  0
 43 LDW   0 12 -4
 44 WRD   0  0  0
 45 WRL   0  0  0
 46 ADDI  0 12 -8
 47 PSH   0 13  4
 48 BSR  -30
 49 LDW   0 12 -8
 50 WRD   0  0  0
 51 WRL   0  0  0
 52 LDW   0 12 -4
 53 WRD   0  0  0
 54 WRL   0  0  0
 55 MOV  13  0 12
 56 POP  12 13  4
 57 POP  14 13  4
 58 RET  14
 59 MOVI 13  0 4096
 60 PSH  14 13  4
 61 MOVI  0  0 100
 62 STW   0 15 -252
 63 LDW   0 15 -256
 64 STW   0 15 -268
 65 LDW   0 15 -264
 66 ADDI  0  0 10
 67 STW   0 15 -276
 68 LDW   0 15 -284
 69 WRD   0  0  0
 70 WRL   0  0  0
 71 LDW   0 15 -292
 72 WRD   0  0  0
 73 WRL   0  0  0
 74 BSR  -74
 75 ADDI  0 15 -312
 76 PSH   0 13  4
 77 BSR  -59
 78 LDW   0 15 -324
 79 WRD   0  0  0
 80 WRL   0  0  0
 81 LDW   0 15 -332
 82 WRD   0  0  0
 83 WRL   0  0  0
 84 BSR  -52
 85 POP  14 13  4
 86 RET  14
RISC OUTPUT BEGIN
 100
 110
 12
 34
 5
 -6
 1
 2
 5
 -6
RISC OUTPUT END
 496:      0  497:      0  498:      0  499:      0  500:      0  501:      0  502:      0  503:      0 
 504:      0  505:      0  506:      0  507:      0  508:      0  509:      5  510:     -6  511:    100 
*)
