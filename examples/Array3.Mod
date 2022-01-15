MODULE M;
    CONST n = 2;
    TYPE ARR = ARRAY n OF INTEGER;
    VAR i: INTEGER; a: ARR; (* ARRAY n OF INTEGER; *)

(*
    PROCEDURE f;
        VAR fi: INTEGER; fa: ARRAY n OF INTEGER;
    BEGIN
        fi := 0;
        WHILE fi < n DO
            fa[fi] := fi + 97;
            fi := fi + 1
        END;
        fi := 0;
        WHILE fi < n DO
            WriteByte(fa[fi]);
            fi := fi + 1
        END;
        WriteLn;
    END;
*)

    (*PROCEDURE g(VAR arr: ARRAY n OF INTEGER);*)
    (*PROCEDURE g(arr: ARR);*)
    PROCEDURE g(VAR arr: ARR);
        VAR gx: INTEGER;
    BEGIN
        gx := 0;
        Write(arr[gx]);
        WriteLn;
    END g;


BEGIN
(*
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
    WriteLn;
    f;
*)
    a[0] := 100;
    g(a);
END M.

(*

G_header: size = 0, entry = 18
class == Par
r = 0
27 instructions generated
  0 PSH  14 13  4
  1 PSH  12 13  4
  2 MOV  12  0 13
  3 SUBI 13 13  4      local var gx
  4 MOVI  0  0  0
  5 STW   0 12 -4      gx := 0
  6 LDW   0 12  8      R0 := a
  7 LDW   1 12 -4      gx
  8 CHKI  1  0  2
  9 MULI  1  1  4      4 * gx
 10 ADD   1  0  1      4 * gx + a
 11 LDW   0  1  0      R0 := M[a + 4 * gx]
 12 WRD   0  0  0
 13 WRL   0  0  0
 14 MOV  13  0 12
 15 POP  12 13  4
 16 POP  14 13  8
 17 RET  14
 18 MOVI 13  0 4096
 19 PSH  14 13  4
 20 MOVI  0  0 100
 21 STW   0 15 -96
 22 ADDI  0 15 -100    effective address of a
 23 PSH   0 13  4      push address of a
 24 BSR  -24
 25 POP  14 13  4
 26 RET  14
RISC OUTPUT BEGIN
 0
RISC OUTPUT END
 496:      0  497:      0  498:      0  499:      0  500:      0  501:      0  502:      0  503:      0 
 504:      0  505:      0  506:      0  507:      0  508:      0  509:    100  510:      0  511:      0 

*)
