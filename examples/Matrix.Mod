(*
MODULE Matrix;
    CONST rows = 3; cols = 2;
    TYPE M = ARRAY rows OF ARRAY cols OF INTEGER;
         R = RECORD x, y: INTEGER END;
         AR = ARRAY 2 OF R;
    VAR m: M; i, j, x: INTEGER;
        ar: AR;
BEGIN
    x := 1;
    i := 0; WHILE i < rows DO
        j := 0; WHILE j < cols DO
            m[i][j] := x; INC(x);
            INC(j);
        END;
        INC(i);
    END;
    ar[0].x := x; INC(x);
    ar[0].y := x; INC(x);
    ar[1] := ar[0];
    (* ar[1].x := x; INC(x); *)
    (* ar[1].y := x; INC(x); *)
END Matrix.
*)

MODULE Matrix;

    PROCEDURE W(i: INTEGER); BEGIN Write(i); WriteLn END W;

    PROCEDURE P;
        CONST rows = 3; cols = 2;
        TYPE M = ARRAY rows OF ARRAY cols OF INTEGER;
             R = RECORD x, y: INTEGER END;
             AR = ARRAY 2 OF R;
        VAR m: M; i, j, x: INTEGER;
            ar: AR;
    BEGIN
        x := 1;
        i := 0; WHILE i < rows DO
            j := 0; WHILE j < cols DO
                m[i][j] := x; INC(x);
                INC(j);
            END;
            INC(i);
        END;
        ar[0].x := x; INC(x);
        ar[0].y := x; INC(x);
        ar[1] := ar[0];
        ar[1].x := x; INC(x); 
        ar[1].y := x; INC(x); 
        i := 0; WHILE i < rows DO
            j := 0; WHILE j < cols DO
                W(m[i][j]);
                INC(j);
            END;
            INC(i);
        END;
        W(ar[0].x);
        W(ar[0].y);
        W(ar[1].x);
        W(ar[1].y);
    END P;
BEGIN
    P
END Matrix.
