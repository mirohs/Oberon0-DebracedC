MODULE QuickSort;
    CONST N = 10;
    TYPE Numbers = ARRAY N OF INTEGER;
    VAR i, z: INTEGER; a: Numbers;

    PROCEDURE W(i: INTEGER); BEGIN Write(i); WriteLn END W;

    PROCEDURE WriteNumbers;
        VAR i: INTEGER;
    BEGIN
        i := 0; WHILE i < N DO Write(a[i]); INC(i) END;
        WriteLn
    END WriteNumbers;

    PROCEDURE Swap(VAR x, y: INTEGER);
        VAR h: INTEGER; 
    BEGIN h := x; x := y; y := h END Swap;

    PROCEDURE Random(VAR result: INTEGER; low, high: INTEGER);
        CONST a = 16807; m = 83647; q = m DIV a; r = m MOD a;
        VAR gamma: INTEGER;
    BEGIN
        gamma := a * (z MOD q) - r * (z DIV q);
        IF gamma > 0 THEN z := gamma
        ELSE z := gamma + m
        END;
        result := low + z MOD (high - low)
    END Random;

    PROCEDURE Sort(VAR a: Numbers; low, high: INTEGER);
        VAR ipivot, pivot, i, j: INTEGER;
    BEGIN
        IF low < high THEN
            Random(ipivot, low, high);
            pivot := a[ipivot];
            Swap(a[low], a[ipivot]);
            i := low + 1; j := low + 1;
            WHILE i <= high DO
                IF a[i] < pivot THEN Swap(a[i], a[j]); INC(j) END;
                INC(i);
            END;
            (* assert: a[low] = pivot, a[low+1..j-1] < pivot, a[j..high] >= pivot *)
            (* assert: j >= low + 1, i == high + 1 *)
            Swap(a[low], a[j-1]);
            (* assert: a[low..j-2] < pivot, a[j-1] = pivot, a[j..high] >= pivot *)
            (* assert: j - 2 >= low - 1, j <= high + 1 *)
            Sort(a, low, j-2);
            Sort(a, j, high);
        END;
    END Sort;

BEGIN
    z := 114359; (* random seed *)
    i := 0; WHILE i < N DO Random(a[i], 0, 100); INC(i) END;
    WriteNumbers;
    Sort(a, 0, N - 1);
    WriteNumbers;
END QuickSort.
