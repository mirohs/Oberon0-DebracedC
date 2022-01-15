MODULE CountDigits;
CONST zero = 48;
      nine = 57;
      len = nine - zero + 1;
VAR digits: ARRAY len OF INTEGER;
    i, c: INTEGER;
    PROCEDURE Count;
        VAR digits: ARRAY len OF INTEGER;
            i, c: INTEGER;
    BEGIN
        i := 0;
        WHILE i < len DO digits[i] := 0; INC(i) END;
        ReadByte(c);
        WHILE c # -1 DO
            IF (c >= zero) & (c <= nine) THEN
                INC(digits[c - zero]);
            END;
            ReadByte(c)
        END;
        i := 0;
        WHILE i < len DO
            Write(i); Write(digits[i]); WriteLn;
            INC(i)
        END
    END Count;
BEGIN
    i := 0;
    WHILE i < len DO digits[i] := 0; INC(i) END;
    ReadByte(c);
    WHILE c # -1 DO
        IF (c >= zero) & (c <= nine) THEN
            INC(digits[c - zero]);
        END;
        ReadByte(c)
    END;
    i := 0;
    WHILE i < len DO
        Write(i); Write(digits[i]); WriteLn;
        INC(i)
    END;
    (* Count; *)
END CountDigits.

(*
    proc count
        var a: array 10 of real
        var r: record x: int, y: int end
            i, c: int
    {
        i := 0
        while i < len { digits[i] := 0; inc(i) }
        read_byte(c)
        while c # -1 {
            if (c >= zero) & (c <= nine) {
                inc(digits[c - zero])
            }
            read_byte(c)
        }
        i := 0
        while i < len {
            write(i); write(digits[i]); writeln
            inc(i)
        }
    }

    Proc Count
        Var digits: Arr len of Int
            i, c: Int
    {
        i := 0
        While i < len { digits[i] := 0; Inc(i) }
        ReadByte(c)
        While c # -1 {
            If (c >= zero) & (c <= nine) {
                Inc(digits[c - zero])
            }
            ReadByte(c)
        }
        i := 0
        While i < len {
            Write(i); Write(digits[i]); WriteLn
            Inc(i)
        }
    }


    proc Count
        var digits: arr len of int
            i, c: int
    is
        i := 0
        while i < len do digits[i] := 0; Inc(i)
        ReadByte(c)
        while c # -1 do
            if (c >= zero) & (c <= nine)
                Inc(digits[c - zero])
            ReadByte(c)
        i := 0
        while i < len
            Write(i); Write(digits[i]); WriteLn
            Inc(i)

    proc Count
        var digits: arr len of int
            i, c: int
    begin
        i := 0
        while i < len do digits[i] := 0; Inc(i)
        ReadByte(c)
        while c # -1 do
            if (c >= zero) & (c <= nine) then
                Inc(digits[c - zero])
            ReadByte(c)
        i := 0
        while i < len
            Write(i); Write(digits[i]); WriteLn
            Inc(i)
    end


    proc Count
        var digits: arr len of int
            i, c: int
    begin
        i := 0
        while i < len: digits[i] := 0; Inc(i)
        ReadByte(c)
        while c # -1:
            if (c >= zero) & (c <= nine):
                Inc(digits[c - zero])
            ReadByte(c)
        i := 0
        while i < len:
            Write(i); Write(digits[i]); WriteLn
            Inc(i)
    end

    (* Pyberon *)
    proc count
        const N = 100
        type R: record x, y: int
        type S: record 
            x, y: int
            digits: array len of int
        var digits: array len of int
        var i, c: int

        const N = 100
        type R: rec x, y: int
        var digits: arr len of int
        var i, c: int
    begin
        i := 0
        while i < len: digits[i] := 0; inc(i)
        read_byte(c)
        while c # -1:
            if (c >= zero) & (c <= nine):
                inc(digits[c - zero])
            read_byte(c)
        i := 0
        while i < len:
            write(i); write(digits[i]); writeln
            inc(i)
    end

    proc f(var r: real; x, y: real)
        const k = 100
    begin
        r = k * x * y
    end

    proc f(var r: real; x, y: real)
    begin r = 100 * x * y end

    PROCEDURE Count;
        VAR digits: ARRAY len OF INTEGER;
            i, c: INTEGER;
    BEGIN
        i := 0;
        WHILE i < len DO digits[i] := 0; INC(i) END;
        ReadByte(c);
        WHILE c # -1 DO
            IF (c >= zero) & (c <= nine) THEN
                INC(digits[c - zero]);
            END;
            ReadByte(c)
        END;
        i := 0;
        WHILE i < len DO
            Write(i); Write(digits[i]); WriteLn;
            INC(i)
        END
    END;

*)
