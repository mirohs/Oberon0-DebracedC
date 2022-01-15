MODULE M;
    VAR x: INTEGER;
    PROCEDURE Double(VAR x: INTEGER);
        VAR y: INTEGER;
    BEGIN
        x := 2 * x;
    END Double;
BEGIN
    x := 123;
    Double(x);
END M.


(*

module M
    var x: int
    proc Double(var x: int)
        var y: int
    begin
        x := 2 * x
    end
begin
    x := 123
    Double(x)
end M

*)
