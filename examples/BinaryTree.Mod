MODULE BinaryTree;
    CONST M = 100; (* memory size (number of objects) *)
        NULL = 0;
    TYPE int = INTEGER; (* just an abbreviation *)
        NodePtr = int; (* our "pointer" type *)
        Node = RECORD (* one node of a binary tree *)
            value: int;
            left, right: NodePtr;
        END;
        Memory = ARRAY M OF Node; (* memory consists of M Node records *)
        FreeList = ARRAY M OF NodePtr; (* maintains "pointers" of free memory locations *)
    VAR deref: Memory; freelist: FreeList;
        z: int; (* state of the random number generator *)

    (* Generates a "random" integer between low (inclusive) and high (exclusive). *)
    PROCEDURE Random(VAR result: int; low, high: int);
        CONST a = 16807; m = 83647; q = m DIV a; r = m MOD a;
        VAR gamma: INTEGER;
    BEGIN
        ASSERT(low < high);
        gamma := a * (z MOD q) - r * (z DIV q);
        IF gamma > 0 THEN z := gamma
        ELSE z := gamma + m
        END;
        result := low + z MOD (high - low)
    END Random;

    (* Initializes the freelist such that each element points to its
       successor, except the last one, whicht points to NULL. *)
    PROCEDURE InitFreeList;
        VAR i: int;
    BEGIN
        i := 0;
        WHILE i < M - 1 DO
            freelist[i] := i + 1;
            INC(i)
        END;
        freelist[M - 1] := 0;
    END InitFreeList;

    (* Allocates memory for a new node object. *)
    PROCEDURE new(VAR p: NodePtr);
    BEGIN
        p := freelist[0]; (* return "pointer" to first free node *)
        ASSERT(p # NULL);
        freelist[0] := freelist[p]; (* advance to next free node *)
        freelist[p] := -1; (* mark p as in use, not strictly required *)
    END new;

    (* Frees the memory of the node object. *)
    PROCEDURE free(p: NodePtr);
    BEGIN
        freelist[p] := freelist[0]; (* p's successor is the old front of the freelist *)
        freelist[0] := p; (* insert p at the front of the freelist *)
    END free;

    (* Add x into binary search tree. *)
    PROCEDURE add(VAR tree: NodePtr; x: int);
        VAR p: NodePtr;
    BEGIN
        IF tree = NULL THEN
            new(p);
            deref[p].value := x;
            deref[p].left := NULL;
            deref[p].right := NULL;
            tree := p;
        ELSIF x < deref[tree].value THEN
            add(deref[tree].left, x)
        ELSE
            add(deref[tree].right, x)
        END;
    END add;
    (*
    PROCEDURE print(VAR tree: NodePtr);
    BEGIN
        IF tree # NULL THEN
            print(deref[tree].left);
            Write(deref[tree].value);
            print(deref[tree].right);
        END;
    END;
    *)
    PROCEDURE print(VAR tree: NodePtr);
        VAR node: Node;
    BEGIN
        IF tree # NULL THEN
            node := deref[tree];
            print(node.left);
            Write(node.value);
            print(node.right);
        END;
    END print;

    (* Free all of the dynamically allocated nodes of the tree. *)
    PROCEDURE FreeTree(VAR tree: NodePtr);
        VAR node: Node;
    BEGIN
        IF tree # NULL THEN
            node := deref[tree];
            FreeTree(node.left);
            FreeTree(node.right);
            free(tree);
        END;
    END FreeTree;

    PROCEDURE test;
        VAR t, t1, t2, t3, t4: NodePtr; i, x: int;
    BEGIN
        (*
        new(t1); W(t1);
        new(t2); W(t2);
        new(t3); W(t3);
        free(t2);
        new(t4); W(t4);
        deref[t4].value := 4;
        *)
        t := NULL;
        i := 0; WHILE i < 20 DO
            Random(x, 10, 100); add(t, x);
            INC(i)
        END;
        print(t); WriteLn;

        FreeTree(t);
        (* free(deref[t].left); free(t); *)
        x := 0; i := 0;
        WHILE i < M DO 
            IF freelist[i] = -1 THEN INC(x) END;
            INC(i);
        END;
        Write(x); WriteLn;
    END test;

BEGIN
    z := 114352; (* random seed *)
    InitFreeList;
    test;
END BinaryTree.
