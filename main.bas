$CONSOLE
'$DYNAMIC
TYPE LabelType
    name AS STRING
    line AS _UNSIGNED LONG
END TYPE

TYPE ProgramBufferType
    cmd AS STRING
    arg AS STRING
    ln AS STRING
    line AS _UNSIGNED LONG
END TYPE

TYPE VarType
    type AS STRING * 1
    val AS STRING
    name AS STRING
END TYPE

TYPE RegType
    A AS STRING
    B AS STRING
    C AS STRING
    D AS STRING
    E AS _BYTE
END TYPE

CONST __InternalStackCount = 2
CONST __InternalStack_Loops = 0
CONST __InternalStack_Scopes = 1
CONST __InternalStack_Subs = 2

DIM Labels(1) AS LabelType
DIM Scopes(1) AS STRING, Scopes AS _UNSIGNED INTEGER
DIM Vars(1, LBOUND(Scopes) TO UBOUND(Scopes)) AS VarType
DIM Stack(1, LBOUND(Scopes) TO UBOUND(Scopes)) AS STRING
DIM InternalStack(1, __InternalStackCount) AS LONG
DIM InternalStackPtr(__InternalStackCount) AS INTEGER
DIM StackPtr(LBOUND(Scopes) TO UBOUND(Scopes)) AS INTEGER
DIM Keywords(1 TO 3) AS STRING

DIM Scope AS INTEGER
DIM Reg AS RegType


DIM Prg(&HFFFFFF) AS ProgramBufferType
DIM i AS _UNSIGNED LONG, prgln AS _UNSIGNED LONG

DO
    prgln = prgln + 1
    LINE INPUT #f%, Prg(i).ln
    Prg(i).line = prgln

    Prg(i).ln = LTRIM$(RTRIM$(Prg(i).ln))

    IF LEN(Prg(i).ln) = 0 THEN _CONTINUE

    q~% = INSTR(Prg(i).ln, CHR$(34))
    WHILE q~%
        q2~% = INSTR(q~% + 1, Prg(i).ln, CHR$(34))
        IF q2~% = 0 THEN
            PRINT USING "On line ######: WARN: Cannot find closing quotation_! Placing one at the end of the line_."; prgln
            Prg(i).ln = Prg(i).ln + CHR$(34)
            _CONTINUE
        END IF
        q~% = q2~%
    WEND
    Prg(i).ln = RTRIM$(LEFT$(Prg(i).ln, INSTR(q~%, Prg(i).ln, "#")))

    IF ASC(Prg(i).ln) = 64 THEN '@ for labels
        NewLabel i, LTRIM$(MID$(Prg(i).ln, 2))
        _CONTINUE
    END IF

    l~%% = KeywordLen(Prg(i).ln)
    IF l~%% THEN
        Prg(i).cmd = LEFT$(Prg(i).ln, l~%%)
        Prg(i).arg = LTRIM$(MID$(Prg(i).ln, l~%% + 1))
        i = i + 1
    ELSE
        PRINT USING "On line ######: WARN: Cannot identify keyword_! Skipping_."; prgln
    END IF
LOOP UNTIL EOF(f%)
REDIM _PRESERVE Prg(i) AS ProgramBufferType

FOR i = LBOUND(prg) TO UBOUND(prg)
    SELECT CASE Prg(i).cmd
        CASE ":"
            i = GetLabel(Prg(i).arg)

        CASE ";": InternalPush i, __InternalStack_Subs
            i = GetLabel(Prg(i).arg)


        CASE "(": InternalPush i, __InternalStack_Loops
        CASE ")": i = InternalPull(__InternalStack_Loops)
        CASE ")?": IF Reg.E THEN i = InternalPull(__InternalStack_Loops)
        CASE ")!": IF Reg.E = 0 THEN i = InternalPull(__InternalStack_Loops)


        CASE "{": InternalPush Scope, __InternalStack_Scopes
            UseScope Prg(i).arg
        CASE "}": Scope = InternalPull(__InternalStack_Scopes)


        CASE "!": IF Reg.E = 0 THEN i = GetLabel(Prg(i).arg)
        CASE "?": IF Reg.E THEN i = GetLabel(Prg(i).arg)

        CASE "<-": PRINT ParseArg(Prg(i).arg);
        CASE "->":

        CASE ">"
        CASE "*"
        CASE "+"
        CASE "-"
        CASE "="

        CASE "s": UseScope Prg(i).arg
    END SELECT
NEXT

SUB NewLabel (ln AS _UNSIGNED LONG, n$)
    SHARED Labels() AS LabelType
    STATIC NextAutoLabel AS _UNSIGNED LONG

    IF n$ = "" THEN n$ = MKL$(NextAutoLabel): NextAutoLabel = NextAutoLabel + 1

    FOR i~% = LBOUND(labels) TO UBOUND(labels)
        IF Labels(i~%).name = n$ THEN Labels(i~%).line = ln: EXIT SUB
        IF Labels(i~%).name = "" THEN EXIT FOR
    NEXT
    IF i~% = UBOUND(labels) THEN
        i~% = i~% + 1
        REDIM _PRESERVE Labels(i~%) AS LabelType
    END IF
    Labels(i~%).name = n$
    Labels(i~%).line = ln
END SUB

FUNCTION GetLabel~& (n$)
    SHARED Labels() AS LabelType
    FOR i~% = LBOUND(labels) TO UBOUND(labels)
        IF Labels(i~%).name = n$ THEN GetLabel = Labels(i~%).line: EXIT FUNCTION
    NEXT
END FUNCTION

FUNCTION GetVar$ (Var$)
    SHARED Vars() AS VarType, Scope AS INTEGER
    FOR i~% = LBOUND(vars) TO UBOUND(vars)
        IF Vars(i~%, Scope).name = Var$ THEN GetVar = Vars(i~%, Scope).val: EXIT FUNCTION
        IF Vars(i~%, Scope).name = "" THEN EXIT FOR
    NEXT
    IF i~% = UBOUND(vars) THEN
        i~% = i~% + 1
        REDIM _PRESERVE Vars(i~%, LBOUND(vars, 2) TO UBOUND(vars, 2)) AS VarType
    END IF
    Vars(i~%, Scope).name = Var$
END FUNCTION

SUB SetVar (Var$, Val$)
    SHARED Vars() AS VarType, Scope AS INTEGER
    FOR i~% = LBOUND(vars) TO UBOUND(vars)
        IF Vars(i~%, Scope).name = Var$ THEN Vars(i~%, Scope).val = Val$: EXIT SUB
        IF Vars(i~%, Scope).name = "" THEN EXIT FOR
    NEXT
    IF i~% = UBOUND(vars) THEN
        i~% = i~% + 1
        REDIM _PRESERVE Vars(i~%, LBOUND(vars, 2) TO UBOUND(vars, 2)) AS VarType
    END IF
    Vars(i~%, Scope).name = Var$
    Vars(i~%, Scope).val = Val$
END SUB

FUNCTION ParseArg$ (s$)
    IF ASC(s$) = 35 THEN '#1, #&H5
        IF INSTR(s$, ".") THEN
            ParseArg = _MK$(_FLOAT, VAL(MID$(s$, 2)))
        ELSE
            ParseArg = _MK$(_INTEGER64, VAL(MID$(s$, 2)))
        END IF
        EXIT FUNCTION

    ELSEIF ASC(s$) = 46 THEN '.A, .D
        SHARED Reg AS RegType
        SELECT CASE ASC(s$, 2)
            CASE 65: ParseArg = Reg.A: EXIT FUNCTION '.A
            CASE 66: ParseArg = Reg.B: EXIT FUNCTION '.B
            CASE 67: ParseArg = Reg.C: EXIT FUNCTION '.C
            CASE 68: ParseArg = Reg.D: EXIT FUNCTION '.D
            CASE 69: ParseArg = TrueFalse(Reg.E): EXIT FUNCTION '.E
        END SELECT
    END IF

    'Parse quotes
    'Quotation is like this:
    '  "This is text"this is a variable"This is more text"
    q~% = INSTR(s$, CHR$(34))
    WHILE q~%
        var$ = MID$(s$, q2~% + 1, q~% - q2~%)
        IF var$ <> "" THEN o$ = o$ + GetVar(var$)
        q2~% = INSTR(q~%, s$, CHR$(34))
        o$ = o$ + ParseStr(MID$(s$, q2~% + 1, q~% - q2~%))
        q~% = INSTR(q2~%, s$, CHR$(34))
    WEND
    ParseArg$ = o$
END FUNCTION

FUNCTION ParseStr$ (s$)
    b~% = INSTR(s$, "\")
    WHILE b~%
        SELECT CASE ASC(s$, b~% + 1)
            CASE ASC("q")
                s$ = SubSt(s$, b~%, 2, CHR$(34))
            CASE ASC("n")
                s$ = SubSt(s$, b~%, 2, CHR$(13))
            CASE ASC("r")
                s$ = SubSt(s$, b~%, 2, CHR$(10))
            CASE ASC("\")
                s$ = SubSt(s$, b~%, 2, "\")
        END SELECT
        b~% = INSTR(b~%, s$, "\")
    WEND
    ParseStr = s$
END FUNCTION

FUNCTION SubSt$ (s$, s~%, l~%, sub$)
    SubSt$ = LEFT$(s$, s~%) + sub$ + MID$(s$, s~% + l~%)
END FUNCTION

SUB UseScope (scope$)
    SHARED Vars() AS VarType, Scopes() AS STRING, Stack() AS STRING, StackPtr() AS INTEGER
    SHARED Scope AS _UNSIGNED INTEGER
    FOR i~% = LBOUND(scopes) TO UBOUND(scopes)
        IF Scopes(i~%) = scope$ THEN Scope = i~%: EXIT SUB
        IF Scopes(i~%) = "" THEN EXIT FOR
    NEXT
    IF i~% = UBOUND(scopes) THEN
        i~% = i~% + 1
        REDIM _PRESERVE Vars(LBOUND(vars) TO UBOUND(vars), i~%) AS VarType
        REDIM _PRESERVE Scopes(i~%) AS STRING
        REDIM _PRESERVE Stack(LBOUND(stack) TO UBOUND(stack), i~%) AS STRING
        REDIM _PRESERVE StackPtr(i~%) AS INTEGER
    END IF
    Scopes(i~%) = scope$
    Scope = i~%
END SUB

SUB Push (Val$)
    SHARED Stack() AS STRING, StackPtr() AS INTEGER, Scopes() AS STRING
    SHARED Scope AS _UNSIGNED INTEGER
    StackPtr(Scope) = StackPtr(Scope) + 1
    IF StackPtr(Scope) >= UBOUND(stack) THEN
        REDIM _PRESERVE Stack(LBOUND(stack) TO StackPtr(Scope), LBOUND(scopes) TO UBOUND(scopes)) AS STRING
    END IF
    Stack(StackPtr(Scope), Scope) = Val$
END SUB

SUB InternalPush (V AS LONG, Stack AS _UNSIGNED _BYTE)
    SHARED InternalStack() AS LONG, InternalStackPtr() AS INTEGER

    InternalStackPtr(Stack) = InternalStackPtr(Stack) + 1

    IF InternalStackPtr(Stack) >= UBOUND(InternalStack) THEN
        REDIM _PRESERVE InternalStack(LBOUND(InternalStack) TO InternalStackPtr(Stack), __InternalStackCount) AS LONG
    END IF

    InternalStack(InternalStackPtr(Stack), Stack) = V
END SUB


FUNCTION Pull$
    SHARED Stack() AS STRING, StackPtr() AS INTEGER, Scopes() AS STRING
    SHARED Scope AS _UNSIGNED INTEGER
    StackPtr(Scope) = StackPtr(Scope) - 1
    IF StackPtr(Scope) <= LBOUND(stack) THEN
        REDIM _PRESERVE Stack(StackPtr(Scope) TO UBOUND(stack), LBOUND(scopes) TO UBOUND(scopes)) AS STRING
    END IF
    Pull = Stack(StackPtr(Scope), Scope)
END FUNCTION

FUNCTION InternalPull& (Stack AS _UNSIGNED _BYTE)
    SHARED InternalStack() AS LONG, InternalStackPtr() AS INTEGER

    InternalStackPtr(Stack) = InternalStackPtr(Stack) - 1

    IF InternalStackPtr(Stack) <= LBOUND(InternalStack) THEN
        REDIM _PRESERVE InternalStack(InternalStackPtr(Stack) TO UBOUND(InternalStack), __InternalStackCount) AS LONG
    END IF

    InternalPull = InternalStack(InternalStackPtr(Stack), Stack)
END FUNCTION



SUB InitKwdList
    SHARED Keywords() AS STRING
    'sorry for the garbage way of putting in comments, inline comments arent supported

    'and since this function will only be run once, it wouldnt really matter performance-wise.

    Keywords(1) =_
    " ( )" +                left$("Loop",0)          +_
    " : ;" +                left$("Goto/gosub",0)    +_
    " ? !" +                left$("If true goto/if false goto",0) +_
    " [ ]" +                left$("Def sub/return",0)+_
    " ." +                  left$("Misc functionality",0)+_
    " { }" +                left$("Define scope",0)  +_
    " s" +                  left$("Set scope",0)


    Keywords(2) =_
    " -> <-" +              left$("Print/Input",0)   +_
    _
    " ++ --" +              left$("Inc/Dec",0)       +_
    " A+ B+ C+ D+" +        left$("Add",0)           +_
    " A- B- C- D-" +        left$("Subtract",0)      +_
    _
    " A= B= C= D=" +        left$("Equals",0)        +_
    " A< B< C< D<" +        left$("Greater than",0)  +_
    " A> B> C> D>" +        left$("Less than",0)     +_
    _
    " )? )!" +              left$("Loop if eq/ne",0) +_
    _
    " s> s<" +              left$("Pop/push",0)      +_
    _
    " >> <<"


    Keywords(3) =_
    " A>= B>= C>= D>=" +    left$("Less than or equal to",0)    +_
    " A=< B=< C=< D=<" +    left$("Greater than or equal to",0) +_
    _
    " A>> B>> C>> D>>" +    left$("Put reg into var",0)         +_
    " A<< B<< C<< D<<" +    left$("Put var into reg",0)         +_
    _
        " tAB tAC tAD" +    left$("Transfer A",0)               +_
    " tBA"+ " tBC tBD" +    left$("Transfer B",0)               +_
    " tCA tCB"+ " tCD" +    left$("Transfer C",0)               +_
    " tDA tDB tDC"     +    left$("Transfer D",0)
END SUB

FUNCTION KeywordLen~%% (ln$)
    SHARED Keywords() AS STRING
    FOR i~%% = UBOUND(keywords) TO LBOUND(keywords) STEP -1
        IF INSTR(Keywords(i~%%), " " + LEFT$(ln$, i~%%)) THEN
            KeywordLen~%% = i~%%
            EXIT FUNCTION
        END IF
    NEXT
END FUNCTION

FUNCTION TrueFalse$ (Eq%%)
    IF Eq%% THEN TrueFalse = "TRUE" ELSE TrueFalse = "FALSE"
END FUNCTION
