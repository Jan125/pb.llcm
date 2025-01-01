EnableExplicit

Procedure.s LLCM_Error(Message.s, Position, Line, Column)
  ProcedureReturn "Compiler Error: " + Message + #CRLF$ + "Position: " + Str(Position) + ", Line: " + Str(Line) + ", Column: " + Str(Column) + #CRLF$ + "Compilation aborted."
EndProcedure


Structure TETRORDI
  a.i
  b.i
  c.i
  d.i
EndStructure

EnumerationBinary LLCM_TYPE 0
  #LLCM_TYPE_NONE
  #LLCM_TYPE_LIST = 1
  #LLCM_TYPE_COMMAND
  
  #LLCM_TYPE_UBYTE
  #LLCM_TYPE_CHARACTER
  #LLCM_TYPE_UWORD
  
  #LLCM_TYPE_BYTE
  #LLCM_TYPE_WORD
  #LLCM_TYPE_LONG
  #LLCM_TYPE_INTEGER
  #LLCM_TYPE_EPIC
  
  #LLCM_TYPE_FLOAT
  #LLCM_TYPE_DOUBLE
  
  #LLCM_TYPE_POINTER
  
  #LLCM_TYPE_STRING
  #LLCM_TYPE_NAME
EndEnumeration

Procedure.s LLCM_Compile(String.s)
  ;-Init
  Protected NewList IndexList.TETRORDI()
  ;a: Beginning
  ;b: End
  ;c: Type
  ;d: Depth
  
  Protected Finish.i = Len(String)
  Protected Index.i = 0
  Protected Count.i = 0
  Protected Depth.i = 0
  
  Protected Line.i = 1
  Protected Column.i = 1
  
  
  Protected NewList Declarations.s()
  Protected NewList Code.s()
  Protected NewList Resources.s()
  
  Protected NewList Output.s()
  
  ;-Lex
  While Index < Finish
    Select PeekC(@String + (Index * SizeOf(Character)))
      Case Asc("(")
        AddElement(IndexList())
        IndexList()\a = Index
        IndexList()\b = -1
        IndexList()\d = Depth
        Depth + 1
        
      Case Asc(")")
        If ListIndex(IndexList()) <> -1
          If PeekC(@String + IndexList()\a * SizeOf(Character)) = Asc("(")
            Depth - 1
            IndexList()\b = Index
            If Not PreviousElement(IndexList())
              ResetList(IndexList())
            EndIf
          Else
            ClearList(IndexList())
            ProcedureReturn LLCM_Error("Misguided closing parenthesis ')'.", Index + 1, Line, Column)
          EndIf
        Else
          ClearList(IndexList())
          ProcedureReturn LLCM_Error("Unintended closing parenthesis ')'.", Index + 1, Line, Column)
        EndIf
        
      Case Asc("]")
        ClearList(IndexList())
        ProcedureReturn LLCM_Error("Unintended closing bracket ']'.", Index + 1, Line, Column)
        
      Case Asc(";")
        Count = Index
        While Index < Finish
          Index + 1
          Column + 1
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case 10
              Line + 1
              Column = 1
            Case Asc(";")
              Break
          EndSelect
        Wend
        
        If Index = Finish
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case Asc(";")
            Default
              ClearList(IndexList())
              ProcedureReturn LLCM_Error("Unclosed comment block ';'.", Count + 1, Line, Column)
          EndSelect
        EndIf
        
      Case Asc("[")
        Count = Index
        AddElement(IndexList())
        IndexList()\a = Index
        IndexList()\d = Depth
        While Index < Finish
          Index + 1
          Column + 1
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case 10
              Line + 1
              Column = 1
            Case Asc("]")
              If ListIndex(IndexList()) <> -1
                IndexList()\b = Index
                If Not PreviousElement(IndexList())
                  ResetList(IndexList())
                EndIf
              Else
                ClearList(IndexList())
              ProcedureReturn LLCM_Error("Misguided closing bracket ']'.", Index + 1, Line, Column)
              EndIf
              Break
          EndSelect
        Wend
        
        If Index = Finish
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case Asc("]")
            Default
              Column - (Index - Count)
              ClearList(IndexList())
              ProcedureReturn LLCM_Error("Unclosed string block '['.", Count + 1, Line, Column)
          EndSelect
        EndIf
        
        
      Case 34 ;"
        Count = Index
        AddElement(IndexList())
        IndexList()\a = Index
        IndexList()\b = -1
        IndexList()\d = Depth
        While Index < Finish
          Index + 1
          Column + 1
          Select PeekC(@String + (Index * SizeOf(Character)))
              Case 10
              Line + 1
              Column = 1
            Case 34 ;"
              If ListIndex(IndexList()) <> -1
                IndexList()\b = Index
                If Not PreviousElement(IndexList())
              ResetList(IndexList())
            EndIf
              Else
                ClearList(IndexList())
                ProcedureReturn LLCM_Error("Misguided quotation marks '" + Chr(34) + "'.", Index + 1, Line, Column)
              EndIf
              Break
          EndSelect
        Wend
        
        If Index = Finish
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case 34 ;"
            Default
              Column - (Index - Count)
              ClearList(IndexList())
              ProcedureReturn LLCM_Error("Unclosed string block '" + Chr(34) + "'.", Count + 1, Line, Column)
          EndSelect
        EndIf
        
        
      Case Asc("'")
        Count = Index
        AddElement(IndexList())
        IndexList()\a = Index
        IndexList()\b = -1
        IndexList()\d = Depth
        While Index < Finish
          Index + 1
          Column + 1
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case 10
              Line + 1
              Column = 1
            Case Asc("'")
              If ListIndex(IndexList()) <> -1
                IndexList()\b = Index
                If Not PreviousElement(IndexList())
              ResetList(IndexList())
            EndIf
              Else
                ClearList(IndexList())
                ProcedureReturn LLCM_Error("Misguided quotation marks '''.", Index + 1, Line, Column)
              EndIf
              Break
          EndSelect
        Wend
        
        If Index = Finish
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case 34 ;"
            Default
              Column - (Index - Count)
              ClearList(IndexList())
              ProcedureReturn LLCM_Error("Unclosed string block '''.", Count + 1, Line, Column)
          EndSelect
        EndIf
        
      Case Asc(" "), 9
      Case 13
      Case 10
        Column = 0
        Line + 1
        
      Default
        AddElement(IndexList())
        IndexList()\a = Index
        IndexList()\b = -1
        IndexList()\d = Depth
        While Index < Finish
          Index + 1
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case Asc(" "), 9, 13, 10, Asc("("), Asc(")"), Asc("["), Asc("]"), Asc(";"), 34, Asc("'")
              Index - 1
              IndexList()\b = Index
              If Not PreviousElement(IndexList())
              ResetList(IndexList())
            EndIf
              Break
          EndSelect
          If Index = Finish
            Index - 1
            IndexList()\b = Index
            If Not PreviousElement(IndexList())
              ResetList(IndexList())
            EndIf
            Break
          EndIf
        Wend
        
    EndSelect
    Index + 1
    Column + 1
  Wend
  
  If Depth <> 0
    If ListIndex(IndexList()) <> -1
      Count = IndexList()\a
      ClearList(IndexList())
      Index = 0
      Line = 1
      Column = 1
      While Index < Count
        Select PeekC(@String + (Index * SizeOf(Character)))
          Case 10
            Line + 1
            Column = 1
            Break
        EndSelect
        Index + 1
        Column + 1
      Wend
      ProcedureReturn LLCM_Error("Unclosed opening parenthesis '('.", Count + 1, Line, Column)
    Else
      ClearList(IndexList())
        ProcedureReturn LLCM_Error("Unknown error. No diagnostics available", -1, -1, -1)
    EndIf
  EndIf
  
  
  ;-Sort
  SortStructuredList(IndexList(), #PB_Sort_Ascending, OffsetOf(TETRORDI\a), TypeOf(TETRORDI\a))
  
  
  ;-Type
  ForEach IndexList()
    ;Clear list when an invalid End value was discovered.
    ;This may happen when there is an index overrun.
    Select IndexList()\b
      Case -1
        Count = IndexList()\a
        ClearList(IndexList())
        Index = 0
        Line = 1
        Column = 1
        While Index < Count
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case 10
              Line + 1
              Column = 1
              Break
          EndSelect
          Index + 1
          Column + 1
        Wend
        ProcedureReturn LLCM_Error("Unfinished statement.", Count + 1, Line, Column)
    EndSelect
    
    
    Select PeekC(@String + IndexList()\a * SizeOf(Character))
      Case Asc("(")
        IndexList()\c | #LLCM_TYPE_LIST
        Continue
      Case Asc("["), Asc("'"), 34
        IndexList()\c | #LLCM_TYPE_STRING
        Continue
      Case Asc("0") To Asc("9"), Asc("+"), Asc("-"), Asc(".") ;Numbers, plus, minus, and decimal
        Select PeekC(@String + IndexList()\a * SizeOf(Character))
          Case Asc("+"), Asc("-"), Asc(".")
            If IndexList()\a = IndexList()\b
              If PreviousElement(IndexList())
                If IndexList()\c = #LLCM_TYPE_LIST
                  NextElement(IndexList())
                  IndexList()\c | #LLCM_TYPE_COMMAND
                Else
                  NextElement(IndexList())
                EndIf
              Else
                IndexList()\c | #LLCM_TYPE_COMMAND
              EndIf
              IndexList()\c | #LLCM_TYPE_NAME
              Continue
            EndIf
        EndSelect
        
        ;check illegals
        For Index = IndexList()\a To IndexList()\b
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case Asc("x")
              If Index = IndexList()\a + 1
                Continue
              Else
                Break
              EndIf
            Case Asc("+"), Asc("-")
              If Index = IndexList()\a
                Continue
              Else
                Break
              EndIf
            Case Asc("0") To Asc("9")
            Case Asc(".")
              If IndexList()\b - IndexList()\a >= 1
                Select PeekC(@String + (IndexList()\a + 1) * SizeOf(Character))
                  Case Asc("x")
                    Break
                EndSelect
              EndIf
            Case Asc("a") To Asc("f"), Asc("A") To Asc("F")
              If IndexList()\b - IndexList()\a >= 1
                Select PeekC(@String + (IndexList()\a + 1) * SizeOf(Character))
                  Case Asc("x")
                    Continue
                  Default
                    Break
                EndSelect
              EndIf
            Default
              Break
          EndSelect
        Next
        
        If Index <= IndexList()\b
          If PreviousElement(IndexList())
            If IndexList()\c = #LLCM_TYPE_LIST
              NextElement(IndexList())
              IndexList()\c | #LLCM_TYPE_COMMAND
            Else
              NextElement(IndexList())
            EndIf
          Else
            IndexList()\c | #LLCM_TYPE_COMMAND
          EndIf
          IndexList()\c | #LLCM_TYPE_NAME
          Continue
        EndIf
        ;end check illegals
        
        
        For Index = IndexList()\a To IndexList()\b
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case Asc(".")
              If Index < IndexList()\b
                Index + 1
                For Index = Index To IndexList()\b
                  Select PeekC(@String + (Index * SizeOf(Character)))
                    Case Asc(".")
                      If PreviousElement(IndexList())
                        If IndexList()\c = #LLCM_TYPE_LIST
                          NextElement(IndexList())
                          IndexList()\c | #LLCM_TYPE_COMMAND
                        Else
                          NextElement(IndexList())
                        EndIf
                      Else
                        IndexList()\c | #LLCM_TYPE_COMMAND
                      EndIf
                      IndexList()\c | #LLCM_TYPE_NAME
                      Index = -1
                      Break
                  EndSelect
                Next
              EndIf
              Break
          EndSelect
        Next
        
        If Index = -1
          Continue
        EndIf
        
        For Index = IndexList()\a To IndexList()\b
          Select PeekC(@String + (Index * SizeOf(Character)))
            Case Asc(".")
              Break
          EndSelect
        Next
        
        If Index <= IndexList()\b
          If IndexList()\b - Index < 15
            IndexList()\c | #LLCM_TYPE_FLOAT
            Continue
          Else
            IndexList()\c | #LLCM_TYPE_DOUBLE
            Continue
          EndIf
        Else
          If IndexList()\b - IndexList()\a >= 2
            Select PeekS(@String + IndexList()\a * SizeOf(Character), 2)
              Case "0x"
                If IndexList()\b - IndexList()\a < 11
                  IndexList()\c | #LLCM_TYPE_INTEGER
                  Continue
                Else
                  IndexList()\c | #LLCM_TYPE_EPIC
                  Continue
                EndIf
              Case "0b"
                If IndexList()\b - IndexList()\a < 35
                  IndexList()\c | #LLCM_TYPE_INTEGER
                  Continue
                Else
                  IndexList()\c | #LLCM_TYPE_EPIC
                  Continue
                EndIf
            EndSelect
          EndIf
          
          For Index = IndexList()\a To IndexList()\b
            Select PeekC(@String + (Index * SizeOf(Character)))
              Case Asc("0") To Asc("9"), Asc("+"), Asc("-")
              Default
                Break
            EndSelect
          Next
          
          If Index <= IndexList()\b
            If PreviousElement(IndexList())
              If IndexList()\c = #LLCM_TYPE_LIST
                NextElement(IndexList())
                IndexList()\c | #LLCM_TYPE_COMMAND
              Else
                NextElement(IndexList())
              EndIf
            Else
              IndexList()\c | #LLCM_TYPE_COMMAND
            EndIf
            IndexList()\c | #LLCM_TYPE_NAME
            Continue
          EndIf
          
          
          If IndexList()\b - IndexList()\a < 12
            IndexList()\c | #LLCM_TYPE_INTEGER
            Continue
          Else
            IndexList()\c | #LLCM_TYPE_EPIC
            Continue
          EndIf
        EndIf
        
      Default
        
        If PreviousElement(IndexList())
          If IndexList()\c = #LLCM_TYPE_LIST
            NextElement(IndexList())
            IndexList()\c | #LLCM_TYPE_COMMAND
          Else
            NextElement(IndexList())
          EndIf
        Else
          IndexList()\c | #LLCM_TYPE_COMMAND
        EndIf
        IndexList()\c | #LLCM_TYPE_NAME
        Continue
    EndSelect
  Next
  ;-Compile
  
  Index = 0
  Count = 0
  ForEach IndexList()
    If IndexList()\c <> 1
      Count + 1
      Debug Space(IndexList()\d * 2) + Str(IndexList()\a) + "_" + Str(IndexList()\b) + ":" + Str(IndexList()\c) + ":" + PeekS(@String + IndexList()\a * SizeOf(Character), IndexList()\b - IndexList()\a + 1)
    EndIf
  Next
  
  Index = ListSize(IndexList())
  
  ClearList(IndexList())
  ProcedureReturn "Compiler Status: Compilation successful." + #CRLF$ + "Lexed " + Str(Count) + " token" + LSet("s", Bool(Count <> 1)) + ", " + Str(Index - Count) + " sublist" + LSet("s", Bool(Index - Count <> 1)) + "."
EndProcedure
