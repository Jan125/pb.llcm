EnableExplicit

DeclareModule LLCM
  Declare.s Compile(String.s)
EndDeclareModule

Module LLCM
  Procedure.s Error(Message.s, Position, Line, Column)
    ProcedureReturn "Compiler Error: " + Message + #CRLF$ + "Position: " + Str(Position) + ", Line: " + Str(Line) + ", Column: " + Str(Column) + #CRLF$ + "Compilation aborted."
  EndProcedure
  
  Structure Integer_Group_4
    a.i
    b.i
    c.i
    d.i
  EndStructure
  
  EnumerationBinary LLCM_TYPE 0
    #LLCM_TYPE_NON
    #LLCM_TYPE_LIS = 1
    #LLCM_TYPE_CMD
    #LLCM_TYPE_END
    
    #LLCM_TYPE_HEX
    #LLCM_TYPE_BIN
    
    
    #LLCM_TYPE_STR
    #LLCM_TYPE_NAM
    
    #LLCM_TYPE_I64
    #LLCM_TYPE_I32
    #LLCM_TYPE_I16
    #LLCM_TYPE_I08
    
    #LLCM_TYPE_U16
    #LLCM_TYPE_U08
    
    #LLCM_TYPE_F64
    #LLCM_TYPE_F32
    
    #LLCM_TYPE_PTR
  EndEnumeration
  
  Procedure.i TypePriority(Flags.i)
    If Flags & #LLCM_TYPE_LIS
      ProcedureReturn #LLCM_TYPE_LIS
    ElseIf Flags & #LLCM_TYPE_NAM
      ProcedureReturn #LLCM_TYPE_NAM
    ElseIf Flags & #LLCM_TYPE_STR
      ProcedureReturn #LLCM_TYPE_STR
    ElseIf Flags & #LLCM_TYPE_PTR
      ProcedureReturn #LLCM_TYPE_PTR
    ElseIf Flags & #LLCM_TYPE_F64
      ProcedureReturn #LLCM_TYPE_F64
    ElseIf Flags & #LLCM_TYPE_F32
      ProcedureReturn #LLCM_TYPE_F32
    ElseIf Flags & #LLCM_TYPE_I64
      ProcedureReturn #LLCM_TYPE_I64
    ElseIf Flags & #LLCM_TYPE_I32
      ProcedureReturn #LLCM_TYPE_I32
    ElseIf Flags & #LLCM_TYPE_I16
      ProcedureReturn #LLCM_TYPE_I16
    ElseIf Flags & #LLCM_TYPE_I08
      ProcedureReturn #LLCM_TYPE_I08
    ElseIf Flags & #LLCM_TYPE_U16
      ProcedureReturn #LLCM_TYPE_U16
    ElseIf Flags & #LLCM_TYPE_U08
      ProcedureReturn #LLCM_TYPE_U08
    EndIf
    ProcedureReturn #LLCM_TYPE_NON
  EndProcedure
  
  Procedure.i AssertIndexStructure(*String, List IndexList.Integer_Group_4(), Pattern.s = "")
    Protected a.i
    Protected b.i = 1
    Protected c.i
    Protected d.i
    Protected e.s
    
    PushListPosition(IndexList())
    For a = 0 To Len(Pattern) - 1
      If ListIndex(IndexList()) <> -1
        Select PeekC(@Pattern + a * SizeOf(Character))
          Case Asc("?")
            If c
              c = 0
            Else
              b = 0
              Break
            EndIf
            
          Case Asc(":")
            a + 1
            d = PeekC(@Pattern + a * SizeOf(Character))
            Repeat
              a + 1
              If PeekC(@Pattern + a * SizeOf(Character)) <> d
                e + Chr(PeekC(@Pattern + a * SizeOf(Character)))
              EndIf
            Until PeekC(@Pattern + a * SizeOf(Character)) = d
            d = 0
            
            If Not PeekS(*String + IndexList()\a * SizeOf(Character), IndexList()\b - IndexList()\a + 1) = e
              b = 0
              e = ""
              Break
            Else
              c = 1
            EndIf
            e = ""
            
          Case Asc("+")
            If Not NextElement(IndexList())
              b = 0
              Break
            EndIf
            c = 1
            
          Case Asc("-")
            If Not PreviousElement(IndexList())
              b = 0
              Break
            EndIf
            c = 1
            
          Case Asc("*")
            If IndexList()\c & #LLCM_TYPE_LIS
              d = IndexList()\b
              While NextElement(IndexList())
                If IndexList()\a > d
                  Break
                EndIf
              Wend
              If ListIndex(IndexList()) = -1
                b = 0
                Break
              EndIf
              c = 1
            Else
              b = 0
              Break
            EndIf
            
          Case Asc("l")
            If Not IndexList()\c & #LLCM_TYPE_LIS
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("c")
            If Not IndexList()\c & #LLCM_TYPE_CMD
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("e")
            If Not IndexList()\c & #LLCM_TYPE_END
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("h")
            If Not IndexList()\c & #LLCM_TYPE_HEX
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("b")
            If Not IndexList()\c & #LLCM_TYPE_BIN
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("s")
            If Not IndexList()\c & #LLCM_TYPE_STR
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("n")
            If Not IndexList()\c & #LLCM_TYPE_NAM
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("6")
            If Not IndexList()\c & #LLCM_TYPE_I64
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("3")
            If Not IndexList()\c & #LLCM_TYPE_I32
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("1")
            If Not IndexList()\c & #LLCM_TYPE_I16
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("8")
            If Not IndexList()\c & #LLCM_TYPE_I08
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("2")
            If Not IndexList()\c & #LLCM_TYPE_U16
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("9")
            If Not IndexList()\c & #LLCM_TYPE_U08
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("d")
            If Not IndexList()\c & #LLCM_TYPE_F64
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("f")
            If Not IndexList()\c & #LLCM_TYPE_F32
              b = 0
              Break
            Else
              c = 1
            EndIf
            
          Case Asc("p")
            If Not IndexList()\c & #LLCM_TYPE_PTR
              b = 0
              Break
            Else
              c = 1
            EndIf
            
        EndSelect
      Else
        b = 0
        Break
      EndIf
    Next
    PopListPosition(IndexList())
    ProcedureReturn b
  EndProcedure
  
  Procedure.s Compile(String.s)
    ;-Init
    Protected NewList IndexList.Integer_Group_4()
    ;a: Beginning
    ;b: End
    ;c: Type
    ;d: Depth
    
    Protected Finish.i = Len(String)
    Protected Index.i = 0
    Protected Count.i = 0
    Protected Depth.i = 0
    Protected Accumulate.i = 0
    
    Protected Line.i = 1
    Protected Column.i = 1
    
    Protected Pointer.i
    Protected Comparator.i
    
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
              ProcedureReturn Error("Misguided closing parenthesis ')'.", Index + 1, Line, Column)
            EndIf
          Else
            ClearList(IndexList())
            ProcedureReturn Error("Unintended closing parenthesis ')'.", Index + 1, Line, Column)
          EndIf
          
        Case Asc("]")
          ClearList(IndexList())
          ProcedureReturn Error("Unintended closing bracket ']'.", Index + 1, Line, Column)
          
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
                ProcedureReturn Error("Unclosed comment block ';'.", Count + 1, Line, Column)
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
                  ProcedureReturn Error("Misguided closing bracket ']'.", Index + 1, Line, Column)
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
                ProcedureReturn Error("Unclosed string block '['.", Count + 1, Line, Column)
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
                  ProcedureReturn Error("Misguided quotation marks '" + Chr(34) + "'.", Index + 1, Line, Column)
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
                ProcedureReturn Error("Unclosed string block '" + Chr(34) + "'.", Count + 1, Line, Column)
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
                  ProcedureReturn Error("Misguided quotation marks '''.", Index + 1, Line, Column)
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
                ProcedureReturn Error("Unclosed string block '''.", Count + 1, Line, Column)
            EndSelect
          EndIf
          
        Case Asc(",")
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
              Case Asc(" "), 9, 13, 10, Asc("("), Asc(")"), Asc("["), Asc("]"), Asc(";"), 34, Asc("'"), Asc(",")
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
        ProcedureReturn Error("Unclosed opening parenthesis '('.", Count + 1, Line, Column)
      Else
        ClearList(IndexList())
        ProcedureReturn Error("Unknown error. No diagnostics available.", -1, -1, -1)
      EndIf
    EndIf
    
    
    ;-Sort
    SortStructuredList(IndexList(), #PB_Sort_Ascending, OffsetOf(Integer_Group_4\a), TypeOf(Integer_Group_4\a))
    
    
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
          ProcedureReturn Error("Unfinished statement.", Count + 1, Line, Column)
      EndSelect
      
      
      Select PeekC(@String + IndexList()\a * SizeOf(Character))
        Case Asc("(")
          IndexList()\c | #LLCM_TYPE_LIS
          Pointer = IndexList()\b
          PushListPosition(IndexList())
          
          While NextElement(IndexList())
            If ListIndex(IndexList()) = ListSize(IndexList()) - 1
              IndexList()\c | #LLCM_TYPE_END
              
              Break 
            EndIf
            If IndexList()\a >= Pointer
              
              While PreviousElement(IndexList())
                IndexList()\c | #LLCM_TYPE_END
                If Not IndexList()\c & #LLCM_TYPE_LIS
                  IndexList()\c | #LLCM_TYPE_END
                  Break
                EndIf
              Wend
              Break
            EndIf
          Wend
          
          PopListPosition(IndexList())
          
          
          Continue
        Case Asc("["), Asc("'"), 34
          IndexList()\c | #LLCM_TYPE_STR
          Continue
        Case Asc("0") To Asc("9"), Asc("+"), Asc("-"), Asc(".") ;Numbers, plus, minus, and decimal
          Select PeekC(@String + IndexList()\a * SizeOf(Character))
            Case Asc("+"), Asc("-"), Asc(".")
              If IndexList()\a = IndexList()\b
                If PreviousElement(IndexList())
                  If IndexList()\c = #LLCM_TYPE_LIS
                    NextElement(IndexList())
                    IndexList()\c | #LLCM_TYPE_CMD
                  Else
                    NextElement(IndexList())
                  EndIf
                Else
                  IndexList()\c | #LLCM_TYPE_CMD
                EndIf
                IndexList()\c | #LLCM_TYPE_NAM
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
              If IndexList()\c = #LLCM_TYPE_LIS
                NextElement(IndexList())
                IndexList()\c | #LLCM_TYPE_CMD
              Else
                NextElement(IndexList())
              EndIf
            Else
              IndexList()\c | #LLCM_TYPE_CMD
            EndIf
            IndexList()\c | #LLCM_TYPE_NAM
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
                          If IndexList()\c = #LLCM_TYPE_LIS
                            NextElement(IndexList())
                            IndexList()\c | #LLCM_TYPE_CMD
                          Else
                            NextElement(IndexList())
                          EndIf
                        Else
                          IndexList()\c | #LLCM_TYPE_CMD
                        EndIf
                        IndexList()\c | #LLCM_TYPE_NAM
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
              IndexList()\c | #LLCM_TYPE_F32
              Continue
            Else
              IndexList()\c | #LLCM_TYPE_F64
              Continue
            EndIf
          Else
            If IndexList()\b - IndexList()\a >= 2
              Select PeekS(@String + IndexList()\a * SizeOf(Character), 2)
                Case "0x"
                  If IndexList()\b - IndexList()\a < 11
                    IndexList()\c | #LLCM_TYPE_I32
                    IndexList()\c | #LLCM_TYPE_HEX
                    Continue
                  Else
                    IndexList()\c | #LLCM_TYPE_I64
                    IndexList()\c | #LLCM_TYPE_HEX
                    Continue
                  EndIf
                Case "0b"
                  If IndexList()\b - IndexList()\a < 35
                    IndexList()\c | #LLCM_TYPE_I32
                    IndexList()\c | #LLCM_TYPE_BIN
                    Continue
                  Else
                    IndexList()\c | #LLCM_TYPE_I64
                    IndexList()\c | #LLCM_TYPE_BIN
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
                If IndexList()\c = #LLCM_TYPE_LIS
                  NextElement(IndexList())
                  IndexList()\c | #LLCM_TYPE_CMD
                Else
                  NextElement(IndexList())
                EndIf
              Else
                IndexList()\c | #LLCM_TYPE_CMD
              EndIf
              IndexList()\c | #LLCM_TYPE_NAM
              Continue
            EndIf
            
            
            If IndexList()\b - IndexList()\a < 12
              IndexList()\c | #LLCM_TYPE_I32
              Continue
            Else
              IndexList()\c | #LLCM_TYPE_I64
              Continue
            EndIf
          EndIf
          
        Default
          
          If PreviousElement(IndexList())
            If IndexList()\c = #LLCM_TYPE_LIS
              NextElement(IndexList())
              IndexList()\c | #LLCM_TYPE_CMD
            Else
              NextElement(IndexList())
            EndIf
          Else
            IndexList()\c | #LLCM_TYPE_CMD
          EndIf
          IndexList()\c | #LLCM_TYPE_NAM
          Continue
      EndSelect
    Next
    
    
    ;-Compile
    ForEach IndexList()
      If Not IndexList()\c & #LLCM_TYPE_LIS And IndexList()\c & #LLCM_TYPE_CMD

        ;         Select PeekS(@String + IndexList()\a * SizeOf(Character), IndexList()\b - IndexList()\a + 1)
        ;           Case "Function"
        ;             Debug AssertIndexStructure(@String, IndexList(), "n?:'Function'?+?l?+?n?e?+?n?:'Do'?+?l?*?")
        ;             Debug AssertIndexStructure(@String, IndexList(), "n?:'Function'?+?l?+?n?e?+?n?:'Do'?+?l?*? n?:'With'?+?l?*?")
        ;             Debug AssertIndexStructure(@String, IndexList(), "n?:'Function'?+?l?+?n?e?+?n?:'Do'?+?l?*? n?:'With'?+?l?*? n?:'As'+?l?*?")
        ;             
        ;         EndSelect
      EndIf
    Next
    ;Delay(30000)
    ;-Output
    Index = 0
    Count = 0
    ForEach IndexList()
      If Not IndexList()\c & #LLCM_TYPE_LIS
        Count + 1
        Debug Space(IndexList()\d * 2) + Str(IndexList()\a) + "_" + Str(IndexList()\b) + ":" + RSet(Bin(IndexList()\c), 14, "0") + ":" + PeekS(@String + IndexList()\a * SizeOf(Character), IndexList()\b - IndexList()\a + 1)
      EndIf
      
      
    Next
    
    Index = ListSize(IndexList())
    
    
    ClearList(IndexList())
    ProcedureReturn "Compiler Status: Compilation successful." + #CRLF$ + "Lexed " + Str(Count) + " token" + LSet("s", Bool(Count <> 1)) + ", " + Str(Index - Count) + " sublist" + LSet("s", Bool(Index - Count <> 1)) + "."
  EndProcedure
  
EndModule