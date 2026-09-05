EnableExplicit

DeclareModule LLCM
  Declare.s Evaluate(String.s)
EndDeclareModule

Module LLCM
  
  Procedure.s Error(Message.s, Position, Line, Column)
    ProcedureReturn "Compiler Error: " + Message + #CRLF$ +
                    "Position: " + Str(Position) + ", Line: " + Str(Line) + ", Column: " + Str(Column) + #CRLF$ +
                    "Compilation aborted."
  EndProcedure
  
  Structure LLCM_TOKEN
    Start.i
    Stop.i
    Type.i
    List Children.LLCM_TOKEN()
    *Data
  EndStructure
  
  EnumerationBinary LLCM_TYPE 0
    #LLCM_TYPE_NON
    #LLCM_TYPE_LIS = 1
    #LLCM_TYPE_BEG
    #LLCM_TYPE_END
    
    #LLCM_TYPE_STR
    #LLCM_TYPE_NAM
    
    #LLCM_TYPE_INT
    
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
  
  #LLCM_REGEX_INTEGER = "^[+-]*\d+$" ;Only numbers and an optional +- at the beginning.
  #LLCM_REGEX_FLOAT = "^[+-]*\d*\.\d*$" ;Only optional numbers and an optional +- at the beginning, seperated by a single . .
  #LLCM_REGEX_HEXADECIMAL = "^0[xXhH][\da-fA-F]*$"
  #LLCM_REGEX_BINARY = "^0[bB][01]*$"
  
  Procedure.i RegExFastMatch(RegEx.s, String.s)
    Protected RegExID.i = CreateRegularExpression(#PB_Any, RegEx)
    Protected ReturnValue.i
    If RegExID
      ReturnValue = MatchRegularExpression(RegExID, String)
      FreeRegularExpression(RegExID)
      ProcedureReturn ReturnValue
    EndIf
    ProcedureReturn #False
  EndProcedure
  
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
  
  
  Procedure.s Evaluate(String.s)
    ;-Init
    Protected TokenList.LLCM_TOKEN
    Protected *CurrentToken.LLCM_TOKEN = @TokenList
    
    Protected NewList *TokenAddressStack()
    
    Protected Finish.i = Len(String)
    Protected Index.i = 0
    Protected Count.i = 0
    
    Protected Line.i = 1
    Protected Column.i = 1
    
    ;-Lex
    While Index < Finish
      Select PeekC(@String + (Index * SizeOf(Character)))
        Case Asc("(")
          AddElement(*CurrentToken\Children())
          AddElement(*TokenAddressStack())
          *TokenAddressStack() = *CurrentToken
          *CurrentToken = @*CurrentToken\Children()
          *CurrentToken\Start = Index
          
        Case Asc(")")
          If *CurrentToken = @TokenList
            ProcedureReturn Error("Unintended closing parenthesis ')'.", Index + 1, Line, Column)
          Else
            ResetList(*CurrentToken\Children())
            *CurrentToken\Stop = Index
            *CurrentToken = *TokenAddressStack()
            DeleteElement(*TokenAddressStack())
          EndIf
          
        Case Asc("]")
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
                ProcedureReturn Error("Unclosed comment block ';'.", Count + 1, Line, Column)
            EndSelect
          EndIf
          
        Case Asc("[")
          Count = Index
          AddElement(*CurrentToken\Children())
          *CurrentToken\Children()\Start = Index
          While Index < Finish
            Index + 1
            Column + 1
            Select PeekC(@String + (Index * SizeOf(Character)))
              Case 10
                Line + 1
                Column = 1
              Case Asc("]")
                *CurrentToken\Children()\Stop = Index
                Break
            EndSelect
          Wend
          
          If Index = Finish
            Select PeekC(@String + (Index * SizeOf(Character)))
              Case Asc("]")
                *CurrentToken\Children()\Stop = Index
              Default
                Column - (Index - Count)
                ProcedureReturn Error("Unclosed string block '['.", Count + 1, Line, Column)
            EndSelect
          EndIf
          
        Case 34 ;"
          Count = Index
          AddElement(*CurrentToken\Children())
          *CurrentToken\Children()\Start = Index
          While Index < Finish
            Index + 1
            Column + 1
            Select PeekC(@String + (Index * SizeOf(Character)))
              Case 10
                Line + 1
                Column = 1
              Case 34 ;"
                *CurrentToken\Children()\Stop = Index
                Break
            EndSelect
          Wend
          
          If Index = Finish
            Select PeekC(@String + (Index * SizeOf(Character)))
              Case 34 ;"
                *CurrentToken\Children()\Stop = Index
              Default
                Column - (Index - Count)
                ProcedureReturn Error("Unclosed string block '" + Chr(34) + "'.", Count + 1, Line, Column)
            EndSelect
          EndIf
          
        Case Asc("'")
          Count = Index
          AddElement(*CurrentToken\Children())
          *CurrentToken\Children()\Start = Index
          While Index < Finish
            Index + 1
            Column + 1
            Select PeekC(@String + (Index * SizeOf(Character)))
              Case 10
                Line + 1
                Column = 1
              Case Asc("'")
                *CurrentToken\Children()\Stop = Index
                Break
            EndSelect
          Wend
          
          If Index = Finish
            Select PeekC(@String + (Index * SizeOf(Character)))
              Case Asc("'")
                *CurrentToken\Children()\Stop = Index
              Default
                Column - (Index - Count)
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
          AddElement(*CurrentToken\Children())
          *CurrentToken\Children()\Start = Index
          While Index < Finish
            Index + 1
            Column + 1
            Select PeekC(@String + (Index * SizeOf(Character)))
              Case Asc(" "), 9, 13, 10, Asc("("), Asc(")"), Asc("["), Asc("]"), Asc(";"), 34, Asc("'"), Asc(",")
                Index - 1
                Column - 1
                *CurrentToken\Children()\Stop = Index
                Break
            EndSelect
          Wend
          If Index = Finish
            Index - 1
            *CurrentToken\Children()\Stop = Index
            Break
          EndIf
          
      EndSelect
      Index + 1
      Column + 1
    Wend
    
    If ListSize(*TokenAddressStack())
      Index = 0
      Line = 1
      Column = 1
      While Index < *CurrentToken\Start
        Select PeekC(@String + (Index * SizeOf(Character)))
          Case 10
            Column = 0
            Line + 1
        EndSelect
        Index + 1
        Column + 1
      Wend
      ProcedureReturn Error("Unclosed opening parenthesis '('.", *CurrentToken\Start + 1, Line, Column)
    EndIf
    
    ResetList(*CurrentToken\Children())
    
    ;-Type
    Repeat
      If *CurrentToken <> @TokenList
        Select PeekC(@String + (*CurrentToken\Start * SizeOf(Character)))
          Case Asc("(")
            *CurrentToken\Type | #LLCM_TYPE_LIS
          Case Asc("["), Asc("'"), 34
            *CurrentToken\Type | #LLCM_TYPE_STR
            *CurrentToken\Data = AllocateMemory((*CurrentToken\Stop - *CurrentToken\Start) * SizeOf(Character))
            CopyMemory(@String + (*CurrentToken\Start + 1 ) * SizeOf(Character), *CurrentToken\Data, (*CurrentToken\Stop - *CurrentToken\Start) * SizeOf(Character))
          Case Asc("0") To Asc("9"), Asc("+"), Asc("-"), Asc(".") ;Numbers, plus, minus, and decimal
            If RegExFastMatch(#LLCM_REGEX_INTEGER, PeekS(@String + (*CurrentToken\Start * SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start + 1))
              If #PB_Compiler_64Bit Or Bool(Val(PeekS(@String + (*CurrentToken\Start * SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start + 1)) & $FFFFFFFF <> Val(PeekS(@String + (*CurrentToken\Start * SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start + 1)))
                *CurrentToken\Type | #LLCM_TYPE_I64
                *CurrentToken\Data = AllocateMemory(SizeOf(Quad))
                PokeQ(*CurrentToken\Data, Val(PeekS(@String + (*CurrentToken\Start * SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start + 1)))
              Else
                *CurrentToken\Type | #LLCM_TYPE_I32
                *CurrentToken\Data = AllocateMemory(SizeOf(Long))
                PokeL(*CurrentToken\Data, Val(PeekS(@String + (*CurrentToken\Start * SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start + 1)))
              EndIf
            ElseIf RegExFastMatch(#LLCM_REGEX_HEXADECIMAL, PeekS(@String + (*CurrentToken\Start * SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start + 1))
              If #PB_Compiler_64Bit Or *CurrentToken\Stop - *CurrentToken\Start - 1 > 8
                *CurrentToken\Type | #LLCM_TYPE_I64
                *CurrentToken\Data = AllocateMemory(SizeOf(Quad))
                PokeQ(*CurrentToken\Data, Val("$" + PeekS(@String + ((*CurrentToken\Start + 2)* SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start - 1)))
              Else
                *CurrentToken\Type | #LLCM_TYPE_I32
                *CurrentToken\Data = AllocateMemory(SizeOf(Long))
                Debug PeekS(@String + ((*CurrentToken\Start + 2)* SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start - 1)
                PokeL(*CurrentToken\Data, Val("$" + PeekS(@String + ((*CurrentToken\Start + 2)* SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start - 1)))
              EndIf
            ElseIf RegExFastMatch(#LLCM_REGEX_BINARY, PeekS(@String + (*CurrentToken\Start * SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start + 1))
              If #PB_Compiler_64Bit Or *CurrentToken\Stop - *CurrentToken\Start - 1 > 32
                *CurrentToken\Type | #LLCM_TYPE_I64
                *CurrentToken\Data = AllocateMemory(SizeOf(Quad))
                PokeQ(*CurrentToken\Data, Val("%" + PeekS(@String + ((*CurrentToken\Start + 2)* SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start - 1)))
              Else
                *CurrentToken\Type | #LLCM_TYPE_I32
                *CurrentToken\Data = AllocateMemory(SizeOf(Long))
                PokeL(*CurrentToken\Data, Val("%" + PeekS(@String + ((*CurrentToken\Start + 2)* SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start - 1)))
              EndIf
            ElseIf RegExFastMatch(#LLCM_REGEX_FLOAT, PeekS(@String + (*CurrentToken\Start * SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start + 1))
              If *CurrentToken\Stop - *CurrentToken\Start - 1 > 7
                *CurrentToken\Type | #LLCM_TYPE_F64
                *CurrentToken\Data = AllocateMemory(SizeOf(Double))
                PokeD(*CurrentToken\Data, ValD(PeekS(@String + (*CurrentToken\Start * SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start + 1)))
              Else
                *CurrentToken\Type | #LLCM_TYPE_F32
                *CurrentToken\Data = AllocateMemory(SizeOf(Float))
                PokeF(*CurrentToken\Data, ValF(PeekS(@String + (*CurrentToken\Start * SizeOf(Character)), *CurrentToken\Stop - *CurrentToken\Start + 1)))
              EndIf
            Else
              *CurrentToken\Type | #LLCM_TYPE_NAM
              *CurrentToken\Data = AllocateMemory((*CurrentToken\Stop - *CurrentToken\Start + 2) * SizeOf(Character))
              CopyMemory(@String + *CurrentToken\Start * SizeOf(Character), *CurrentToken\Data, (*CurrentToken\Stop - *CurrentToken\Start + 1) * SizeOf(Character))
            EndIf
          Default
            *CurrentToken\Type | #LLCM_TYPE_NAM
            *CurrentToken\Data = AllocateMemory((*CurrentToken\Stop - *CurrentToken\Start + 2) * SizeOf(Character))
            CopyMemory(@String + *CurrentToken\Start * SizeOf(Character), *CurrentToken\Data, (*CurrentToken\Stop - *CurrentToken\Start + 1) * SizeOf(Character))
        EndSelect
      EndIf
      
      
      
      If NextElement(*CurrentToken\Children())
        If ListIndex(*CurrentToken\Children()) = 0
          *CurrentToken\Children()\Type | #LLCM_TYPE_BEG
        EndIf
        If ListIndex(*CurrentToken\Children()) = ListSize(*CurrentToken\Children()) - 1
          *CurrentToken\Children()\Type | #LLCM_TYPE_END
        EndIf
        AddElement(*TokenAddressStack())
        *TokenAddressStack() = *CurrentToken
        *CurrentToken = @*CurrentToken\Children()
      Else
        If *CurrentToken = @TokenList
          Break
        Else
          ResetList(*CurrentToken\Children())
          *CurrentToken = *TokenAddressStack()
          DeleteElement(*TokenAddressStack())
        EndIf
      EndIf
      
    ForEver
    
    If CreateJSON(0)
      InsertJSONStructure(JSONValue(0), @TokenList, LLCM_TOKEN)
      ProcedureReturn ComposeJSON(0, #PB_JSON_PrettyPrint)
    EndIf
    
    ProcedureReturn ""
  EndProcedure
  
EndModule