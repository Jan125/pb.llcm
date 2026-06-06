EnableExplicit

DeclareModule LLCM
	Declare.s Compile(String.s)
EndDeclareModule

Module LLCM
	
	Procedure.s Error(Message.s, Position, Line, Column)
		ProcedureReturn "Compiler Error: " + Message + #CRLF$ +
		                "Position: " + Str(Position) + ", Line: " + Str(Line) + ", Column: " + Str(Column) + #CRLF$ +
		                "Compilation aborted."
	EndProcedure
	
	Structure Integer_Group_4
		a.i
		b.i
		c.i
		d.i
	EndStructure
	
	Structure LLCM_STRUCT_INDEX
		Start.i
		Stop.i
		Type.i
		Depth.i
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
	
	#LLCM_COND_EVAL = "?"
	#LLCM_COND_LITERAL = ":"
	#LLCM_COND_NEXT = "+"
	#LLCM_COND_PREVIOUS = "-"
	#LLCM_COND_SUBLIST = "*"
	#LLCM_COND_LIS = "l"
	#LLCM_COND_CMD = "c"
	#LLCM_COND_END = "e"
	#LLCM_COND_HEX = "h"
	#LLCM_COND_BIN = "b"
	#LLCM_COND_STR = "s"
	#LLCM_COND_NAM = "n"
	#LLCM_COND_I64 = "6"
	#LLCM_COND_I32 = "3"
	#LLCM_COND_I16 = "1"
	#LLCM_COND_I08 = "8"
	#LLCM_COND_U16 = "2"
	#LLCM_COND_U08 = "9"
	#LLCM_COND_F64 = "d"
	#LLCM_COND_F32 = "f"
	#LLCM_COND_PTR = "p"
	
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
	
	;Procedure 
	
	;   Procedure.s Interpret(*String, List IndexList.Integer_Group_4(), )
	;     
	;   EndProcedure
	;   
	Procedure.i AssertIndexStructure(*String, List IndexList.LLCM_STRUCT_INDEX(), Pattern.s = "")
		Protected Index.i
		Protected Boolean.i = 1
		Protected Condition.i
		Protected Temporary.i
		Protected Literal.s
		
		
		PushListPosition(IndexList())
		For Index = 0 To Len(Pattern) - 1
			If ListIndex(IndexList()) <> -1
				Select PeekC(@Pattern + Index * SizeOf(Character))
					Case Asc(#LLCM_COND_EVAL)
						If Condition
							Condition = 0
						Else
							Boolean = 0
							Break
						EndIf
						
					Case Asc(#LLCM_COND_LITERAL)
						Index + 1
						Temporary = PeekC(@Pattern + Index * SizeOf(Character))
						Repeat
							Index + 1
							If PeekC(@Pattern + Index * SizeOf(Character)) <> Temporary
								Literal + Chr(PeekC(@Pattern + Index * SizeOf(Character)))
							EndIf
						Until PeekC(@Pattern + Index * SizeOf(Character)) = Temporary
						Temporary = 0
						
						If Not PeekS(*String + IndexList()\Start * SizeOf(Character), IndexList()\Stop - IndexList()\Start + 1) = Literal
							Boolean = 0
							Literal = ""
							Break
						Else
							Condition = 1
						EndIf
						Literal = ""
						
					Case Asc(#LLCM_COND_NEXT)
						If Not NextElement(IndexList())
							Boolean = 0
							Break
						EndIf
						Condition = 1
						
					Case Asc(#LLCM_COND_PREVIOUS)
						If Not PreviousElement(IndexList())
							Boolean = 0
							Break
						EndIf
						Condition = 1
						
					Case Asc(#LLCM_COND_SUBLIST)
						If IndexList()\Type & #LLCM_TYPE_LIS
							Temporary = IndexList()\Stop
							While NextElement(IndexList())
								If IndexList()\Start > Temporary
									Break
								EndIf
							Wend
							If ListIndex(IndexList()) = -1
								Boolean = 0
								Break
							EndIf
							Condition = 1
						Else
							Boolean = 0
							Break
						EndIf
						
					Case Asc(#LLCM_COND_LIS)
						If Not IndexList()\Type & #LLCM_TYPE_LIS
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_CMD)
						If Not IndexList()\Type & #LLCM_TYPE_CMD
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_END)
						If Not IndexList()\Type & #LLCM_TYPE_END
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_HEX)
						If Not IndexList()\Type & #LLCM_TYPE_HEX
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_BIN)
						If Not IndexList()\Type & #LLCM_TYPE_BIN
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_STR)
						If Not IndexList()\Type & #LLCM_TYPE_STR
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_NAM)
						If Not IndexList()\Type & #LLCM_TYPE_NAM
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_I64)
						If Not IndexList()\Type & #LLCM_TYPE_I64
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_I32)
						If Not IndexList()\Type & #LLCM_TYPE_I32
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_I16)
						If Not IndexList()\Type & #LLCM_TYPE_I16
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_I08)
						If Not IndexList()\Type & #LLCM_TYPE_I08
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_U16)
						If Not IndexList()\Type & #LLCM_TYPE_U16
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_U08)
						If Not IndexList()\Type & #LLCM_TYPE_U08
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_F64)
						If Not IndexList()\Type & #LLCM_TYPE_F64
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_F32)
						If Not IndexList()\Type & #LLCM_TYPE_F32
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
					Case Asc(#LLCM_COND_PTR)
						If Not IndexList()\Type & #LLCM_TYPE_PTR
							Boolean = 0
							Break
						Else
							Condition = 1
						EndIf
						
				EndSelect
			Else
				Boolean = 0
				Break
			EndIf
		Next
		PopListPosition(IndexList())
		ProcedureReturn Boolean
	EndProcedure
	
	Procedure.s Compile(String.s)
		;-Init
		Protected NewList IndexList.LLCM_STRUCT_INDEX()
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
					IndexList()\Start = Index
					IndexList()\Stop = -1
					IndexList()\Depth = Depth
					Depth + 1
					
				Case Asc(")")
					If ListIndex(IndexList()) <> -1
						If PeekC(@String + IndexList()\Start * SizeOf(Character)) = Asc("(")
							Depth - 1
							IndexList()\Stop = Index
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
					IndexList()\Start = Index
					IndexList()\Depth = Depth
					While Index < Finish
						Index + 1
						Column + 1
						Select PeekC(@String + (Index * SizeOf(Character)))
							Case 10
								Line + 1
								Column = 1
							Case Asc("]")
								If ListIndex(IndexList()) <> -1
									IndexList()\Stop = Index
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
					IndexList()\Start = Index
					IndexList()\Stop = -1
					IndexList()\Depth = Depth
					While Index < Finish
						Index + 1
						Column + 1
						Select PeekC(@String + (Index * SizeOf(Character)))
							Case 10
								Line + 1
								Column = 1
							Case 34 ;"
								If ListIndex(IndexList()) <> -1
									IndexList()\Stop = Index
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
					IndexList()\Start = Index
					IndexList()\Stop = -1
					IndexList()\Depth = Depth
					While Index < Finish
						Index + 1
						Column + 1
						Select PeekC(@String + (Index * SizeOf(Character)))
							Case 10
								Line + 1
								Column = 1
							Case Asc("'")
								If ListIndex(IndexList()) <> -1
									IndexList()\Stop = Index
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
					IndexList()\Start = Index
					IndexList()\Stop = -1
					IndexList()\Depth = Depth
					While Index < Finish
						Index + 1
						Select PeekC(@String + (Index * SizeOf(Character)))
							Case Asc(" "), 9, 13, 10, Asc("("), Asc(")"), Asc("["), Asc("]"), Asc(";"), 34, Asc("'"), Asc(",")
								Index - 1
								IndexList()\Stop = Index
								If Not PreviousElement(IndexList())
									ResetList(IndexList())
								EndIf
								Break
						EndSelect
						If Index = Finish
							Index - 1
							IndexList()\Stop = Index
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
				Count = IndexList()\Start
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
		SortStructuredList(IndexList(), #PB_Sort_Ascending, OffsetOf(LLCM_STRUCT_INDEX\Start), TypeOf(LLCM_STRUCT_INDEX\Start))
		
		
		;-Type
		ForEach IndexList()
			;Clear list when an invalid End value was discovered.
			;This may happen when there is an index overrun.
			Select IndexList()\Stop
				Case -1
					Count = IndexList()\Start
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
			
			
			Select PeekC(@String + IndexList()\Start * SizeOf(Character))
				Case Asc("(")
					IndexList()\Type | #LLCM_TYPE_LIS
					Pointer = IndexList()\Stop
					PushListPosition(IndexList())
					
					While NextElement(IndexList())
						If ListIndex(IndexList()) = ListSize(IndexList()) - 1
							IndexList()\Type | #LLCM_TYPE_END
							
							Break 
						EndIf
						If IndexList()\Start >= Pointer
							
							While PreviousElement(IndexList())
								IndexList()\Type | #LLCM_TYPE_END
								If Not IndexList()\Type & #LLCM_TYPE_LIS
									IndexList()\Type | #LLCM_TYPE_END
									Break
								EndIf
							Wend
							Break
						EndIf
					Wend
					
					PopListPosition(IndexList())
					
					
					Continue
				Case Asc("["), Asc("'"), 34
					IndexList()\Type | #LLCM_TYPE_STR
					Continue
				Case Asc("0") To Asc("9"), Asc("+"), Asc("-"), Asc(".") ;Numbers, plus, minus, and decimal
					Select PeekC(@String + IndexList()\Start * SizeOf(Character))
						Case Asc("+"), Asc("-"), Asc(".")
							If IndexList()\Start = IndexList()\Stop
								If PreviousElement(IndexList())
									If IndexList()\Type = #LLCM_TYPE_LIS
										NextElement(IndexList())
										IndexList()\Type | #LLCM_TYPE_CMD
									Else
										NextElement(IndexList())
									EndIf
								Else
									IndexList()\Type | #LLCM_TYPE_CMD
								EndIf
								IndexList()\Type | #LLCM_TYPE_NAM
								Continue
							EndIf
					EndSelect
					
					;check illegals
					For Index = IndexList()\Start To IndexList()\Stop
						Select PeekC(@String + (Index * SizeOf(Character)))
							Case Asc("x")
								If Index = IndexList()\Start + 1
									Continue
								Else
									Break
								EndIf
							Case Asc("+"), Asc("-")
								If Index = IndexList()\Start
									Continue
								Else
									Break
								EndIf
							Case Asc("0") To Asc("9")
							Case Asc(".")
								If IndexList()\Stop - IndexList()\Start >= 1
									Select PeekC(@String + (IndexList()\Start + 1) * SizeOf(Character))
										Case Asc("x")
											Break
									EndSelect
								EndIf
							Case Asc("a") To Asc("f"), Asc("A") To Asc("F")
								If IndexList()\Stop - IndexList()\Start >= 1
									Select PeekC(@String + (IndexList()\Start + 1) * SizeOf(Character))
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
					
					If Index <= IndexList()\Stop
						If PreviousElement(IndexList())
							If IndexList()\Type = #LLCM_TYPE_LIS
								NextElement(IndexList())
								IndexList()\Type | #LLCM_TYPE_CMD
							Else
								NextElement(IndexList())
							EndIf
						Else
							IndexList()\Type | #LLCM_TYPE_CMD
						EndIf
						IndexList()\Type | #LLCM_TYPE_NAM
						Continue
					EndIf
					;end check illegals
					
					
					For Index = IndexList()\Start To IndexList()\Stop
						Select PeekC(@String + (Index * SizeOf(Character)))
							Case Asc(".")
								If Index < IndexList()\Stop
									Index + 1
									For Index = Index To IndexList()\Stop
										Select PeekC(@String + (Index * SizeOf(Character)))
											Case Asc(".")
												If PreviousElement(IndexList())
													If IndexList()\Type = #LLCM_TYPE_LIS
														NextElement(IndexList())
														IndexList()\Type | #LLCM_TYPE_CMD
													Else
														NextElement(IndexList())
													EndIf
												Else
													IndexList()\Type | #LLCM_TYPE_CMD
												EndIf
												IndexList()\Type | #LLCM_TYPE_NAM
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
					
					For Index = IndexList()\Start To IndexList()\Stop
						Select PeekC(@String + (Index * SizeOf(Character)))
							Case Asc(".")
								Break
						EndSelect
					Next
					
					If Index <= IndexList()\Stop
						If IndexList()\Stop - Index < 15
							IndexList()\Type | #LLCM_TYPE_F32
							Continue
						Else
							IndexList()\Type | #LLCM_TYPE_F64
							Continue
						EndIf
					Else
						If IndexList()\Stop - IndexList()\Start >= 2
							Select PeekS(@String + IndexList()\Start * SizeOf(Character), 2)
								Case "0x"
									If IndexList()\Stop - IndexList()\Start < 11
										IndexList()\Type | #LLCM_TYPE_I32
										IndexList()\Type | #LLCM_TYPE_HEX
										Continue
									Else
										IndexList()\Type | #LLCM_TYPE_I64
										IndexList()\Type | #LLCM_TYPE_HEX
										Continue
									EndIf
								Case "0b"
									If IndexList()\Stop - IndexList()\Start < 35
										IndexList()\Type | #LLCM_TYPE_I32
										IndexList()\Type | #LLCM_TYPE_BIN
										Continue
									Else
										IndexList()\Type | #LLCM_TYPE_I64
										IndexList()\Type | #LLCM_TYPE_BIN
										Continue
									EndIf
							EndSelect
						EndIf
						
						For Index = IndexList()\Start To IndexList()\Stop
							Select PeekC(@String + (Index * SizeOf(Character)))
								Case Asc("0") To Asc("9"), Asc("+"), Asc("-")
								Default
									Break
							EndSelect
						Next
						
						If Index <= IndexList()\Stop
							If PreviousElement(IndexList())
								If IndexList()\Type = #LLCM_TYPE_LIS
									NextElement(IndexList())
									IndexList()\Type | #LLCM_TYPE_CMD
								Else
									NextElement(IndexList())
								EndIf
							Else
								IndexList()\Type | #LLCM_TYPE_CMD
							EndIf
							IndexList()\Type | #LLCM_TYPE_NAM
							Continue
						EndIf
						
						
						If IndexList()\Stop - IndexList()\Start < 12
							IndexList()\Type | #LLCM_TYPE_I32
							Continue
						Else
							IndexList()\Type | #LLCM_TYPE_I64
							Continue
						EndIf
					EndIf
					
				Default
					
					If PreviousElement(IndexList())
						If IndexList()\Type = #LLCM_TYPE_LIS
							NextElement(IndexList())
							IndexList()\Type | #LLCM_TYPE_CMD
						Else
							NextElement(IndexList())
						EndIf
					Else
						IndexList()\Type | #LLCM_TYPE_CMD
					EndIf
					IndexList()\Type | #LLCM_TYPE_NAM
					Continue
			EndSelect
		Next
		
		
		;-Compile
		ForEach IndexList()
			If Not IndexList()\Type & #LLCM_TYPE_LIS And IndexList()\Type & #LLCM_TYPE_CMD
				
				;         Select PeekS(@String + IndexList()\Start * SizeOf(Character), IndexList()\Stop - IndexList()\Start + 1)
				;           Case "Function"
				;             Debug AssertIndexStructure(@String, IndexList(), "n?:'Function'?+?l?+?n?e?+?n?:'Do'?+?l?*?")
				;             Debug AssertIndexStructure(@String, IndexList(), "n?:'Function'?+?l?+?n?e?+?n?:'Do'?+?l?*? n?:'With'?+?l?*?")
				;             Debug AssertIndexStructure(@String, IndexList(), "n?:'Function'?+?l?+?n?e?+?n?:'Do'?+?l?*? n?:'With'?+?l?*? n?:'As'+?l?*?")
				;             
				;         EndSelect
			EndIf
		Next
		;Delay(30000)
		
		;-Interpret
		;--PreProcess
		ForEach IndexList()
			If Not IndexList()\Type & #LLCM_TYPE_LIS And IndexList()\Type & #LLCM_TYPE_CMD
			EndIf
		Next
		;Process
		ForEach IndexList()
			If Not IndexList()\Type & #LLCM_TYPE_LIS And IndexList()\Type & #LLCM_TYPE_CMD
			EndIf
		Next
		ForEach IndexList()
			If Not IndexList()\Type & #LLCM_TYPE_LIS And IndexList()\Type & #LLCM_TYPE_CMD
			EndIf
		Next
		
		
		
		;-Output
		Index = 0
		Count = 0
		ForEach IndexList()
			If Not IndexList()\Type & #LLCM_TYPE_LIS
				Count + 1
				Debug Space(IndexList()\Depth * 2) + Str(IndexList()\Start) + "_" + Str(IndexList()\Stop) + ":" + RSet(Bin(IndexList()\Type), 14, "0") + ":" + PeekS(@String + IndexList()\Start * SizeOf(Character), IndexList()\Stop - IndexList()\Start + 1)
			EndIf
			
		Next
		
		Index = ListSize(IndexList())
		
		
		ClearList(IndexList())
		ProcedureReturn "Compiler Status: Compilation successful." + #CRLF$ + "Lexed " + Str(Count) + " token" + LSet("s", Bool(Count <> 1)) + ", " + Str(Index - Count) + " sublist" + LSet("s", Bool(Index - Count <> 1)) + "."
	EndProcedure
	
EndModule