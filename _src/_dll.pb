XIncludeFile "llcm.pbi"

CompilerIf #PB_Compiler_ExecutableFormat = #PB_Compiler_DLL And #PB_Compiler_Thread = 1
	ProcedureDLL AttachProcess(Instance)
		Global MUTEX = CreateMutex()
		
	EndProcedure
	
	ProcedureDLL DetachProcess(Instance)
		FreeMutex(MUTEX)
		
	EndProcedure
CompilerEndIf

ProcedureDLL.s Compile(String.s)
	CompilerIf #PB_Compiler_ExecutableFormat = #PB_Compiler_DLL
		Global ReturnString.s
	CompilerElse
		Protected ReturnString.s
	CompilerEndIf
	ReturnString = LLCM::Compile(String)
	ProcedureReturn ReturnString
	
EndProcedure

ProcedureCDLL.s _Compile(String.s)
	CompilerIf #PB_Compiler_ExecutableFormat = #PB_Compiler_DLL
		Global ReturnString.s
	CompilerElse
		Protected ReturnString.s
	CompilerEndIf
	ReturnString = LLCM::Compile(String)
	ProcedureReturn ReturnString
	
EndProcedure