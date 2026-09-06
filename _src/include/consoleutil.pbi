EnableExplicit

Procedure.l WriteError(String.s)
  CompilerSelect #PB_Compiler_OS
    CompilerCase #PB_OS_Windows
      Protected lpConsoleScreenBufferInfo.CONSOLE_SCREEN_BUFFER_INFO
      Protected lNumberOfCharsWritten.l
      
      If GetStdHandle_(#STD_ERROR_HANDLE)
        GetConsoleScreenBufferInfo_(GetStdHandle_(#STD_ERROR_HANDLE), @lpConsoleScreenBufferInfo)
        SetConsoleTextAttribute_(GetStdHandle_(#STD_ERROR_HANDLE), #FOREGROUND_RED | #FOREGROUND_INTENSITY)
        
        ConsoleError(String)
      EndIf
      
      SetConsoleTextAttribute_(GetStdHandle_(#STD_ERROR_HANDLE), lpConsoleScreenBufferInfo\wAttributes)
      
      ProcedureReturn lNumberOfCharsWritten
    CompilerDefault
      ConsoleError(String)
  CompilerEndSelect
EndProcedure

Procedure.l WriteDefault(String.s)
  
  PrintN(String)
EndProcedure