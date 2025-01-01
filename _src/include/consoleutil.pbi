EnableExplicit

Procedure.l WriteError(String.s)
  Protected lpConsoleScreenBufferInfo.CONSOLE_SCREEN_BUFFER_INFO
  Protected lNumberOfCharsWritten.l
  
  GetConsoleScreenBufferInfo_(GetStdHandle_(#STD_ERROR_HANDLE), @lpConsoleScreenBufferInfo)
  SetConsoleTextAttribute_(GetStdHandle_(#STD_ERROR_HANDLE), #FOREGROUND_RED|#FOREGROUND_INTENSITY)
  
  If GetStdHandle_(#STD_ERROR_HANDLE)
    WriteConsole_(GetStdHandle_(#STD_ERROR_HANDLE), String, Len(String), @lNumberOfCharsWritten, 0)
  EndIf
  
  SetConsoleTextAttribute_(GetStdHandle_(#STD_ERROR_HANDLE), lpConsoleScreenBufferInfo\wAttributes)
  
  ProcedureReturn lNumberOfCharsWritten
EndProcedure

Procedure.l WriteDefault(String.s)
  Protected lNumberOfCharsWritten.l
  
  If GetStdHandle_(#STD_ERROR_HANDLE)
    WriteConsole_(GetStdHandle_(#STD_ERROR_HANDLE), String, Len(String), @lNumberOfCharsWritten, 0)
  EndIf
  
  ProcedureReturn lNumberOfCharsWritten
EndProcedure