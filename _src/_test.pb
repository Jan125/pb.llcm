EnableExplicit

XIncludeFile "llcm.pbi"
XIncludeFile "include\consoleutil.pbi"

Procedure UnitTestSuccess(Name.s, Query.s, Expected.s)
  Protected Response.s
  Response = LLCM::Compile(Query)
  
  If Response <> Expected
    Debug "Unit Test Failed: "+Name
    If Expected <> ""
      Debug "Should be: "+Expected
    Else
      Debug "Should be: (None)"
    EndIf
    Debug "Is: "+Response
    CallDebugger
  EndIf
  ProcedureReturn
EndProcedure

Procedure.s EntireFileContent(Name.s, Flags.i = 0)
  Protected File.i
  Protected Output.s
  
  Flags & (#PB_Ascii | #PB_UTF8 | #PB_Unicode)
  
  File = ReadFile(#PB_Any, Name, #PB_File_SharedRead | #PB_File_SharedWrite | Flags)
  If File
    Output = ReadString(File, #PB_File_IgnoreEOL | Flags)
    CloseFile(File)
    ProcedureReturn Output
  EndIf
  
  ProcedureReturn ""
EndProcedure


OpenConsole()

Define String.s
;String = LLCM::Compile(EntireFileContent("C:\Users\Dino\Documents\PureBasic\PNBpaddlesandball\pnbpaddlesandball.pnb"))
String = LLCM::Compile("Hello.")
If PeekS(@String, 14) = "Compiler Error"
  WriteError(String)
Else
  WriteDefault(String)
EndIf

Input()