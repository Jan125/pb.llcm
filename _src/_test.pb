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

OpenConsole()

Define String.s
String = LLCM::Compile("0x,")
If PeekS(@String, 14) = "Compiler Error"
  WriteError(String)
Else
  WriteDefault(String)
EndIf

Input()