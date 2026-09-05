EnableExplicit

ImportC "msvcrt.lib"
  system(str.p-ascii)
EndImport

Define LibraryName.s
Define Library.i
Define String.s
Define InString.s
Define i.i
Define File.i
OpenConsole()

Procedure.i Exit()
  End
EndProcedure

CompilerSelect #PB_Compiler_Processor 
  CompilerCase #PB_Processor_x86
    LibraryName = "llcm.dll"
  CompilerCase #PB_Processor_x64
    LibraryName = "llcm64.dll"
  CompilerDefault
    CompilerError "Only x86 and x64 supported."
CompilerEndSelect

Library = OpenLibrary(#PB_Any, LibraryName)
If Not Library
  PrintN("Required library " + LibraryName + " Not found in directory. This program will terminate.")
  system("pause")
  End
EndIf

String = "(Function (Exit) Do (Invoke None " + Str(@Exit()) + "))"
CallFunction(Library, "Compile", @String)

If CountProgramParameters()
  For i = 0 To CountProgramParameters() - 1
    InString = InString + ProgramParameter(i)
    If i < CountProgramParameters() - 1
      InString = InString + " "
    EndIf
  Next
EndIf

If Len(InString)
  If FileSize(InString) >= 0
    File = ReadFile(#PB_Any, InString)
    String = ReadString(File, #PB_File_IgnoreEOL)
    String = PeekS(CallFunction(Library, "Compile", @String))
    If Len(String)
      PrintN(String)
    EndIf
    CloseFile(File)
    End
  Else
    String = PeekS(CallFunction(Library, "Compile", @InString))
    If Len(String)
      PrintN(String)
    EndIf
    End
  EndIf
Else
  Repeat
    Print(">")
    String = Input()
    String = PeekS(CallFunction(Library, "Compile", @String))
    If Len(String)
      PrintN(String)
    EndIf
  ForEver
EndIf

