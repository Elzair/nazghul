; ----------------------------------------------------------------------------
;
; AutoIt Version: 3.1.0
; Author:         kaypy
;
; Script Function:
;	Nazghul backup and autostart script. 
;
;   Starts using a save file if one exists
;   If called with -b, backs up the file (in not already backed up)
;   If called with -n, starts a new game
;
; can also take a parameter for an alternate save location
;
; Compile script using AutoIt V3
; Dont forget to set the icon at compilation
;
; ----------------------------------------------------------------------------

; Script Start - Add your code below here

$cmdparam = ""
$saveloc = "save"

If $CmdLine[0] > 0 Then
	$cmdparam = $CmdLine[1];
	If $CmdLine[0] > 1 Then
		$saveloc = $CmdLine[2];
	EndIf
EndIf
	
$savefile = @ScriptDir & "\..\" & $saveloc & "\save.scm"

If $cmdparam == "-b" Then

	If FileExists($savefile) Then
	
		$t =  FileGetTime($savefile,0,1)
		$backup = @ScriptDir & "\..\" & $saveloc & "\save" & $t & ".scm"
		FileCopy($savefile,$backup,0) ;noclobber copy
	
		MsgBox (64,"Nazghul backup","Save file was backed up to " & @CRLF & $backup)
	
	Else
		
		MsgBox (16,"Nazghul backup","No save file was found")

	EndIf
	
ElseIf ($cmdparam == "-n") OR (NOT FileExists($savefile)) Then

	Run (@ScriptDir & "\nazghul.exe -I haxima -G " & $saveloc,@ScriptDir & "\..")

Else

	Run (@ScriptDir & "\nazghul.exe -I haxima -G " & $saveloc & " save.scm",@ScriptDir & "\..")
	
EndIf

