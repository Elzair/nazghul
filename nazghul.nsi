; Nazghul Installer Script
;
; Compile in directory with:
; .\bin\(executables and libraries)
; .\haxima\(game files)
; .\(documents)
;
;--------------------------------

; The name of the installer
Name "Nazghul"

; The file to write
OutFile "nazghul-install.exe"

; The default installation directory
InstallDir $PROGRAMFILES\nazghul

; Installer Icon
Icon "nsi\nazinst.ico"

; File for license info- must be in DOS (\r\n) format
LicenseData "COPYING.txt"

; Show Nazghul splash image ;; not working yet
;AddBrandingImage left 100

;--------------------------------

; Pages

Page license
Page directory
Page components
Page instfiles

;--------------------------------

; The stuff to install
Section "-Main Program"
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  ; needed in other sections
  Var /global varSFBack
  StrCpy $varSFBack "n"
  Var /global varSMenu
  StrCpy $varSMenu "n"
  ; executables
  SetOutPath $INSTDIR
  File /r bin
  File /r haxima
  File *.txt 
  CreateDirectory "$INSTDIR\save"
  CreateShortCut "$INSTDIR\Nazghul.lnk" "$INSTDIR\bin\nazghul.exe" \
    "-I haxima -G save" "" 0 SW_SHOWNORMAL \
    "" "Retro-style RPG"
  CreateShortCut "$INSTDIR\Nazghul (Continue).lnk" "$INSTDIR\bin\nazghulrun.exe" \
    "-c save" "" 0 SW_SHOWNORMAL \
    "" "Immediately load saved game"
  ;File nazghul.ico
  ;Files below this wouldnt be needed by a real nazghul install
  ;CreateDirectory "$INSTDIR\nsi"
  ;SetOutPath "$INSTDIR\nsi"
  ;File nazinst.ico
  ;File nexample1.nsi
  ;File COPYING.txt
SectionEnd ; end the section

; sets up the save-file backup script 
Section "Save file backup utility"
  StrCpy $varSFBack "y" ;add appropriate menus elsewhere
  CreateShortCut "$INSTDIR\Backup Savefile.lnk" "$INSTDIR\bin\nazghulrun.exe" \
    "-b save" "" 0 SW_SHOWNORMAL \
    "" "Backs up the nazghul save file by timestamp"   
SectionEnd;

Section "Start Menu items"
  ;note targets and icons already created
  StrCpy $varSMenu "y" ;note that we have a start menu
  CreateDirectory "$SMPROGRAMS\Nazghul"
  CreateShortCut "$SMPROGRAMS\Nazghul\Users Guide.lnk" "$INSTDIR\USERS_GUIDE.TXT"
  strcmp $varSFBack "y" TRUE1 FALSE1 ;; My kingdom for an if/else!
  TRUE1:
  CreateShortCut "$SMPROGRAMS\Nazghul\Savefile Backup.lnk" "$INSTDIR\bin\nazghulrun.exe" \
    "-b save" "" 0 SW_SHOWNORMAL \
    "" "Backs up the nazghul save file by timestamp" 
  FALSE1:
  CreateShortCut "$SMPROGRAMS\Nazghul\Nazghul (Continue).lnk" "$INSTDIR\bin\nazghulrun.exe" \
    "-c save" "" 0 SW_SHOWNORMAL \
    "" "Immediately load saved game"
  CreateShortCut "$SMPROGRAMS\Nazghul\Nazghul.lnk" "$INSTDIR\bin\nazghul.exe" \
    "-I haxima -G save" "" 0 SW_SHOWNORMAL \
    "" "Retro-style RPG"
SectionEnd ; end the section

Section /o "Desktop icon" 
  ;note targets and icons already created
  CreateShortCut "$DESKTOP\Nazghul.lnk" "$INSTDIR\bin\nazghul.exe" \
    "-I haxima -G save" "" 0 SW_SHOWNORMAL \
    "" "Retro-style RPG"
  strcmp $varSFBack "y" TRUE2 NODTICON ;; My kingdom for an if/else!
  TRUE2:
  strcmp $varSMenu "y" NODTICON DTICON ;; My kingdom for an if/else!  
  DTICON:
  CreateShortCut "$DESKTOP\Backup Nazghul Savefile.lnk" "$INSTDIR\bin\nazghulrun.exe" \
    "-b save" "" 0 SW_SHOWNORMAL \
    "" "Backs up the nazghul save file by timestamp" 
  NODTICON:
SectionEnd ; end the section

;Function .onGUIInit ;; not working yet...
;	GetTempFileName $0
;	SetOutPath $TEMP
;	File /oname=$0 Splash.png
;   SetBrandingImage $0
;   Delete $0
;FunctionEnd