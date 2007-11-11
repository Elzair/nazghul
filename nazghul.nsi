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
UninstallIcon "nsi\nazinst.ico"

SetCompress off

; File for license info- must be in DOS (\r\n) format
LicenseData "doc/COPYING.txt"

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
  File /r doc
  File *.html
  CreateDirectory "$INSTDIR\save"
  CreateShortCut "$INSTDIR\Nazghul.lnk" "$INSTDIR\bin\nazghul.exe" \
    "-I haxima -G save" "" 0 SW_SHOWNORMAL \
    "" "Retro-style RPG"
  WriteUninstaller "uninstall.exe"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Nazghul_0.6.0" "DisplayName" "Nazghul 0.6.0"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Nazghul_0.6.0" "UninstallString" '"$INSTDIR\uninstall.exe"'
SectionEnd ; end the section

Section "Start Menu Shortcuts"
  ;note targets and icons already created
  CreateDirectory "$SMPROGRAMS\Nazghul"
  CreateShortCut "$SMPROGRAMS\Nazghul\Users Guide.lnk" "$INSTDIR\users_guide.html"
  CreateShortCut "$SMPROGRAMS\Nazghul\Nazghul.lnk" "$INSTDIR\bin\nazghul.exe" \
    "-I haxima -G save" "" 0 SW_SHOWNORMAL \
    "" "Retro-style RPG"
  CreateShortCut "$SMPROGRAMS\Nazghul\Uninstall.lnk" "$INSTDIR\uninstall.exe" \
    "" "" 0 SW_SHOWNORMAL \
    "" "Uninstall Nazghul"
SectionEnd ; end the section

Section /o "Desktop Shortcuts" 
  ;note targets and icons already created
  CreateShortCut "$DESKTOP\Nazghul.lnk" "$INSTDIR\bin\nazghul.exe" \
    "-I haxima -G save" "" 0 SW_SHOWNORMAL \
    "" "Retro-style RPG"
SectionEnd ; end the section

;Function .onGUIInit ;; not working yet...
;	GetTempFileName $0
;	SetOutPath $TEMP
;	File /oname=$0 Splash.png
;   SetBrandingImage $0
;   Delete $0
;FunctionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"

  ; Remove shortcuts, if any
  Delete "$DESKTOP\Nazghul.lnk"
  RMDir /r "$SMPROGRAMS\Nazghul"
  
  ; Remove directories used
  RMDir /r "$INSTDIR\doc"
  RMDir /r "$INSTDIR\bin"
  RMDir /r "$INSTDIR\haxima"
  Delete "$INSTDIR\*.*"
  
  MessageBox MB_YESNO|MB_ICONQUESTION "Do you want to remove the save game data as well as the program itself?" IDNO NoDelete
    RMDir /r "$INSTDIR" ; skipped if no
  NoDelete:

SectionEnd