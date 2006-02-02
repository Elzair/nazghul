; example1.nsi
;
; This script is perhaps one of the simplest NSIs you can make. All of the
; optional settings are left to their default settings. The installer simply 
; prompts the user asking them where to install, and drops a copy of example1.nsi
; there. 

;--------------------------------

; The name of the installer
Name "Example1"

; The file to write
OutFile "nazghul-0.5.2-install.exe"

; The default installation directory
InstallDir $PROGRAMFILES\nazghul

;--------------------------------

; Pages

Page directory
Page instfiles

;--------------------------------

; The stuff to install
Section "" ;No components page, name is not important

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Put file there
  File nazghul.exe
  File *.scm
  File *.dll
  File *.png
  File *.wav
  File *.txt
  File *.bat
  
SectionEnd ; end the section
