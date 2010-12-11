; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "Haxima"
#define MyAppVerName "Haxima 1.002 (Nazghul 0.7.1)"
#define MyAppPublisher "Haxima.org"
#define MyAppURL "http://myweb.cableone.net/gmcnutt/nazghul.html"
#define MyAppExeName "nazghul.exe"
#define MyAppUrlName "nazghul.url"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
LicenseFile=..\COPYING
OutputDir=.
OutputBaseFilename=nazghul-0.7.1-setup
SetupIconFile=..\win32\haxima.ico
Compression=lzma
SolidCompression=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: ..\src\nazghul.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\doc\*; DestDir: {app}\doc; Flags: ignoreversion recursesubdirs createallsubdirs
Source: ..\worlds\haxima-1.002\*; DestDir: {app}\haxima-1.002; Flags: ignoreversion recursesubdirs createallsubdirs
Source: ..\dlls\jpeg.dll; DestDir: {app}; Flags: ignoreversion
Source: ..\dlls\libpng13.dll; DestDir: {app}; Flags: ignoreversion
Source: ..\dlls\SDL.dll; DestDir: {app}; Flags: ignoreversion
Source: ..\dlls\SDL_image.dll; DestDir: {app}; Flags: ignoreversion
Source: ..\dlls\SDL_mixer.dll; DestDir: {app}; Flags: ignoreversion
Source: ..\dlls\zlib1.dll; DestDir: {app}; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[INI]
Filename: {app}\{#MyAppUrlName}; Section: InternetShortcut; Key: URL; String: {#MyAppURL}

[Icons]
Name: {group}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; Parameters: -I haxima-1.002 -G saves; WorkingDir: {app}; Comment: Haxima, a Nazghul engine game
Name: {group}\{cm:ProgramOnTheWeb,{#MyAppName}}; Filename: {app}\{#MyAppUrlName}
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
Name: {userdesktop}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; Tasks: desktopicon; Parameters: -I haxima-1.002 -G saves; WorkingDir: {app}; Comment: Haxima, a Nazghul engine game

[Run]
Filename: {app}\{#MyAppExeName}; Description: {cm:LaunchProgram,{#MyAppName}}; Flags: nowait postinstall skipifsilent; Parameters: -I haxima-1.002 -G saves; WorkingDir: {app}

[UninstallDelete]
Type: files; Name: {app}\{#MyAppUrlName}

[Dirs]
Name: {app}\saves
