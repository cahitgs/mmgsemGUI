; Inno Setup script for mmgsem GUI. Compiled by build_installer.R, which first
; fills the "staging" folder with R, the R packages and the application.

#ifndef AppVersion
  #define AppVersion "0.1.0"
#endif
#define AppName "mmgsem GUI"

[Setup]
AppId={{6F0E3B52-7C4D-4E0A-9C1B-2A7D5E8F4B31}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher=mmgsemGUI authors
; Installs for the current user, without administrator rights.
PrivilegesRequired=lowest
DefaultDirName={autopf}\mmgsemGUI
DefaultGroupName={#AppName}
DisableProgramGroupPage=yes
OutputDir=output
OutputBaseFilename=mmgsemGUI-setup-{#AppVersion}
SetupIconFile=app.ico
UninstallDisplayIcon={app}\app.ico
UninstallDisplayName={#AppName} {#AppVersion}
WizardStyle=modern
Compression=lzma2
SolidCompression=yes
LZMAUseSeparateProcess=yes
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
InfoAfterFile=staging\LICENSES.txt

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "turkish"; MessagesFile: "compiler:Languages\Turkish.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"

[Files]
Source: "staging\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs ignoreversion

[Icons]
; The application is R running a local web application; the console window
; starts minimized and closes by itself when the application window is closed.
Name: "{group}\{#AppName}"; Filename: "{app}\R\bin\x64\Rscript.exe"; \
  Parameters: "--no-save --no-restore --no-init-file --no-environ -e ""mmgsemGUI::run_app(desktop = TRUE, lang = '{code:AppLang}')"""; \
  WorkingDir: "{userdocs}"; IconFilename: "{app}\app.ico"; Flags: runminimized
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\R\bin\x64\Rscript.exe"; \
  Parameters: "--no-save --no-restore --no-init-file --no-environ -e ""mmgsemGUI::run_app(desktop = TRUE, lang = '{code:AppLang}')"""; \
  WorkingDir: "{userdocs}"; IconFilename: "{app}\app.ico"; Flags: runminimized; Tasks: desktopicon

[Run]
Filename: "{app}\R\bin\x64\Rscript.exe"; \
  Parameters: "--no-save --no-restore --no-init-file --no-environ -e ""mmgsemGUI::run_app(desktop = TRUE, lang = '{code:AppLang}')"""; \
  WorkingDir: "{userdocs}"; Description: "{cm:LaunchProgram,{#AppName}}"; \
  Flags: nowait postinstall skipifsilent runminimized

[Code]
// The interface starts in the language chosen for the installation; it can be
// switched inside the application at any time.
function AppLang(Param: String): String;
begin
  if ActiveLanguage = 'turkish' then
    Result := 'tr'
  else
    Result := 'en';
end;
