program steamoverlaytoggler;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Windows;

const
  // Windows API constants needed for file access
  GENERIC_EXECUTE = $20000000;
  FILE_SHARE_DELETE = $00000004;
  CREATE_NO_WINDOW = $08000000;

var
  DLLFiles: array of string;
  CurrentDir: string;
  FoundFiles: TStringList;
  FilesFound: Integer;
  CurrentlyRestricted: Boolean;
  ActionToTake: string;
  UserInput: Char;
  i: Integer;

// Check if running as administrator using Windows API directly
function IsAdmin: Boolean;
var
  TokenHandle: THandle;
  TokenInformation: TTokenElevation;
  ReturnLength: DWORD;
begin
  Result := False;
  TokenHandle := 0;
  ReturnLength := 0;

  if OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, TokenHandle) then
  try
    if GetTokenInformation(TokenHandle, TokenElevation, @TokenInformation,
                          SizeOf(TokenInformation), ReturnLength) then
    begin
      Result := TokenInformation.TokenIsElevated <> 0;
    end;
  finally
    CloseHandle(TokenHandle);
  end;
end;

// Check if file permissions are restricted by trying to open the file with execute access
function IsRestricted(const FilePath: string): Boolean;
var
  FileHandle: THandle;
begin
  FileHandle := CreateFile(PChar(FilePath),
                           GENERIC_READ or GENERIC_EXECUTE,
                           FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                           nil,
                           OPEN_EXISTING,
                           FILE_ATTRIBUTE_NORMAL,
                           0);

  Result := (FileHandle = INVALID_HANDLE_VALUE);

  if FileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FileHandle);
end;

// Execute a Windows command safely
function ExecuteCommand(const Command: string): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  CmdLine: array[0..1023] of Char;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE;

  FillChar(PI, SizeOf(PI), 0);

  StrPCopy(CmdLine, Command);

  Result := CreateProcess(nil, CmdLine, nil, nil, False,
                          CREATE_NO_WINDOW, nil, nil, SI, PI);

  if Result then
  begin
    WaitForSingleObject(PI.hProcess, INFINITE);
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);
  end;
end;

// Restrict permissions on a file
procedure RestrictPermissions(const FilePath: string);
var
  FileName: string;
  Command: string;
begin
  FileName := ExtractFileName(FilePath);
  WriteLn('Disabling overlay for: ', FileName);

  Command := 'cmd.exe /c takeown /F "' + FilePath + '" /A && icacls "' + FilePath + '" /deny *S-1-1-0:(RX)';
  if ExecuteCommand(Command) then
    WriteLn('- Done.')
  else
    WriteLn('ERROR: Failed to set permissions for ', FileName);
end;

// Restore permissions on a file
procedure RestorePermissions(const FilePath: string);
var
  FileName: string;
  Command: string;
begin
  FileName := ExtractFileName(FilePath);
  WriteLn('Enabling overlay for: ', FileName);

  Command := 'cmd.exe /c takeown /F "' + FilePath + '" /A && icacls "' + FilePath + '" /remove:d *S-1-1-0';
  if ExecuteCommand(Command) then
    WriteLn('- Done.')
  else
    WriteLn('ERROR: Failed to restore permissions for ', FileName);
end;

{$R *.res}

begin
  WriteLn('Steam Overlay Permission Toggle');
  WriteLn('==============================');
  WriteLn;

  // Check for admin rights using Windows API
  if not IsAdmin then
  begin
    WriteLn('ERROR: Administrator privileges required.');
    WriteLn('Please right-click and select "Run as administrator".');
    WriteLn('Press Enter to exit...');
    ReadLn;
    Exit;
  end;

  // Define target DLLs
  SetLength(DLLFiles, 4);
  DLLFiles[0] := 'GameOverlayRenderer.dll';
  DLLFiles[1] := 'GameOverlayRenderer64.dll';
  DLLFiles[2] := 'SteamOverlayVulkanLayer.dll';
  DLLFiles[3] := 'SteamOverlayVulkanLayer64.dll';

  // Search for DLLs in current directory
  CurrentDir := ExtractFilePath(ParamStr(0));
  WriteLn('Searching for Steam overlay DLLs...');

  FoundFiles := TStringList.Create;
  try
    for i := 0 to Length(DLLFiles) - 1 do
    begin
      if FileExists(CurrentDir + DLLFiles[i]) then
      begin
        WriteLn('Found: ', DLLFiles[i]);
        FoundFiles.Add(CurrentDir + DLLFiles[i]);
      end;
    end;

    FilesFound := FoundFiles.Count;

    if FilesFound = 0 then
    begin
      WriteLn('ERROR: No Steam overlay DLLs found in this directory.');
      WriteLn('This application must be placed in the same directory as the Steam overlay DLLs.');
      WriteLn('Press Enter to exit...');
      ReadLn;
      Exit;
    end;

    WriteLn('Found ', FilesFound, ' Steam overlay DLL files.');

    // Check current status using first file
    CurrentlyRestricted := IsRestricted(FoundFiles[0]);

    WriteLn;
    if CurrentlyRestricted then
    begin
      WriteLn('CURRENT STATUS: Steam Overlay is DISABLED');
      WriteLn;
      WriteLn('This application will ENABLE the Steam overlay by restoring permissions.');
      ActionToTake := 'ENABLE';
    end
    else
    begin
      WriteLn('CURRENT STATUS: Steam Overlay is ENABLED');
      WriteLn;
      WriteLn('This application will DISABLE the Steam overlay by restricting permissions.');
      ActionToTake := 'DISABLE';
    end;

    WriteLn;
    Write('Do you want to ', ActionToTake, ' the Steam Overlay? (Y/N): ');
    ReadLn(UserInput);

    if UpCase(UserInput) <> 'Y' then
    begin
      WriteLn;
      WriteLn('Operation cancelled by user. No changes were made.');
      WriteLn('Press Enter to exit...');
      ReadLn;
      Exit;
    end;

    WriteLn;
    WriteLn('Proceeding with the change...');
    WriteLn;

    // Process each DLL file
    for i := 0 to FoundFiles.Count - 1 do
    begin
      if CurrentlyRestricted then
        RestorePermissions(FoundFiles[i])
      else
        RestrictPermissions(FoundFiles[i]);
    end;

    WriteLn;
    if CurrentlyRestricted then
    begin
      WriteLn('Steam overlay has been ENABLED.');
      WriteLn('Note: You may need to restart Steam for changes to take effect.');
    end
    else
      WriteLn('Steam overlay has been DISABLED.');
  finally
    FoundFiles.Free;
  end;

  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
