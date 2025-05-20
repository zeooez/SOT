program SteamOverlayToggler;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Process;

var
  DLLFiles: array of string;
  CurrentDir: string;
  FoundFiles: TStringList;
  FilesFound: Integer;
  CurrentlyRestricted: Boolean;
  ActionToTake: string;
  UserInput: Char;
  i: Integer;

// Check if running as administrator
function IsAdmin: Boolean;
var
  RunAsAdmin: TProcess;
  ExitCode: Integer;
begin
  Result := False;
  RunAsAdmin := TProcess.Create(nil);
  try
    RunAsAdmin.Executable := 'net';
    RunAsAdmin.Parameters.Add('session');
    RunAsAdmin.Options := RunAsAdmin.Options + [poWaitOnExit, poNoConsole];
    RunAsAdmin.Execute;
    ExitCode := RunAsAdmin.ExitStatus;
    Result := (ExitCode = 0);
  finally
    RunAsAdmin.Free;
  end;
end;

// Check if file permissions are restricted
function IsRestricted(const FilePath: string): Boolean;
var
  CheckProcess: TProcess;
  OutputFile: TextFile;
  TempFileName: string;
  OutputLine: string;
begin
  Result := False;
  TempFileName := GetTempDir + 'overlay_check_' + IntToStr(Random(100000)) + '.txt';
  
  CheckProcess := TProcess.Create(nil);
  try
    CheckProcess.Executable := 'cmd.exe';
    CheckProcess.Parameters.Add('/c');
    CheckProcess.Parameters.Add('icacls "' + FilePath + '" > "' + TempFileName + '"');
    CheckProcess.Options := CheckProcess.Options + [poWaitOnExit, poNoConsole];
    CheckProcess.Execute;
    
    // Read the temporary file
    AssignFile(OutputFile, TempFileName);
    try
      Reset(OutputFile);
      while not EOF(OutputFile) do
      begin
        ReadLn(OutputFile, OutputLine);
        if (Pos('Everyone:(DENY)', OutputLine) > 0) or (Pos('*S-1-1-0:(DENY)', OutputLine) > 0) then
        begin
          Result := True;
          Break;
        end;
      end;
    finally
      CloseFile(OutputFile);
      DeleteFile(TempFileName);
    end;
  finally
    CheckProcess.Free;
  end;
end;

// Run a process and get the success status
function RunProcess(const Executable, Arguments: string): Boolean;
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := Executable;
    // Split the arguments into the parameter list
    if Arguments <> '' then
    begin
      Proc.Parameters.Clear;
      Proc.Parameters.Add(Arguments);
    end;
    Proc.Options := Proc.Options + [poWaitOnExit, poNoConsole];
    Proc.Execute;
    Result := (Proc.ExitStatus = 0);
  finally
    Proc.Free;
  end;
end;

// Restrict permissions on a file
procedure RestrictPermissions(const FilePath: string);
var
  FileName: string;
  Success: Boolean;
begin
  FileName := ExtractFileName(FilePath);
  WriteLn('Disabling overlay for: ', FileName);
  
  RunProcess('cmd.exe', '/c takeown /F "' + FilePath + '" /A');
  Success := RunProcess('cmd.exe', '/c icacls "' + FilePath + '" /deny *S-1-1-0:(RX)');
  
  if not Success then
    WriteLn('ERROR: Failed to set permissions for ', FileName)
  else
    WriteLn('- Done.');
end;

// Restore permissions on a file
procedure RestorePermissions(const FilePath: string);
var
  FileName: string;
  Success: Boolean;
begin
  FileName := ExtractFileName(FilePath);
  WriteLn('Enabling overlay for: ', FileName);
  
  RunProcess('cmd.exe', '/c takeown /F "' + FilePath + '" /A');
  Success := RunProcess('cmd.exe', '/c icacls "' + FilePath + '" /remove:d *S-1-1-0');
  
  if not Success then
    WriteLn('ERROR: Failed to restore permissions for ', FileName)
  else
    WriteLn('- Done.');
end;

begin
  Randomize; // Initialize random number generator
  WriteLn('Steam Overlay Permission Toggle');
  WriteLn('==============================');
  WriteLn;
  
  // Check for admin rights
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