unit cm_system;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazFileUtils,
  {$IFDEF UNIX}
  Unix,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  JwaTlHelp32,
  registry,
  {$ENDIF}
  AsyncProcess,
  process;


  function RunApp(const AppName: string; const params: array of string; WaitOnExit: Boolean=False): Boolean;
  //本方法是粗暴的。 另可参见：pl_solutions LSUtils LSKillProcess
  function KillApp(const AppName: string): Boolean;
  function CMExecAsyncProcess(const ACommandLine: string): Boolean;
  //Copy and update by LSForms.pas
  function CMExecProcess(const ACommandLine: string): TStringList;
  function CMExecProcessWithShortOut(const ACommandLine: string): TStringList;
  {$IFDEF UNIX}
  function UnixSystem(const Command: string): LongInt;
  {$ENDIF}

  //Windows暂未实现
  function GetSystemMemoryUsage(out total, used: Integer): Boolean;
  function GetSystemMemoryUsage: Double;
  function GetProcessMemorySize(const APID: Integer): Integer;
  //
  function GetProcessList(const AShowPID: Boolean=True; const AIgnoreCurrent: Boolean=False): TStrings;
  function GetDesktopPath: string;

implementation

function RunApp(const AppName: string; const params: array of string; WaitOnExit: Boolean=False): boolean;
var
  process: TProcess;
  i: integer;
begin
  Result := False;
  try
    if not FileExistsUTF8(AppName) then
      Exit;
    process := TProcess.Create(nil);
    try
      {$IFDEF UNIX}
      fpSystem('sudo chmod a+r+w+x ' + AppName);
      {$ENDIF}
      process.Executable := AppName;
      for i := Low(params) to High(params) do
        process.Parameters.Add(params[i]);
      if WaitOnExit then
        process.Options := process.Options + [poWaitOnExit] + [poNoConsole]
      else
        process.Options := process.Options - [poWaitOnExit] + [poNoConsole];
      process.Execute;
    finally
      process.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function KillApp(const AppName: string): Boolean;
var
  process: TProcess;
begin
  Result := False;
  try
    process := TProcess.Create(nil);
    try
      process.Options := process.Options + [poNoConsole, poWaitOnExit];
      {$IFDEF UNIX}
      process.Executable := 'sudo killall';
      process.Parameters.Add(ExtractFileName(AppName));
      {$ELSE}
      process.Executable := 'taskkill';
      process.Parameters.Add('/f');
      process.Parameters.Add('/im');
      process.Parameters.Add(ExtractFileName(AppName));
      {$ENDIF}
      process.Execute;
    finally
      process.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function CMExecAsyncProcess(const ACommandLine: string): Boolean;
var
  aProcess: TAsyncProcess;
begin
  Result := False;
  try
    aProcess := TAsyncProcess.Create(nil);
    try
      aProcess.CommandLine := ACommandLine;
      aProcess.Execute;
      Sleep(10);
      Result := True;
    finally
      aProcess.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function CMExecProcess(const ACommandLine: string): TStringList;
const
  READ_BYTES = 2048;
var
  VMemoryStream: TMemoryStream;
  VProcess: TProcess;
  I64: LongInt;
  VBytesRead: LongInt;
begin
  Result := TStringList.Create;
  VMemoryStream := TMemoryStream.Create;
  VProcess := TProcess.Create(nil);
  try
    VBytesRead := 0;
    {$WARN SYMBOL_DEPRECATED OFF}
    VProcess.CommandLine := ACommandLine;
    {$WARN SYMBOL_DEPRECATED ON}
    VProcess.Options := [poUsePipes, poNoConsole];
    VProcess.Execute;
    while VProcess.Running do
    begin
      VMemoryStream.SetSize(VBytesRead + READ_BYTES);
      I64 := VProcess.Output.Read((VMemoryStream.Memory + VBytesRead)^, READ_BYTES);
      if I64 > 0 then
        Inc(VBytesRead, I64)
      else
        Sleep(100);
    end;
    repeat
      VMemoryStream.SetSize(VBytesRead + READ_BYTES);
      I64 := VProcess.Output.Read((VMemoryStream.Memory + VBytesRead)^, READ_BYTES);
      if I64 > 0 then
        Inc(VBytesRead, I64);
    until I64 <= 0;
    VMemoryStream.SetSize(VBytesRead);
    Result.LoadFromStream(VMemoryStream);
  finally
    VProcess.Free;
    VMemoryStream.Free;
  end;
end;

function CMExecProcessWithShortOut(const ACommandLine: string): TStringList;
var
  VProcess: TProcess;
begin
  Result := TStringList.Create;
  Result.NameValueSeparator := '=';
  VProcess := TProcess.Create(nil);
  try
    VProcess.Options := [poStderrToOutPut, poNoConsole, poUsePipes];
    {$WARN SYMBOL_DEPRECATED OFF}
    VProcess.CommandLine := ACommandLine;
    {$WARN SYMBOL_DEPRECATED ON}
    VProcess.Execute;
    Result.LoadFromStream(VProcess.Output);
  finally
    VProcess.Free;
  end;
end;

{$IFDEF UNIX}
function UnixSystem(const Command: string): LongInt;
begin
  Result := fpSystem(Command);
end;
{$ENDIF}

function GetSystemMemoryUsage(out total, used: Integer): Boolean;
{$IFDEF UNIX}
var
  ts: TStrings;
  tempStr: string;
{$ENDIF}
begin
  Result := False;
  total := -1;
  used := -1;
  try
    {$IFDEF UNIX}
    ts := CMExecProcess('free -k');
    if Assigned(ts) then
      begin
        try
          if ts.Count > 2 then
            begin
              if (Copy(ts[1], 1, 4) = 'Mem:') then
                begin
                  if (Copy(ts[2], 1, 3) = '-/+') then
                    begin
                      tempStr := ts[2];
                      ts.Delimiter := #9;
                      ts.DelimitedText := ts[1];
                      if ts.Count < 3 then
                        Exit;
                      if not TryStrToInt(ts[1], total) then
                        Exit;
                      //
                      ts.Delimiter := #9;
                      ts.DelimitedText := tempStr;
                      if ts.Count < 3 then
                        Exit;
                      if not TryStrToInt(ts[2], used) then
                        Exit;
                      Result := True;
                    end
                  else if (Copy(ts[2], 1, 5) = 'Swap:') then
                    begin
                      ts.Delimiter := #9;
                      ts.DelimitedText := ts[1];
                      if ts.Count < 7 then
                        Exit;
                      if not TryStrToInt(ts[1], total) then
                        Exit;
                      if TryStrToInt(ts[6], used) then
                        used := total - used
                      else
                        total := -1;
                      Result := True;
                    end;
                end;
            end;
        finally
          ts.Free;
        end;
      end;
    {$ENDIF}
  except
  end;
end;

function GetSystemMemoryUsage: Double;
var
  total, used: Integer;
begin
  Result := -1;
  if GetSystemMemoryUsage(total, used) then
    Result := used / total;
end;

function GetProcessMemorySize(const APID: Integer): Integer;
var
  ts: TStrings;
begin
  Result := -1;
  {$IFDEF UNIX}
  ts := CMExecProcess('sh -c "ps -eo ''pid,rss'' | grep ''\<' + IntToStr(APID) + '\>''"');
  try
    if ts.Count > 0 then
  {$ELSE}
  ts := CMExecProcess('tasklist /nh /fi "PID eq ' + IntToStr(APID) + '"');
  try
    if ts.Count > 1 then
  {$ENDIF}
      begin
        ts.Delimiter := #9;
        {$IFDEF UNIX}
        ts.DelimitedText := ts[0];
        if ts.Count >= 2 then
          begin
            if Trim(ts[0]) = IntToStr(APID) then
              Result := StrToIntDef(ts[1], -1)
          end;
        {$ELSE}
        ts.DelimitedText := ts[1];
        if ts.Count >= 5 then
          Result := StrToIntDef(StringReplace(ts[4], ',', '', [rfReplaceAll]), -1);
        {$ENDIF}
      end;
  finally
    ts.Free;
  end;
end;

function GetProcessList(const AShowPID: Boolean=True; const AIgnoreCurrent: Boolean=False): TStrings;
var
  {$IFDEF UNIX}
  i: Integer;
  {$ELSE}
  VSnapshotHandle: THandle;
  VProcessEntry32: TProcessEntry32;
  {$ENDIF}
begin
  {$IFDEF UNIX}
  if not AIgnoreCurrent then
    begin
      if AShowPID then
        Result := CMExecProcess('sh -c "ps -A | awk ''{print $4 "=" $1}''"')
      else
        Result := CMExecProcess('sh -c "ps -A | awk ''{print $4}''"');
    end
  else
    begin
      Result := CMExecProcess('sh -c "ps -A | awk ''{print $4 "=" $1}''"');
      for i := Result.Count-1 downto 0 do
        begin
          if StrToIntDef(Result.ValueFromIndex[i], -1) = Integer(GetProcessID) then
            begin
              Result.Delete(i);
              Continue;
            end;
          if not AShowPID then
            Result.Strings[i] := Result.Names[i];
        end;
    end;
  {$ELSE}
  //Result := CMExecProcess('tasklist /nh');
  Result := TStringList.Create;
  try
    VSnapshotHandle := CreateToolHelp32SnapShot(TH32CS_SNAPALL, 0);
    VProcessEntry32.dwSize := SizeOf(TProcessEntry32);
    Process32First(VSnapshotHandle, VProcessEntry32);
    repeat
      if AIgnoreCurrent and (GetProcessID = VProcessEntry32.th32ProcessID) then
        Continue;
      if AShowPID then
        Result.Add(VProcessEntry32.szExeFile + '=' + IntToStr(VProcessEntry32.th32ProcessID))
      else
        Result.Add(VProcessEntry32.szExeFile);
    until (not Process32Next(VSnapshotHandle, VProcessEntry32));
  except
  end;
  {$ENDIF}
end;

function GetDesktopPath: string;
{$IFDEF MSWINDOWS}
const
  RegPath = '\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
var
  Reg: TRegistry;
{$ENDIF}
{$IFDEF UNIX}
var
  ts: TStrings;
  str: string;
{$ENDIF}
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(RegPath, false) then begin
      Result := Reg.ReadString('Desktop') + '\';
    end;
  finally
    Reg.Free;
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  ts := cm_system.CMExecProcess('sh -c "echo $USER"');
  try
    str := Trim(ts.Text);
  finally
    ts.Free;
  end;
  if str = 'root' then
    Result := '/root/Desktop/'
  else
    Result := '/home/' + str + '/Desktop/';
  {$ENDIF}
end;

end.

