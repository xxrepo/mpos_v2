{
    This file is part of the CM Kit.
    Copyright (c) 2013-2017 by the ChenMeng studio

    logutils

    This is not a complete unit,
    Here is the copy part of the CMKit, for testing

 **********************************************************************}

unit cm_logutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF UNIX}
  Unix,
  {$ENDIF}
  dateutils,
  syncobjs,
  FileUtil,
  cm_sysutils,
  cm_interfaces;

type

  ECMLogError = class(Exception);

  //Update by TEventLog

  { TCMFileLogger }

  TCMFileLogger = Class(TCMBaseComponent, ICMLog)
  Private
    fAppendContent : Boolean;
    FStream : TFileStream;
    FActive: Boolean;
    FRaiseExceptionOnError: Boolean;
    FIdentification: String;
    FFileName: String;
    FTimeStampFormat: String;
    FPaused : Boolean;
    FCloseTimeStamp: Boolean;
    FLastLog: string;
    FCriticalSection: TCriticalSection;
    procedure SetActive(const Value: Boolean);
    procedure SetIdentification(const Value: String);
    procedure ActivateLog;
    procedure DeActivateLog;
    //
    function DefaultFileName: string;
    function getLogFileSize: Integer;
    function getCurrentFileStream: TFileStream;
    procedure doLog(EventType: TEventType; const Msg: String; DateTime: TDateTime);
  Protected
    procedure WriteFileLog(EventType: TEventType; const Msg: String; DateTime: TDateTime); virtual;
    procedure SetFileName(const Value: String); virtual;
    Procedure CheckInactive;  //只用于设置文件名时，检查激活，如果有激活状态 抛出 SErrOperationNotAllowed
    Procedure EnsureActive;   //保证
  Public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure Pause;
    Procedure Resume;
    Procedure Log(AEventType: TEventType; const AMsg: String; DateTime: TDateTime); overload;
    procedure Log(AEventType: TEventType; const AMsg: string); overload;
  public
    procedure Custom(const AMsg: string);
    procedure Debug(const AMsg: string);
    procedure Info(const AMsg: string);
    procedure Warning(const AMsg: string);
    procedure Error(const AMsg: string);
    procedure Error(const AMsg: string; Ex: Exception);
  Published
    Property AppendContent: Boolean Read fAppendContent Write fAppendContent;
    Property Identification: String Read FIdentification Write SetIdentification;    //时间前的识别
    Property Active: Boolean Read FActive write SetActive;
    Property RaiseExceptionOnError: Boolean Read FRaiseExceptionOnError Write FRaiseExceptionOnError;
    Property FileName: String Read FFileName Write SetFileName;
    Property TimeStampFormat: String Read FTimeStampFormat Write FTimeStampFormat;
    Property Paused: Boolean Read FPaused Write FPaused;
    //
    property FileSize: Integer read getLogFileSize;
    property CurrentFileStream: TFileStream read getCurrentFileStream;
    property CloseTimeStamp: Boolean read FCloseTimeStamp write FCloseTimeStamp;
    property LastLog: string read FLastLog write FLastLog;
  End;

  TCMJointFileLogger = class(TCMFileLogger)
  protected
    FCurrentDayOf: Word;
    FFileSizeLimit: Integer;
    FFilePath: string;
    FPrefix: string;
    FSuffix: string;
    FFileIndex: Integer;
    procedure setNewFileName;
    procedure WriteFileLog(EventType: TEventType; const Msg: String; DateTime: TDateTime); override;
  private
    procedure setFilePath(const AFilePath: string);
    procedure setPrefix(const APrefix: string);
    procedure setSuffix(const ASuffix: string);
  public
    constructor Create(AOwner: TComponent); override;
    property FilePath: string read FFilePath write setFilePath;
    property FileNamePrefix: string read FPrefix write setPrefix;
    property FileNameSuffix: string read FSuffix write setSuffix;
    property FileSizeLimit: Integer read FFileSizeLimit write FFileSizeLimit; //default 5242880
  end;

const
  EventTypeNames: array[TEventType] of string = ('Custom','Info','Warning','Error','Debug');

Resourcestring
  SErrLogFailedMsg = 'Failed to log entry (Error: %s)';
  SErrOperationNotAllowed = 'Operation not allowed when eventlog is active.';

implementation

{TCMJointFileLogger}

constructor TCMJointFileLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileSizeLimit := 5242880;
  FFilePath := ExtractFilePath(DefaultFileName);
  FPrefix := '';
  FSuffix := '';
  FFileIndex := 1;
  FCurrentDayOf := DayOf(now);
  setNewFileName;
end;

procedure TCMJointFileLogger.setFilePath(const AFilePath: string);
var
  tempStr: string;
begin
  tempStr := ExtractFilePath(AFilePath);
  if tempStr <> '' then
    begin
      FFilePath := tempStr;
      {$IFDEF UNIX}
      if FFilePath <> '' then
        fpSystem('sudo chmod a+r+w+x ' + FFilePath + '*');
      {$ENDIF}
      setNewFileName;
    end;
end;

procedure TCMJointFileLogger.setPrefix(const APrefix: string);
begin
  FPrefix := APrefix;
  setNewFileName;
end;

procedure TCMJointFileLogger.setSuffix(const ASuffix: string);
begin
  FSuffix := ASuffix;
  setNewFileName;
end;

procedure TCMJointFileLogger.setNewFileName;
var
  tempFileName: string;
  tempDStr: string;
begin
  Self.Active := False;
  {$IFDEF UNIX}
  fpSystem('sudo chmod a+r+w+x ' + ExtractFilePath(paramStr(0)) + '*');
  {$ENDIF}
  if not DirectoryExists(FFilePath) then
    ForceDirectories(FFilePath);
  //
  tempDStr := FormatDateTime('yyyyMMdd', now);
  if FFileIndex <= 1 then
    tempFileName := FFilePath + FPrefix + tempDStr + FSuffix + '.log'
  else
    tempFileName := FFilePath + FPrefix + tempDStr + FSuffix + '_' + IntToStr(FFileIndex) + '.log';
  Self.FileName := tempFileName;
  //
  while FileExists(tempFileName) and (FileUtil.FileSize(tempFileName) >= FFileSizeLimit) do
    begin
      FFileIndex := FFileIndex + 1;
      tempFileName := FFilePath + FPrefix + tempDStr + FSuffix + '_' + IntToStr(FFileIndex) + '.log';
      Self.FileName := tempFileName;
    end;
  //
  if not DirectoryExists(FFilePath) then
    ForceDirectories(FFilePath);
end;

procedure TCMJointFileLogger.WriteFileLog(EventType: TEventType; const Msg: String; DateTime: TDateTime);
var
  isNewLog: Boolean;
begin
  isNewLog := False;
  if Self.FileSize > FFileSizeLimit then
    begin
      FFileIndex := FFileIndex + 1;
      isNewLog := True;
    end;
  if DayOf(now) <> FCurrentDayOf then
    begin
      FCurrentDayOf := DayOf(now);
      FFileIndex := 1;
      isNewLog := True;
    end;
  //
  if isNewLog then
    begin
      setNewFileName;
      Sleep(10);
    end;
  //
  inherited WriteFileLog(EventType, Msg, DateTime);
end;

{ TCMFileLogger }

function TCMFileLogger.DefaultFileName: string;
begin
  {$IFDEF UNIX}
  Result:='/tmp/'+ChangeFileExt(ExtractFileName(Paramstr(0)),'.log');
  {$ELSE}
  Result := ChangeFileExt(Paramstr(0),'.log');
  {$ENDIF}
end;

procedure TCMFileLogger.CheckInactive;
begin
  If Active then
    Raise ECMLogError.Create(SErrOperationNotAllowed);
end;

procedure TCMFileLogger.EnsureActive;
begin
  If Not Active then
    Active:=True;
end;

procedure TCMFileLogger.Pause;
begin
  Paused:=True;
end;

procedure TCMFileLogger.Resume;
begin
  Paused:=False;
end;

procedure TCMFileLogger.Log(AEventType: TEventType; const AMsg: String; DateTime: TDateTime);
begin
  If Paused then
    Exit;
  DoLog(AEventType, AMsg, DateTime);
end;

procedure TCMFileLogger.Log(AEventType: TEventType; const AMsg: String);
begin
  Log(AEventType, AMsg, now);
end;

procedure TCMFileLogger.Custom(const AMsg: string);
begin
  Log(etCustom, AMsg);
end;

procedure TCMFileLogger.Debug(const AMsg: string);
begin
  Log(etDebug, AMsg);
end;

procedure TCMFileLogger.Info(const AMsg: string);
begin
  Log(etInfo, AMsg);
end;

procedure TCMFileLogger.Warning(const AMsg: string);
begin
  Log(etWarning, AMsg);
end;

procedure TCMFileLogger.Error(const AMsg: string);
begin
  Log(etError, AMsg);
end;

procedure TCMFileLogger.Error(const AMsg: string; Ex: Exception);
begin
  Error(AMsg + ' ' + Ex.ClassName + ' ' + Ex.Message);
end;

procedure TCMFileLogger.doLog(EventType: TEventType; const Msg: String; DateTime: TDateTime);
begin
  //系统关闭时因为环境关闭的顺应u问题，直接使用 Enter() 可能会b出错
  //FCriticalSection.Enter;
  if FCriticalSection.TryEnter then
  try
    WriteFileLog(EventType, Msg, DateTime);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TCMFileLogger.WriteFileLog(EventType: TEventType; const Msg : String; DateTime: TDateTime);
var
  S,TS,T : String;
begin
  EnsureActive;
  //
  TS:=FormatDateTime(FTimeStampFormat, DateTime, CMFormatSettings);
  T := EventTypeNames[EventType];
  if FCloseTimeStamp then
    S:=Format('%s%s',[Msg,LineEnding])
  else if Identification = '' then
    S:=Format('[%s %s] %s%s',[TS,T,Msg,LineEnding])
  else
    S:=Format('%s [%s %s] %s%s',[Identification,TS,T,Msg,LineEnding]);
  FLastLog := S;
  try
    FStream.WriteBuffer(S[1],Length(S));
    S:='';
  except
    On E : Exception do
      S:=E.Message;
  end;
  If (S<>'') and RaiseExceptionOnError then
    Raise ECMLogError.CreateFmt(SErrLogFailedMsg,[S]);
end;

function TCMFileLogger.getCurrentFileStream: TFileStream;
begin
  Result := nil;
  FCriticalSection.Enter;
  try
    if Assigned(FStream) then
      begin
        FStream.Position := 0;
        try
          Result.CopyFrom(FStream, FStream.Size);
          Result.Position := 0;
        finally
          FStream.Position := FStream.Size;
        end;
      end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TCMFileLogger.SetActive(const Value: Boolean);
begin
  If Value<>FActive then
    begin
      If Value then
        ActivateLog
      else
        DeActivateLog;
      FActive:=Value;
    end;
end;

procedure TCMFileLogger.ActivateLog;
var
  fFileFlags : Word;
begin
  If (FFileName='') then
    FFileName:=DefaultFileName;
  // This will raise an exception if the file cannot be opened for writing !
  if fAppendContent and FileExists(FFileName) then
    fFileFlags := fmOpenWrite
  else
    fFileFlags := fmCreate;

  fFileFlags := fFileFlags or fmShareDenyWrite;
  FStream:=TFileStream.Create(FFileName,fFileFlags);
  if fAppendContent then
    FStream.Seek(0,soFromEnd);
end;

procedure TCMFileLogger.DeActivateLog;
begin
  FStream.Free;
  FStream:=Nil;
end;

function TCMFileLogger.getLogFileSize: Integer;
begin
  Result := -1;
  if Active then
    begin
      Result := FStream.Size;
    end
  else
    begin
      if FileExists(Self.FileName) then
        Result := FileUtil.FileSize(Self.FileName);
    end;
end;

procedure TCMFileLogger.SetIdentification(const Value: String);
begin
  FIdentification := Value;
end;

procedure TCMFileLogger.SetFileName(const Value: String);
begin
  CheckInactive;
  FFileName := Value;
  {$IFDEF UNIX}
  fpSystem('sudo chmod a+r+w+x ' + ExtractFilePath(Value) + '*');
  {$ENDIF}
end;

constructor TCMFileLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AppendContent := True;
  RaiseExceptionOnError := True;
  FCloseTimeStamp := False;
  FCriticalSection := TCriticalSection.Create;
  FTimeStampFormat := 'yyyy-mm-dd hh:nn:ss.zzz';
end;

destructor TCMFileLogger.Destroy;
begin
  Active:=False;
  if Assigned(FCriticalSection) then
    FCriticalSection.Free;
  inherited;
end;

end.

