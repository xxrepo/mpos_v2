unit cm_threadutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


type

  { TRunMethodThread }

  TRunMethodThread = class(TThread)
  private
    FMethod: TThreadMethod;
  public
    constructor Create(AExecuteMethod: TThreadMethod);
    procedure Execute; override;
  end;

  { TExecuteThread }

  TExecuteThread = class(TThread)
  private
    FTM: TThreadMethod;
  protected
    procedure Execute; override;
  public
    constructor Create;
    function ExecuteProc(ATM: TThreadMethod): Boolean;
  end;

  { TSyncExecuteThread }

  TSyncExecuteThread = class(TExecuteThread)
  protected
    procedure Execute; override;
  end;

  { TExecuteThreadBool }

  TExecuteThreadBool = class(TComponent)
  private
    FThreadList: TThreadList;
    FSyncThreadList: TThreadList;
    FThreadCount: Integer;
    FSyncThreadCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetExecuteThread: TExecuteThread;
    function GetSyncExecuteThread: TSyncExecuteThread;
    function ExecuteProc(ATM: TThreadMethod): Boolean;
    function SyncExecuteProc(ATM: TThreadMethod): Boolean;
    property ThreadCount: Integer read FThreadCount write FThreadCount;
    property SyncThreadCount: Integer read FSyncThreadCount write FSyncThreadCount;
  end;

implementation

{ TRunMethodThread }

constructor TRunMethodThread.Create(AExecuteMethod: TThreadMethod);
begin
  FMethod := AExecuteMethod;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TRunMethodThread.Execute;
begin
  if Assigned(FMethod) then
    FMethod();
end;

{TExecuteThread}

constructor TExecuteThread.Create;
begin
  FTM := nil;
  inherited Create(False);
end;

procedure TExecuteThread.Execute;
begin
  FreeOnTerminate := False;
  while not Self.Terminated do
    begin
      try
        if Assigned(FTM) then
          begin
            FTM();
          end;
      finally
        Self.Sleep(10);
        FTM := nil;
        Self.Suspended := True;
      end;
    end;
end;

function TExecuteThread.ExecuteProc(ATM: TThreadMethod): Boolean;
begin
  Result := False;
  if FTM <> nil then
    begin
      Self.Sleep(20);
      if FTM <> nil then
        Exit;
    end;
  FTM := ATM;
  Result := True;
  (***************************************************************)
  Self.Suspended := False;
  //有时并未能继续，必须如此
  if Self.Suspended then
    //Self.Resume;
    Self.Suspended := False;
end;

{ TSyncExecuteThread }

procedure TSyncExecuteThread.Execute;
begin
  FreeOnTerminate := False;
  while not Self.Terminated do
    begin
      try
        if Assigned(FTM) then
          begin
            Self.Synchronize(FTM);
          end;
      finally
        Self.Sleep(10);
        FTM := nil;
        Self.Suspended := True;
      end;
    end;
end;

{TExecuteThreadBool}

constructor TExecuteThreadBool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadList := TThreadList.Create;
  FSyncThreadList := TThreadList.Create;
  FThreadCount := 4;
  FSyncThreadCount := 2;
end;

destructor TExecuteThreadBool.Destroy;
begin
  FThreadList.Free;
  FSyncThreadList.Free;
  inherited Destroy;
end;

function TExecuteThreadBool.GetExecuteThread: TExecuteThread;
var
  i: Integer;
  theThread: TExecuteThread;
  list: TList;
  cc: Integer;
begin
  Result := nil;
  cc := 0;
  list := FThreadList.LockList;
  try
    repeat
      for i:=0 to list.Count-1 do
        begin
          theThread := TExecuteThread(list.Items[i]);
          if Assigned(theThread) then
            begin
              if theThread.Suspended then
                begin
                  Result := theThread;
                  Exit;
                end;
            end;
        end;
      //
      Sleep(100);
      cc := cc + 1;
      if cc > 1000 then
        Break;
    until list.Count < FThreadCount;
  finally
    FThreadList.UnlockList;
  end;
  //
  theThread := TExecuteThread.Create;
  FThreadList.Add(theThread);
  Result := theThread;
end;

function TExecuteThreadBool.GetSyncExecuteThread: TSyncExecuteThread;
var
  i: Integer;
  theThread: TSyncExecuteThread;
  list: TList;
  cc: Integer;
begin
  Result := nil;
  cc := 0;
  list := FSyncThreadList.LockList;
  try
    repeat
      for i:=0 to list.Count-1 do
        begin
          theThread := TSyncExecuteThread(list.Items[i]);
          if Assigned(theThread) then
            begin
              if theThread.Suspended then
                begin
                  Result := theThread;
                  Exit;
                end;
            end;
        end;
      //
      Sleep(100);
      cc := cc + 1;
      if cc > 1000 then
        Break;
    until list.Count < FSyncThreadCount;
  finally
    FSyncThreadList.UnlockList;
  end;
  //
  theThread := TSyncExecuteThread.Create;
  FSyncThreadList.Add(theThread);
  Result := theThread;
end;

function TExecuteThreadBool.ExecuteProc(ATM: TThreadMethod): Boolean;
var
  t: TExecuteThread;
begin
  Result := False;
  t := GetExecuteThread;
  if Assigned(t) then
    Result := t.ExecuteProc(ATM);
end;

function TExecuteThreadBool.SyncExecuteProc(ATM: TThreadMethod): Boolean;
var
  t: TSyncExecuteThread;
begin
  Result := False;
  t := GetSyncExecuteThread;
  if Assigned(t) then
    Result := t.ExecuteProc(ATM);
end;




end.

