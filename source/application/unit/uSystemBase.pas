unit uSystemBase;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, cm_generics,
  cm_interfaces, cm_messager, cm_parameter, cm_dialogs,
  uSystem, uVersion;

type

  { TAppSystemBase }

  TAppSystemBase = class abstract(TCMMessageable, IAppSystem)
  protected
    FStartTime: TDateTime;
    FIsLogined: Boolean;
    FLastLoginedTime: TDateTime;
    FLoginHandler: ILoginHandler;
    FSystemListenerList: TSystemListenerList;
    FLoadedExecuteList: TGInterfaceList<IRunnable>;
    FClosingExecuteList: TGInterfaceList<IRunnable>;
  public
    constructor Create;
    destructor Destroy; override;
  public //IAppSystem
    function GetVersion: string;
    function IsTestMode: Boolean;
    function GetStartTime: TDateTime;
    function IsLogined: Boolean;
    function GetLastLoginedTime: TDateTime;
    function GetParameter: ICMParameter; virtual; abstract;
    function GetMsgBar: ICMMsgBar; virtual; abstract;
    function GetMsgBox: ICMMsgBox; virtual; abstract;
    function GetLog: ICMLog; virtual; abstract;
    function GetWorkRect: TRect; virtual; abstract;
    function GetServiceRect: TRect; virtual; abstract;
    function SetLoginHandler(h: ILoginHandler): Boolean;
    procedure AddSystemListener(l: ISystemListener);
    procedure RemoveSystemListener(l: ISystemListener);
    function GetSystemListeners: TSystemListenerList;
    function IsActive: Boolean;
    procedure Close; virtual;
    procedure Terminate;
    //
    procedure AddLoadedOneOffExecute(rab: IRunnable);
    procedure AddClosingOneOffExecute(rab: IRunnable);
  end;

implementation

{ TAppSystemBase }

constructor TAppSystemBase.Create;
begin
  inherited Create;
  FStartTime := now;
  FIsLogined := False;
  FLastLoginedTime := MinDateTime;
  FLoginHandler := nil;
  FSystemListenerList := TSystemListenerList.Create;
  FLoadedExecuteList := TGInterfaceList<IRunnable>.Create;
  FClosingExecuteList := TGInterfaceList<IRunnable>.Create;
end;

destructor TAppSystemBase.Destroy;
begin
  FLoginHandler := nil;
  FSystemListenerList.Free;
  FLoadedExecuteList.Free;
  FClosingExecuteList.Free;
  inherited Destroy;
end;

function TAppSystemBase.GetVersion: string;
begin
  Result := VersionStr;
end;

function TAppSystemBase.IsTestMode: Boolean;
begin
  Result := False;
  {$IFDEF Test}
  Result := True;
  {$ENDIF}
end;

function TAppSystemBase.GetStartTime: TDateTime;
begin
  Result := FStartTime;
end;

function TAppSystemBase.IsLogined: Boolean;
begin
  Result := FIsLogined;
end;

function TAppSystemBase.GetLastLoginedTime: TDateTime;
begin
  Result := FLastLoginedTime;
end;

function TAppSystemBase.SetLoginHandler(h: ILoginHandler): Boolean;
begin
  Result := False;
  FLoginHandler := h;
  Result := Assigned(FLoginHandler);
end;

procedure TAppSystemBase.AddSystemListener(l: ISystemListener);
begin
  FSystemListenerList.Add(l);
end;

procedure TAppSystemBase.RemoveSystemListener(l: ISystemListener);
begin
  FSystemListenerList.Remove(l);
end;

function TAppSystemBase.GetSystemListeners: TSystemListenerList;
begin
  Result := FSystemListenerList;
end;

function TAppSystemBase.IsActive: Boolean;
begin
  Result := Application.Active;
end;

procedure TAppSystemBase.Close;
begin
  // TODO soso

  //
  Application.Terminate;
end;

procedure TAppSystemBase.Terminate;
begin
  Application.Terminate;
end;

procedure TAppSystemBase.AddLoadedOneOffExecute(rab: IRunnable);
begin
  FLoadedExecuteList.Add(rab);
end;

procedure TAppSystemBase.AddClosingOneOffExecute(rab: IRunnable);
begin
  FClosingExecuteList.Add(rab);
end;

end.

