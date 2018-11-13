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
    FSystemListenerList: TSystemListenerList;
    FLoadedExecuteList: TGInterfaceList<IOneOffExecute>;
    FClosingExecuteList: TGInterfaceList<IOneOffExecute>;
  public
    constructor Create;
    destructor Destroy; override;
  public //IAppSystem
    function GetVersion: string;
    function IsTestMode: Boolean;
    function GetStartTime: TDateTime;
    function GetParameter: ICMParameter; virtual; abstract;
    function GetMsgBar: ICMMsgBar; virtual; abstract;
    function GetMsgBox: ICMMsgBox; virtual; abstract;
    function GetLog: ICMLog; virtual; abstract;
    function GetWorkRect: TRect; virtual; abstract;
    function GetServiceRect: TRect; virtual; abstract;
    procedure AddSystemListener(l: ISystemListener);
    procedure RemoveSystemListener(l: ISystemListener);
    function GetSystemListeners: TSystemListenerList;
    function IsActive: Boolean;
    procedure Close; virtual;
    procedure Terminate;
    //
    procedure AddLoadedExecute(AExecute: IOneOffExecute);
    procedure AddClosingExecute(AExecute: IOneOffExecute);
  end;

implementation

{ TAppSystemBase }

constructor TAppSystemBase.Create;
begin
  inherited Create;
  FStartTime := now;
  FSystemListenerList := TSystemListenerList.Create;
  FLoadedExecuteList := TGInterfaceList<IOneOffExecute>.Create;
  FClosingExecuteList := TGInterfaceList<IOneOffExecute>.Create;
end;

destructor TAppSystemBase.Destroy;
begin
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

procedure TAppSystemBase.AddLoadedExecute(AExecute: IOneOffExecute);
begin
  FLoadedExecuteList.Add(AExecute);
end;

procedure TAppSystemBase.AddClosingExecute(AExecute: IOneOffExecute);
begin
  FClosingExecuteList.Add(AExecute);
end;

end.

