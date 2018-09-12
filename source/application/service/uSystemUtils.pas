unit uSystemUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  cm_interfaces, cm_messager, cm_parameter, cm_dialogs,
  uSystem;

type

  { TPOSSystem }

  TPOSSystem = class(TCMMessageable, IPOSSystem)
  private
    FStartTime, FLoginTime: TDateTime;
    FParameter: ICMParameter;
    FMsgBox: TCMMsgBox;
    FLog: ICMLog;
    FWorkRect: TRect;
    procedure AppKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create;
    destructor Destroy; override;
    property StartTime: TDateTime read FStartTime;
    property LoginTime: TDateTime read FLoginTime write FLoginTime;
    property Parameter: ICMParameter read FParameter write FParameter;
    property Log: ICMLog read FLog write FLog;
    property WorkRect: TRect read FWorkRect write FWorkRect;
    procedure StartRecordKeyDown;
  public
    function GetVersion: string;
    function IsTestMode: Boolean;
    function GetStartTime: TDateTime;
    function GetLoginTime: TDateTime;
    function GetMsgBox: TCMMsgBox;
    function GetParameter: ICMParameter;
    function GetLog: ICMLog;
    function GetWorkRect: TRect;
  end;


const
  VersionStr: string = '0.0.2 alpha';

implementation

uses cm_controlutils, uDialogs;

{ TPOSSystem }

constructor TPOSSystem.Create;
begin
  inherited Create;
  FStartTime := now;
  FParameter := nil;
  FMsgBox := TPOSMsgBox.Create(Application);
  FWorkRect := Screen.DesktopRect;
end;

destructor TPOSSystem.Destroy;
begin
  inherited Destroy;
end;

procedure TPOSSystem.AppKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ds: string;
  cri: IInterfaceComponentReference;
begin
  ds := 'KeyDown:';
  if Assigned(Sender) then
    begin
      if Supports(Sender, IInterfaceComponentReference, cri) then
        begin
          ds := ds + cri.GetComponent.Name;
        end;
      ds := ds + '[' + Sender.UnitName + '.' + Sender.ClassName + ']';
    end;
  Messager.Debug('%s:%s', [ds, GetKeyCodeCHName(Key)]);
end;

//仅 Widdows 有效
procedure TPOSSystem.StartRecordKeyDown;
begin
  Messager.Info('开始记录按键信息...');
  Application.AddOnKeyDownHandler(@AppKeyDown);
end;

function TPOSSystem.GetVersion: string;
begin
  Result := VersionStr;
end;

function TPOSSystem.IsTestMode: Boolean;
begin
  Result := False;
  {$IFDEF Test}
  Result := True;
  {$ENDIF}
end;

function TPOSSystem.GetStartTime: TDateTime;
begin
  Result := FStartTime;
end;

function TPOSSystem.GetLoginTime: TDateTime;
begin
  Result := FLoginTime;
end;

function TPOSSystem.GetMsgBox: TCMMsgBox;
begin
  Result := FMsgBox;
end;

function TPOSSystem.GetParameter: ICMParameter;
begin
  Result := FParameter;
end;

function TPOSSystem.GetLog: ICMLog;
begin
  Result := FLog;
end;

function TPOSSystem.GetWorkRect: TRect;
begin
  Result := FWorkRect;
end;

end.

