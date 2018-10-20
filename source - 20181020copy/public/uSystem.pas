unit uSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_parameter, cm_dialogs,
  cm_Plat;

type

  IAppSystem = interface(ICMBase)
    ['{81D73F7E-8FBE-4D85-9E58-C787DD18192E}']
    function GetVersion: string;
    function IsTestMode: Boolean;
    function GetStartTime: TDateTime;
    function GetLoginTime: TDateTime;
    function GetMsgBox: TCMMsgBox;
    function GetParameter: ICMParameter;
    function GetLog: ICMLog;
    function GetWorkRect: TRect;
  end;

  ISystemEvent = interface(ICMEvent)
    ['{BBEE1CF6-5658-45B7-883C-34D04AA050E9}']
    function GetSystem: IAppSystem;
  end;

  ISystemListener = interface(ICMListener)
    ['{F28C402D-EE0B-43C5-A834-0CCA0568B11B}']
    procedure Loaded(e: ISystemEvent);
    procedure Logined(e: ISystemEvent);
    procedure Showed(e: ISystemEvent);
  end;

function AppSystem: IAppSystem;
procedure NilAppSystem;

implementation

var
  _appSystem: IAppSystem = nil;

function AppSystem: IAppSystem;
begin
  Result := nil;
  if not Assigned(_appSystem) and Assigned(InterfaceRegister) then
    InterfaceRegister.OutInterface(IAppSystem, _appSystem);
  Result := _appSystem;
end;

procedure NilAppSystem;
begin
  _appSystem := nil;
end;


end.

