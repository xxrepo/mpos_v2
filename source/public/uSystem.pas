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

const
  VersionStr: string = '0.1.0 alpha';

function AppSystem: IAppSystem;
procedure NilAppSystem;

implementation

var
  _appSystem: IAppSystem = nil;

function AppSystem: IAppSystem;
begin
  Result := nil;
  if Assigned(_appSystem) then
    Result := _appSystem
  else if Assigned(InterfaceRegister) then
    InterfaceRegister.OutInterface(IAppSystem, Result);
end;

procedure NilAppSystem;
begin
  _appSystem := nil;
end;


end.

