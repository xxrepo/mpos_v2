unit uSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_parameter, cm_dialogs,
  cm_plat;

type

  IAppSystem = interface;

  ISystemEvent = interface(ICMEvent)
    ['{BBEE1CF6-5658-45B7-883C-34D04AA050E9}']
    function GetSystem: IAppSystem;
  end;

  ISystemListener = interface(ICMListener)
    ['{F28C402D-EE0B-43C5-A834-0CCA0568B11B}']
    procedure Loaded(e: ISystemEvent);
    procedure Logined(e: ISystemEvent);
    procedure LoggedOut(e: ISystemEvent);
    procedure Closing(e: ISystemEvent);
  end;

  IAppSystem = interface(ICMBase)
    ['{0D16D9B0-C131-4A14-B14A-E55FB2EFE41D}']
    function GetVersion: string;           //系统版本
    function IsTestMode: Boolean;
    function GetStartTime: TDateTime;      //启动时间
    function GetParameter: ICMParameter;   //配置参数
    function GetMsgBar: ICMMsgBar;         //消息显示条
    function GetMsgBox: ICMMsgBox;         //消息显示框
    function GetLog: ICMLog;               //系统日志
    function GetWorkRect: TRect;           //系统在屏幕工作区域
    function GetServiceRect: TRect;        //分配给业务在屏幕的工作区域
    procedure AddSystemListener(l: ISystemListener);  //添加指定的系统侦听器
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

