unit uSystem;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_parameter, cm_dialogs, cm_generics,
  cm_plat;

type

  IAppSystem = interface;

  ISystemEvent = interface(ICMEvent)
    ['{BBEE1CF6-5658-45B7-883C-34D04AA050E9}']
    function GetSystem: IAppSystem;
  end;

  ISystemListener = interface(ICMListener)
    ['{DE6FAA48-4734-4362-973D-8A6D152A2090}']
    procedure Loaded(e: ISystemEvent);
    procedure Closing(e: ISystemEvent);
  end;

  TSystemListenerList = TGInterfaceList<ISystemListener>;

  TSystemListenerListEnumerator = TGInterfaceListEnumerator<ISystemListener>;

  IAppSystem = interface(ICMBase)
    ['{B7FDA120-2DD9-4ADF-996F-96BA9A3A38E6}']
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
    procedure RemoveSystemListener(l: ISystemListener);
    function GetSystemListeners: TSystemListenerList;
    function IsActive: Boolean;
    procedure Close;
    procedure Terminate;
    // 执行体执行后消除引用
    procedure AddLoadedExecute(AExecute: IOneOffExecute);
    procedure AddClosingExecute(AExecute: IOneOffExecute);
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

