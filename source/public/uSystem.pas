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
    ['{6F93ABB7-7463-44BD-9946-1CDFAE1EA7CE}']
    procedure Loaded(e: ISystemEvent);
    procedure Logined(e: ISystemEvent);
    procedure Logoutting(e: ISystemEvent);
    procedure Closing(e: ISystemEvent);
  end;

  TSystemListenerList = TGInterfaceList<ISystemListener>;

  TSystemListenerListEnumerator = TGInterfaceListEnumerator<ISystemListener>;

  ILoginHandler = interface(ICMBase)
    ['{52F06A91-BA2A-469E-BCAB-D2C726B1E06B}']
    function DoLogin: Boolean;
    function DoLogout: Boolean;
  end;

  IAppSystem = interface(ICMBase)
    ['{A48444D8-6690-4430-B8AA-A517FD2D9FC8}']
    function GetVersion: string;           //系统版本
    function IsTestMode: Boolean;
    function GetStartTime: TDateTime;      //启动时间
    function IsLogined: Boolean;
    function GetLastLoginedTime: TDateTime;
    function GetParameter: ICMParameter;   //配置参数
    function GetMsgBar: ICMMsgBar;         //消息显示条
    function GetMsgBox: ICMMsgBox;         //消息显示框
    function GetLog: ICMLog;               //系统日志
    function GetWorkRect: TRect;           //系统在屏幕工作区域
    function GetServiceRect: TRect;        //分配给业务的工作区域
    function SetLoginHandler(h: ILoginHandler): Boolean; //设置登陆处理器，应在加载时设置
    procedure AddSystemListener(l: ISystemListener);     //添加指定的系统侦听器
    procedure RemoveSystemListener(l: ISystemListener);
    function GetSystemListeners: TSystemListenerList;
    function IsActive: Boolean;
    procedure Close;
    procedure Terminate; //除非异常，否则慎用
    // 执行体执行后消除引用
    procedure AddLoadedOneOffExecute(rab: IRunnable);
    procedure AddClosingOneOffExecute(rab: IRunnable);
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

