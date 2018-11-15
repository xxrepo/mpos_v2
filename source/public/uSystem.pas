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
    ['{EE4E0919-4AA1-43D0-8BCD-ACA40FA59DBA}']
    procedure Loaded(e: ISystemEvent);     //加载完所有模块
    procedure Prepared(e: ISystemEvent);   //所有准备完毕（有界面时包含主界面准备完毕）
    procedure Logined(e: ISystemEvent);    //登录完成后
    procedure Logoutting(e: ISystemEvent); //登出时
    procedure Closing(e: ISystemEvent);    //关闭时
  end;

  TSystemListenerList = TGInterfaceList<ISystemListener>;

  TSystemListenerListEnumerator = TGInterfaceListEnumerator<ISystemListener>;

  ILoginHandler = interface(ICMBase)
    ['{52F06A91-BA2A-469E-BCAB-D2C726B1E06B}']
    function DoLogin: Boolean;
    function DoLogout: Boolean;
  end;

  IAppSystem = interface(ICMBase)
    ['{545FF2FD-A934-4D59-810D-412E01614F23}']
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
    procedure AddPreparedOneOffExecute(rab: IRunnable);
    procedure AddLoginedOneOffExecute(rab: IRunnable);
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

