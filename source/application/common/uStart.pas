unit uStart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  cm_messager, cm_InterfaceLoader,
  cm_Plat,
  uConstant, uSystem,
  uInitialize,
  uFormLoading;

type

  { TPOSStart }

  TPOSStart = class(TCMMessageableComponent)
  private
    FInitialize: TPOSInitialize;
  public
    procedure Init;
    procedure Start;
    procedure AfterLogin;
    procedure SetWorkRectControl(AControl: TControl);
  end;

var
  POSStart: TPOSStart = nil;

implementation

uses TypInfo, cm_classes;

{ TPOSStart }

procedure TPOSStart.Init;
begin
  //1、初始化工具
  FInitialize := TPOSInitialize.Create(Application);
  //2、因为自己在初始化之前，所在要设置 MessageHandler;
  Self.Messager.AddMessageHandler(cm_messager.TCMMessageManager.DefaultHandler);
  //3、打开加载画面
  Messager.Debug('创建加载画面...');
  LoadingForm := TLoadingForm.Create(Application);
  LoadingForm.Show;
  LoadingForm.SetLoadMsg('系统开始启动...', 2);
end;

//在创建 main form 之后
procedure TPOSStart.Start;
var
  icoFileName: string;
  loader: ICMInterfaceLoader;
  mLevelStr: string;
  mLevel: TEventTypeLevel;
begin
  LoadingForm.SetLoadMsg('开始初始化基础工具...', 3);
  FInitialize.InitHeadmost;
  LoadingForm.SetLoadMsg('开始初始化参数相关操作...');
  FInitialize.InitParameter;
  //
  LoadingForm.SetLoadMsg('开始加载应用图标...');
  icoFileName := AppSystem.GetParameter.Get(IcoParameterName).AsString;
  if FileExists(icoFileName) then
    Application.Icon.LoadFromFile(icoFileName)
  else
    Messager.Error('%s 文件:%s 不存在.', [IcoParameterName, icoFileName]);
  //
  LoadingForm.SetLoadMsg('开始设置默认信息等级...');
  mLevelStr := AppSystem.GetParameter.Get(MessageLevelParameterName).AsString;
  if mLevelStr <> '' then
    begin
      Messager.Info('设置默认信息等级为：%s', [mLevelStr]);
      mLevel := TEventTypeLevel(GetEnumValue(TypeInfo(TEventTypeLevel), mLevelStr));
      TCMMessageManager.DefaultHandler.SetLevel(mLevel);
    end;
  //
  LoadingForm.SetLoadMsg('开始初始化 LCL 组件库相关操作工具...');
  FInitialize.InitLCLOperate;
  //
  LoadingForm.SetLoadMsg('开始初始化主题工具...');
  FInitialize.InitTheme;
  //
  LoadingForm.SetLoadMsg('开始数据库信息处理器...');
  FInitialize.InitDBMessageHandler;
  //
  {$IFDEF Windows}
  LoadingForm.SetLoadMsg('开始启用记录按键...');
  FInitialize.StartRecordKeyDown;
  {$ENDIF}
  //---- 以下加载 ----------------------------------------------------------------------------------
  LoadingForm.SetLoadMsg('开始加载基本服务...', 50);
  loader := nil;
  if InterfaceRegister.OutInterface(ICMInterfaceLoader, loader) then
    loader.LoadByConfig(LibrariesConfigFileName);
  //
  LoadingForm.SetLoadMsg('开始加载业务...', 70);
  if Assigned(loader) then
    loader.LoadDirAll(ModulesPath);
  //
  LoadingForm.SetLoadMsg('加载完成,准备显示主操作界面...', 100);
  LoadingForm.Close;
end;

procedure TPOSStart.AfterLogin;
begin
  Messager.Info('AfterLogin()...');

end;

procedure TPOSStart.SetWorkRectControl(AControl: TControl);
begin
  FInitialize.WorkRectControl := AControl;
end;


initialization
  POSStart := TPOSStart.Create(Application);

end.

