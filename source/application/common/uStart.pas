unit uStart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  cm_messager, cm_InterfaceLoader,
  cm_Plat,
  uConstant, uSystem,
  uInitialize,
  uFormLoading,
  uDB, uDAO;

type

  { TPOSStart }

  TPOSStart = class(TCMMessageableComponent)
  private
    FInitialize: TPOSInitialize;
    FLastPosition: Integer;
    procedure SetLoadMsg(const AMsg: string);
    procedure SetLoadMsg(const AMsg: string; APosition: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init;
    procedure Start;
    procedure AfterLogin;
    procedure SetWorkRect(ARect: TRect);
  end;

var
  POSStart: TPOSStart = nil;

implementation

{ TPOSStart }

procedure TPOSStart.SetLoadMsg(const AMsg: string);
begin
  FLastPosition := FLastPosition + 1;
  Self.SetLoadMsg(AMsg, FLastPosition);
end;

procedure TPOSStart.SetLoadMsg(const AMsg: string; APosition: Integer);
begin
  FLastPosition := APosition;
  LoadingForm.Step(AMsg, FLastPosition);
  Messager.Debug('设置加载信息: [pos:%.2d msg:%s]', [FLastPosition, AMsg]);
end;

constructor TPOSStart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastPosition := 0;
end;

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
  SetLoadMsg('系统开始启动...', 2);
end;

//在创建 main form 之后
procedure TPOSStart.Start;
var
  icoFileName: string;
  loader: ICMInterfaceLoader;
  sta: IPOSStatement;
begin
  SetLoadMsg('开始初始化参数相关操作...', 4);
  FInitialize.InitParameter;
  //
  SetLoadMsg('开始加载应用图标...');
  icoFileName := AppSystem.GetParameter.Get('mpos.resources.ico').AsString;
  if FileExists(icoFileName) then
    Application.Icon.LoadFromFile(icoFileName)
  else
    Messager.Error('mpos.resources.ico文件:%s不存在.', [icoFileName]);
  //
  SetLoadMsg('开始Lazarus组件库相关操作...');
  FInitialize.InitLCLOperate;
  //
  SetLoadMsg('开始初始化主题工具...');
  FInitialize.InitTheme;
  SetLoadMsg('开始设置默认主题...');
  FInitialize.ThemeUtil.SetFirstTheme;
  //
  {$IFDEF Windows}
  SetLoadMsg('开始记录按键信息...');
  //FInitialize.POSSystemObject.StartRecordKeyDown;
  {$ENDIF}

  //
  SetLoadMsg('开始数据库信息处理器...');
  FInitialize.InitDBMessageHandler;

  //bin 程序运行需要的依赖文件
  SetLoadMsg('开始加载基本服务...');
  loader := nil;
  if InterfaceRegister.OutInterface(ICMInterfaceLoader, loader) then
    begin
      //loader.LoadDirAll('bin');
      loader.LoadFile('bin/mpos_bss.dll');
      loader.LoadFile('bin/mpos_sale.dll');
    end;

  if InterfaceRegister.OutInterface(IPOSStatement, sta) then
    uDAO.TPOSDAOFactory.SetStatement(sta);

  //lib 程序功能的运行文件
  SetLoadMsg('开始加载业务...');
  if Assigned(loader) then
    loader.LoadDirAll('lib32');
  //


  //
  SetLoadMsg('加载完成,准备显示主操作界面...', 100);
  Sleep(300);
  LoadingForm.Close;
end;

procedure TPOSStart.AfterLogin;
begin
  Messager.Info('AfterLogin()...');

end;

procedure TPOSStart.SetWorkRect(ARect: TRect);
begin
  FInitialize.POSSystemObject.WorkRect := ARect;
end;


initialization
  POSStart := TPOSStart.Create(Application);

end.

