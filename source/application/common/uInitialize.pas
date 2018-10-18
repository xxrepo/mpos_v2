unit uInitialize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, DateTimePicker,
  cm_interfaces, cm_messager, cm_DOM, cm_XML, cm_dialogs, cm_logutils, cm_SimpleMessage,
  cm_parameter, cm_ParameterUtils,
  cm_theme, cm_ThemeUtils,
  cm_Plat, cm_PlatInitialize, cm_LCLUtils,
  uSystem;

type

  { TPOSInitialize }

  TPOSInitialize = class(TCMMessageableComponent, IAppSystem)
  private
    FLogger: TCMJointFileLogger;
    FMessageHandler: ICMMessageHandler;
    FParameter: ICMParameter;
    //
    FParameterLoader: ICMParameterLoader;
    FThemeUtil: TCMThemeUtil;
    FExceptMsgBox: TCMMsgBox;
    procedure HandleExceptionEvent(Sender: TObject; E: Exception);
  public
    constructor Create(AOwner: TComponent); override;
    property MessageHandler: ICMMessageHandler read FMessageHandler;

    //
    property ThemeUtil: TCMThemeUtil read FThemeUtil;
  public
    function InitDBMessageHandler: Boolean;
    function InitLCLOperate: Boolean;
    function InitParameter: Boolean;
    function InitTheme: Boolean;
  public //IAppSystem
    function GetVersion: string;
    function IsTestMode: Boolean;
    function GetStartTime: TDateTime;
    function GetLoginTime: TDateTime;
    function GetMsgBox: TCMMsgBox;
    function GetParameter: ICMParameter;
    function GetLog: ICMLog;
    function GetWorkRect: TRect;
  end;


implementation

uses LazFileUtils, uDialogs, uConstant;

function MessageBoxFunc(Text, Caption :PChar; Flags: Longint): Integer;
begin
  if AppSystem.GetMsgBox.Visible then
    AppSystem.GetMsgBox.Close;
  Result := AppSystem.GetMsgBox.MessageBox(Text, Caption, Flags)
end;

{ TPOSInitialize }

constructor TPOSInitialize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //1、日志
  FLogger := TCMJointFileLogger.Create(Application);
  FLogger.FilePath := LogPath;
  FLogger.FileNamePrefix := LogFileNamePrefix;
  FLogger.Info('============================================================');
  //2、默认信息处理
  FMessageHandler := TCMLogMessageHandler.Create(FLogger);
  cm_PlatInitialize.InitPlat(FMessageHandler);
  Self.Messager.AddMessageHandler(FMessageHandler);
  DefaultMessagerName := 'MPOS';
  //
  FParameterLoader := nil;
  FExceptMsgBox := nil;

  //------------------------------------------------------------------------------------------------
  //以下是其他构建所需的一些依赖
  //------------------------------------------------------------------------------------------------

  //1、主窗体紧接之后创建，需要IThemeable；IThemeable 需要集合 IThemeableSet。
  FThemeUtil := TCMThemeUtil.Create;
  TThemeableManager.GetInstance.AddThemeableSet(FThemeUtil);
  //2、AppSystem
  InterfaceRegister.PutInterface('IAppSystem', IAppSystem, Self);
end;

function TPOSInitialize.InitDBMessageHandler: Boolean;
begin
  Result := InterfaceRegister.PutInterface('数据库信息处理器', ICMMessageHandler, TCMLogMessageHandler.Create(FLogger), DBMessageHandlerCode) >= 0;
end;

function TPOSInitialize.InitParameter: Boolean;
var
  ns: TCMDOMNodeStreamer;
  node: TCMDOMNode;
  i: Integer;
  xmlConfigParameter: ICMParameter;
  fn: string;
  paramObj: TCMParameter;
  p: ICMParameter;
begin
  Result := False;
  Messager.Info('开始初始化参数工具...');
  FParameter := TCMParameter.Create(nil, 'root', '');
  FParameter := paramObj;
  FParameterLoader := paramObj.ParameterSet as ICMParameterLoader;
  InterfaceRegister.PutInterface('ICMParameterLoader', ICMParameterLoader, FParameterLoader);
  //
  ns := TCMDOMNodeStreamer.Create(nil);
  try
    Messager.Info('开始加载默认配置参数...');
    if not FileExistsUTF8(DefaultConfigFileName) then
      begin
        AppSystem.GetMsgBox.ShowMessage('默认配置文件:' + DefaultConfigFileName + '不存在.');
        Exit;
      end;
    if ns.ReadXML(node, DefaultConfigFileName) then
      begin
        FParameterLoader.LoadParameters(paramObj, node);
        node.Free;
      end;
    Messager.Info('开始加载配置的XML文件参数...');
    xmlConfigParameter := FParameter.Get(XMLConfigParameterName);
    if not xmlConfigParameter.IsNull then
      begin
        for i:=0 to xmlConfigParameter.ItemCount-1 do
          begin
            p := xmlConfigParameter.GetItem(i);
            fn := p.AsString;
            if not FileExistsUTF8(fn) then
              begin
                Messager.Error('配置文件:%s不存在.', [fn]);
                Continue;
              end;
            if ns.ReadXML(node, fn) then
              begin
                Messager.Debug('开始加载XML文件:%s...', [fn]);
                FParameterLoader.LoadParameters(paramObj, node);
                node.Free;
              end;
          end;
      end;
  finally
    ns.Free;
  end;
  //
  Result := True;
end;

//private
procedure TPOSInitialize.HandleExceptionEvent(Sender: TObject; E: Exception);
var
  es: string;
begin
  if not Assigned(FExceptMsgBox) then
    FExceptMsgBox := TPOSMsgBox.Create(Application);
  if FExceptMsgBox.Visible then
    FExceptMsgBox.Close;
  es := '系统错误:' + E.ClassName + #10#10'错误信息:' + E.Message;
  Messager.Error('FExceptMsgBox行将显示:' + es);
  FExceptMsgBox.ShowMessage(es);
end;

function TPOSInitialize.InitLCLOperate: Boolean;
var
  manager: TCMLCLGlobalManager;
  generator: TCMLCLGenerator;
begin
  Result := False;
  //
  Messager.Info('开始初始化LCL套件...');
  //cm_PlatInitialize.InitLCLSuite;
  //
  manager := cm_PlatInitialize.GetLCLGlobalManager;
  Messager.Debug('设置主LCL消息盒...');
  manager.GetMainLCLGlobalSet.SetMessageBoxFunction(@MessageBoxFunc);
  Messager.Debug('设置主错误处理事件...');
  manager.ApplicationExceptionEvent := @HandleExceptionEvent;
  //
  Messager.Info('开始注册组件:TDateTimePicker...');
  generator := cm_PlatInitialize.GetLCLGenerator;
  generator.RegisterClass('TDateTimePicker', TDateTimePicker);
  //
  Result := True;
end;

function TPOSInitialize.InitTheme: Boolean;
var
  ns: TCMDOMNodeStreamer;
  node: TCMDOMNode;
  i: Integer;
  themeName, themeTitle: string;
  theme: TCMTheme;
  themesParameter: ICMParameter;
begin
  Result := False;
  Messager.Info('开始初始化主题工具...');
  //
  themesParameter := AppSystem.GetParameter.AddString('themes', '');
  //
  if not FileExistsUTF8(ThemeConfigFileName) then
    begin
      Messager.Error('主题配置文件:%s不存在.', [ThemeConfigFileName]);
      Exit;
    end;
  ns := TCMDOMNodeStreamer.Create(nil);
  try
    if ns.ReadXML(node, ThemeConfigFileName) then
      begin
        //寄存主题控制器
        InterfaceRegister.PutInterface(IThemeableSet, IThemeableSet(FThemeUtil));
        InterfaceRegister.PutInterface(IThemeController, IThemeController(FThemeUtil));
        //
        for i:=0 to node.ChildCount-1 do
          begin
            if node.ChildNodes[i].Name = 'theme' then
              begin
                themeName := node.ChildNodes[i].GetAttribute('name');
                themeTitle := node.ChildNodes[i].GetAttribute('title');
                //加入配置参数
                FParameterLoader.LoadParameters(themesParameter, node.ChildNodes[i]);
                //构建主题
                theme := TCMTheme.Create(themeName, themeTitle, FParameter.Get('themes').Get(themeName));
                //加入控制器
                FThemeUtil.AddTheme(theme);
              end;
          end;
        Result := True;
      end;
  finally
    ns.Free;
  end;
end;

function TPOSInitialize.GetVersion: string;
begin
  Result := VersionStr;
end;

function TPOSInitialize.IsTestMode: Boolean;
begin
  Result := False;
  {$IFDEF Test}
  Result := True;
  {$ENDIF}
end;

function TPOSInitialize.GetStartTime: TDateTime;
begin

end;

function TPOSInitialize.GetLoginTime: TDateTime;
begin

end;

function TPOSInitialize.GetMsgBox: TCMMsgBox;
begin

end;

function TPOSInitialize.GetParameter: ICMParameter;
begin
  Result := FParameter;
end;

function TPOSInitialize.GetLog: ICMLog;
begin
  Result := FLogger;
end;

function TPOSInitialize.GetWorkRect: TRect;
begin

end;


end.

