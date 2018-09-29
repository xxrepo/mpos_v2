unit uInitialize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, DateTimePicker,
  cm_messager, cm_DOM, cm_XML, cm_dialogs,
  cm_parameter, cm_ParameterUtils,
  cm_theme, cm_ThemeUtils,
  cm_LCLPlat, cm_LCLUtils,
  uMPOS,
  uSystem, uSystemUtils;

type

  { TPOSInitialize }

  TPOSInitialize = class(TCMMessageableComponent)
  private
    FPOSSystem: TPOSSystem;
    FParameterLoader: ICMParameterLoader;
    FThemeUtil: TCMThemeUtil;
    FExceptMsgBox: TCMMsgBox;
    procedure HandleExceptionEvent(Sender: TObject; E: Exception);
  public
    constructor Create(AOwner: TComponent); override;
    property ThemeUtil: TCMThemeUtil read FThemeUtil;
    property POSSystemObject: TPOSSystem read FPOSSystem;
  public
    function InitLCLOperate: Boolean;
    function InitParameter: Boolean;
    function InitTheme: Boolean;
  end;


implementation

uses uDialogs, LazFileUtils;

function MessageBoxFunc(Text, Caption :PChar; Flags: Longint): Integer;
begin
  if uMPOS.POSSystem.GetMsgBox.Visible then
    uMPOS.POSSystem.GetMsgBox.Close;
  Result := uMPOS.POSSystem.GetMsgBox.MessageBox(Text, Caption, Flags)
end;

{ TPOSInitialize }

constructor TPOSInitialize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPOSSystem := nil;
  FParameterLoader := nil;
  FExceptMsgBox := nil;

  (*** 是基本的初始化操作，如其他构建所需的一些依赖 ***)

  //1、IThemeable 需要集合: IThemeableSet
  FThemeUtil := TCMThemeUtil.Create;
  TThemeableManager.GetInstance.AddThemeableSet(FThemeUtil);
  //2、uMPOS的全局
  FPOSSystem := TPOSSystem.Create;
  uMPOS.POSSystem := FPOSSystem;
  uMPOS.InterfaceRegister := cm_LCLPlat.InterfaceRegister;
  uMPOS.InterfaceRegister.PutInterface('IPOSystem', IPOSSystem, FPOSSystem);
end;

function TPOSInitialize.InitParameter: Boolean;
var
  ns: TCMDOMNodeStreamer;
  node: TCMDOMNode;
  i: Integer;
  xmlConfigParameter: ICMParameter;
  fn: string;
  paramObj: TCMParameter;
begin
  Result := False;
  Messager.Info('开始初始化参数工具...');
  paramObj := TCMParameter.Create(nil, 'root', '');
  FPOSSystem.Parameter := paramObj;
  FParameterLoader := paramObj.ParameterSet as ICMParameterLoader;
  InterfaceRegister.PutInterface('ICMParameterLoader', ICMParameterLoader, FParameterLoader);
  //
  ns := TCMDOMNodeStreamer.Create(nil);
  try
    Messager.Info('开始加载默认配置参数...');
    if not FileExistsUTF8(DefaultConfigFileName) then
      begin
        POSSystem.GetMsgBox.ShowMessage('默认配置文件:' + DefaultConfigFileName + '不存在.');
        Exit;
      end;
    if ns.ReadXML(node, DefaultConfigFileName) then
      begin
        FParameterLoader.LoadParameters(paramObj, node);
        node.Free;
      end;
    Messager.Info('开始加载配置的XML文件参数...');
    xmlConfigParameter := FPOSSystem.GetParameter.Get(XMLConfigParameterName);
    if not xmlConfigParameter.IsNull then
      begin
        for i:=0 to xmlConfigParameter.ItemCount-1 do
          begin
            fn := xmlConfigParameter.GetItem(i).AsString;
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
  manager: TCMLCLManager;
  generator: TCMLCLGenerator;
begin
  Result := False;
  //
  Messager.Info('开始初始化LCL管理器...');
  manager := InitLCLManager;
  Messager.Debug('设置主LCL消息盒...');
  manager.GetMainLCLGlobalSet.SetMessageBoxFunction(@MessageBoxFunc);
  Messager.Debug('设置主错误处理事件...');
  manager.ApplicationExceptionEvent := @HandleExceptionEvent;
  //
  Messager.Info('开始初始化LCL组件构造工具...');
  generator := InitLCLGenerator;
  generator.RegisterClass('TDateTimePicker', TDateTimePicker);
  //
  Messager.Info('开始初始化LCL属性读写器...');
  InitLCLPropertyReaderWriter;
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
  themesParameter := POSSystem.GetParameter.AddString('themes', '');
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
                theme := TCMTheme.Create(themeName, themeTitle, FPOSSystem.Parameter.Get('themes').Get(themeName));
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


end.

