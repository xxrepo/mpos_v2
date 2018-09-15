{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_servlet

    This is not a complete unit, for testing

    //

    一个仿 jetty 的 servlet 容器的简单实现

 **********************************************************************}

unit cm_JettyImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  cm_messager, cm_parameter, cm_ParameterUtils, cm_threadutils, cm_netutils,
  cm_servlet, cm_servletutils,
  cm_jetty,
  cm_cmstp;

type

  { TLifeCycle }

  TLifeCycle = class(TCMMessageable, ILifeCycle)
  private
    FIsRunning: Boolean;
    FIsStopped: Boolean;
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
  public
    constructor Create;
    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;
    function IsStopped: Boolean;
  end;

  { TConnector }

  TConnector = class(TLifeCycle, IConnector)
  private
    FProtocol: string;
    FPort: Word;
  public
    constructor Create(const AProtocol: string; APort: Word=80);
    function GetProtocol: string;
    function GetPort: Word;
  end;

  (***************************************** Holder ***********************************************)

  { THolder }

  THolder = class(TLifeCycle, IHolder)
  private
    FName: string;
    FInitParameters: ICMParameterDataList;
  public
    constructor Create;
    property InitParameters: ICMParameterDataList read FInitParameters;
    procedure SetName(const AName: string);
    function GetName: string;
    function GetInitParameters: ICMConstantParameterDataList;
  end;

  { TFilterHolder }

  TFilterHolder = class(THolder, IFilterHolder)
  private
    FFilter: IFilter;
  public
    constructor Create;
    procedure SetFilter(AFilter: IFilter);
    function GetFilter: IFilter;
  end;

  { TListenerHolder }

  TListenerHolder = class(THolder, IListenerHolder)
  private
    FListener: IListener;
  public
    constructor Create;
    procedure SetListener(AListener: IListener);
    function GetListener: IListener;
  end;

  { TServletHolder }

  TServletHolder = class(THolder, IServletHolder)
  private
    FServletContext: IServletContext;
    FServlet: IServlet;
    FURLPatterns: TStrings;
    FInitialized: Boolean;
    FServletConfig: IServletConfig;
  public
    constructor Create(AServletContext: IServletContext);
    procedure SetServlet(AServlet: IServlet);
    function GetServlet: IServlet;
    procedure AddURLPattern(const AURLPattern: string);
    function GetURLPatterns: TStrings;
    procedure Init;
    function Initialized: Boolean;
    function GetServletConfig: IServletConfig;
  end;

  (***************************************** HandlerWrapper ***************************************)

  { THandler }

  THandler = class(TLifeCycle, IHandler)
  private
    FServer: IServer;
  public
    constructor Create;
    procedure SetServer(AServer: IServer);
    function GetServer: IServer;
    procedure Handle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse); virtual; abstract;
  end;

  { THandlerContainer }

  THandlerContainer = class(THandler, IHandlerContainer)
  protected
    FHandlers: THandlerList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetHandlers: THandlerList;
  end;

  { THandlerWrapper }

  THandlerWrapper = class(THandlerContainer, IHandlerWrapper)
  private
    FWrapper: IHandlerWrapper;
  protected
    procedure BeforeHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse; var CanHandle: Boolean); virtual;
    procedure DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse); virtual;
  public
    constructor Create;
    procedure Handle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse); override;
    procedure SetHandler(AHandler: IHandler);
    function GetHandler: IHandler;
    procedure InsertHandler(AWrapper: IHandlerWrapper);
  end;

  { TServletContextHandler }

  TServletContextHandler = class(THandlerWrapper, IContextHandler, IServletContextHandler)
  private
    FJettyServletContext: TJettyServletContext;
  public
    constructor Create;
    destructor Destroy; override;
    property JettyServletContext: TJettyServletContext read FJettyServletContext;
  public //ContextHandler
    procedure SetContextPath(const APath: string);
    function GetContextPath: string;
    function GetInitParameters: ICMConstantParameterDataList;
    function GetAttributes: ICMParameterDataList;
    procedure SetServerInfo(const AServerInfo: string);
  public //ServletContextHandler
    procedure AddFilter(AFilter: IFilterHolder);
    procedure AddServlet(AHolder: IServletHolder);
  end;


  { TServletHandler }

  TServletHandler = class(THandlerWrapper, IServletHandler)
  protected
    FJettyServletContext: IJettyServletContext;
  public
    constructor Create(AJettyServletContext: IJettyServletContext);
    destructor Destroy; override;
    procedure DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse); override;
  public //IServletHandler
    procedure AddServlet(AServlet: IServletHolder);
    function GetServlet(const AName: string): IServletHolder;
    function GetServletContext: IServletContext;
    function GetServlets: TServletHolderList;
  end;

  (***************************************** Server ***********************************************)

  { TServer }
  //TODO 多线程
  TServer = class(THandlerWrapper, IServer)
  private
    FConnectors: TConnectorList;
    FThreadPool: TExecuteThreadBool;
  public
    constructor Create;
    destructor Destroy; override;
  protected
    procedure BeforeHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse; var CanHandle: Boolean); override;
  public //IServer
    procedure AddConnector(AConnector: IConnector);
    procedure RemoveConnector(AConnector: IConnector);
    function GetConnectors: TConnectorList;
    function GetThreadPool: TExecuteThreadBool;
  end;


implementation

{$I cm_JettyHolder.inc}



{ THandlerWrapper }

constructor THandlerWrapper.Create;
begin
  inherited Create;
  FWrapper := nil;
end;

procedure THandlerWrapper.BeforeHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse; var CanHandle: Boolean);
begin
  //
end;

procedure THandlerWrapper.DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
begin
  //
end;


//有 Wrapper 时交由其处理，否则经前置处理后有子处理器时交由处理器处理，无子处理器时调用 DoHandle()
procedure THandlerWrapper.Handle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
var
  h: IHandler;
  canHandle: Boolean;
begin
  Messager.Debug('Handle()...');
  if not Self.IsRunning then
    begin
      Messager.Info('Handle() There is no start handle.');
      Exit;
    end;
  if Assigned(FWrapper) then
    begin
      if Supports(FWrapper, IHandler, h) then
        h.Handle(ATarget, ARequest, AResponse)
      else
        begin
          //for i:=0 to FWrapper.GetHandlers.Count-1 do
          //  FWrapper.GetHandlers[i].Handle(ATarget, ARequest, AResponse);
          //暂不考虑 Handler Collection 的情况
          h := FWrapper.GetHandler;
          if Assigned(h) then
            h.Handle(ATarget, ARequest, AResponse);
        end;
    end
  else
    begin
      canHandle := True;
      BeforeHandle(ATarget, ARequest, AResponse, canHandle);
      if CanHandle then
        begin
          h := GetHandler;
          if Assigned(h) then
            h.Handle(ATarget, ARequest, AResponse)
          else
            DoHandle(ATarget, ARequest, AResponse);
        end;
    end;
end;

procedure THandlerWrapper.SetHandler(AHandler: IHandler);
begin
  FHandlers.Clear;
  FHandlers.Add(AHandler);
end;

function THandlerWrapper.GetHandler: IHandler;
begin
  Result := nil;
  if FHandlers.Count > 0 then
    Result := FHandlers[0];
end;

procedure THandlerWrapper.InsertHandler(AWrapper: IHandlerWrapper);
begin
  FWrapper := AWrapper;
end;

{ TServletContextHandler }

constructor TServletContextHandler.Create;
begin
  inherited Create;
  FJettyServletContext := TJettyServletContext.Create(Self);
  FJettyServletContext.ServerInfo := Self.UnitName + '.' + Self.ClassName;
end;

destructor TServletContextHandler.Destroy;
begin
  FJettyServletContext.Free;
  inherited Destroy;
end;

procedure TServletContextHandler.SetContextPath(const APath: string);
begin
  FJettyServletContext.ContextPath := APath;
end;

function TServletContextHandler.GetContextPath: string;
begin
  Result := FJettyServletContext.GetContextPath;
end;

function TServletContextHandler.GetInitParameters: ICMConstantParameterDataList;
begin
  Result := FJettyServletContext.GetInitParameters;
end;

function TServletContextHandler.GetAttributes: ICMParameterDataList;
begin
  Result := FJettyServletContext.GetAttributes;
end;

procedure TServletContextHandler.SetServerInfo(const AServerInfo: string);
begin
  FJettyServletContext.ServerInfo := AServerInfo;
end;

procedure TServletContextHandler.AddFilter(AFilter: IFilterHolder);
begin

end;

procedure TServletContextHandler.AddServlet(AHolder: IServletHolder);
begin
  FJettyServletContext.AddServlet(AHolder);
end;

{ TServer }

constructor TServer.Create;
begin
  inherited Create;
  FConnectors := TConnectorList.Create;
  FThreadPool := TExecuteThreadBool.Create(nil);
end;

destructor TServer.Destroy;
begin
  FConnectors.Free;
  FThreadPool.Free;
  inherited Destroy;
end;

//TODO 后继应实现 IHandlerCollection
procedure TServer.BeforeHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse; var CanHandle: Boolean);
var
  FURL: TCMURL;
  i: Integer;
  sch: IServletContextHandler;
begin
  CanHandle := False;
  Messager.Debug('开始前置处理（检验连接器）...');
  FURL := TCMURL.Create(ATarget);
  try
    for i:=0 to FConnectors.Count-1 do
      begin
        //protocol、port相等时进行下一步。
        //TODO host操作
        if SameText(FConnectors[i].GetProtocol, FURL.Protocol) and (IntToStr(FConnectors[i].GetPort) = FURL.Port) then
          begin
            //校验 context path
            if Supports(Self.GetHandler, IServletContextHandler, sch) then
              if sch.GetContextPath = FURL.ContextPath then
                begin
                  CanHandle := True;
                  Exit;
                end;
          end;
      end;
  finally
    FURL.Free;
  end;
end;

procedure TServer.AddConnector(AConnector: IConnector);
begin
  FConnectors.Add(AConnector);
end;

procedure TServer.RemoveConnector(AConnector: IConnector);
begin
  FConnectors.Remove(AConnector);
end;

function TServer.GetConnectors: TConnectorList;
begin
  Result := FConnectors;
end;

function TServer.GetThreadPool: TExecuteThreadBool;
begin
  Result := FThreadPool;
end;

{ TServletHandler }

constructor TServletHandler.Create(AJettyServletContext: IJettyServletContext);
begin
  inherited Create;
  FJettyServletContext := AJettyServletContext;
end;

destructor TServletHandler.Destroy;
begin
  FJettyServletContext := nil;
  inherited Destroy;
end;

procedure TServletHandler.DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
var
  FURL: TCMURL;
  i: Integer;
  servlet: IServlet;
  sthdList: TServletHolderList;
begin
  FURL := TCMURL.Create(ATarget);
  Messager.Debug('ContextPath:%s URL.path:%s', [FJettyServletContext.GetContextPath, FURL.Path]);
  //上下文路径在前置服务器处理器中已匹对，这里只需对 servlet 的路径作匹对。
  sthdList := FJettyServletContext.GetServlets;
  for i:=0 to sthdList.Count-1 do
    begin
      Messager.Debug('ThisHandle() check servlet: %s...', [sthdList[i].GetName]);
      if sthdList[i].GetURLPatterns.IndexOf(FURL.LetPath) >= 0 then
        begin
          Messager.Debug('--2----------------');
          if not sthdList[i].Initialized then
            sthdList[i].Init;
          servlet := sthdList[i].GetServlet;
          if Assigned(servlet) then
            servlet.Service(ARequest, AResponse);
        end;
    end;
end;

procedure TServletHandler.AddServlet(AServlet: IServletHolder);
begin
  FJettyServletContext.AddServlet(AServlet);
end;

function TServletHandler.GetServlet(const AName: string): IServletHolder;
begin
  Result := FJettyServletContext.GetServlet(AName);
end;

function TServletHandler.GetServletContext: IServletContext;
begin
  Result := FJettyServletContext;
end;

function TServletHandler.GetServlets: TServletHolderList;
begin
  Result := FJettyServletContext.GetServlets;
end;



end.

