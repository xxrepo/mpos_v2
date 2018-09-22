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
    procedure Init; //TODO 后继改进
  public
    constructor Create(AServletContext: IServletContext);
    procedure SetServlet(AServlet: IServlet);
    function GetServlet: IServlet;
    procedure AddURLPattern(const AURLPattern: string);
    function GetURLPatterns: TStrings;
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
    procedure Handle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse); virtual; abstract;
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
    procedure BeforeHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse; var CanHandle: Boolean); virtual;
    procedure DoHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse); virtual;
  public
    constructor Create;
    procedure Handle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse); override;
    procedure SetHandler(AHandler: IHandler); virtual;
    function GetHandler: IHandler;
    procedure InsertHandler(AWrapper: IHandlerWrapper);
  end;

  (***************************************** Server ***********************************************)

  { TServer }
  //TODO 多线程
  TServer = class(THandlerWrapper, IServer)
  private
    FConnectors: TConnectorList;
    FThreadPool: TExecuteThreadBool;
    FServletContext: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetHandler(AHandler: IHandler); override;
  protected
    procedure BeforeHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse; var CanHandle: Boolean); override;
  public //IServer
    procedure AddConnector(AConnector: IConnector);
    procedure RemoveConnector(AConnector: IConnector);
    function GetConnectors: TConnectorList;
    function GetThreadPool: TExecuteThreadBool;
    function GetServletContext(const AContextPath: string): IJettyServletContext;
  end;

  { TContextHandler }

  TContextHandler = class(THandlerWrapper, IContextHandler)
  private
    FJettyServletContext: TJettyServletContext;
  public
    constructor Create;
    destructor Destroy; override;
    property JettyServletContext: TJettyServletContext read FJettyServletContext;
  protected
    procedure BeforeHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse; var CanHandle: Boolean); override;
  public
    procedure SetContextPath(const APath: string);
    function GetContextPath: string;
    function GetInitParameters: ICMConstantParameterDataList;
    function GetAttributes: ICMParameterDataList;
    procedure SetServerInfo(const AServerInfo: string);
    function GetServerInfo: string;
  end;

  { TServletContextHandler }

  TServletContextHandler = class(TContextHandler, IServletContextHandler)
  public //ServletContextHandler
    procedure AddFilter(AFilter: IFilterHolder);
    procedure AddServlet(AHolder: IServletHolder);
    function GetServletContext: IJettyServletContext;
  end;


  { TServletHandler }

  TServletHandler = class(THandlerWrapper, IServletHandler)
  protected
    FJettyServletContext: IJettyServletContext;
  public
    constructor Create(AJettyServletContext: IJettyServletContext);
    destructor Destroy; override;
  protected
    procedure BeforeHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse; var CanHandle: Boolean); override;
    procedure DoHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse); override;
  public //IServletHandler
    procedure AddServlet(AServlet: IServletHolder);
    function GetServlet(const AName: string): IServletHolder;
    function GetServletContext: IServletContext;
    function GetServlets: TServletHolderList;
  end;




implementation

{$I cm_JettyBase.inc}

{ TServer }

constructor TServer.Create;
begin
  inherited Create;
  FConnectors := TConnectorList.Create;
  FThreadPool := TExecuteThreadBool.Create(nil);
  FServletContext := TInterfaceList.Create;
end;

destructor TServer.Destroy;
begin
  FConnectors.Free;
  FThreadPool.Free;
  FServletContext.Free;
  inherited Destroy;
end;

procedure TServer.SetHandler(AHandler: IHandler);
begin
  inherited SetHandler(AHandler);
  if Supports(AHandler, IServletContextHandler) then
    begin
      FServletContext.Add(IServletContextHandler(AHandler));
    end;
end;

//TODO 后继应实现 IHandlerCollection
//校验连接器
procedure TServer.BeforeHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse; var CanHandle: Boolean);
var
  i: Integer;
begin
  Messager.Debug('开始前置处理（检验连接器）...');
  CanHandle := False;
  for i:=0 to FConnectors.Count-1 do
    begin
      //TODO host操作
      if SameText(FConnectors[i].GetProtocol, ARequest.GetProtocol) and (FConnectors[i].GetPort = ARequest.GetPort) then
        begin
          CanHandle := True;
          Exit;
        end;
    end;
  Messager.Debug('没有匹配的连接器（%s）.', [ARequest.GetRequestURL]);
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

function TServer.GetServletContext(const AContextPath: string): IJettyServletContext;
var
  enumerator: TInterfaceListEnumerator;
  sc: IJettyServletContext;
begin
  Result := nil;
  enumerator := FServletContext.GetEnumerator;
  while (enumerator.MoveNext) do
    begin
      sc := IJettyServletContext(enumerator.GetCurrent);
      Messager.Debug('sc.GetContextPath:%s - AContextPath:%s', [sc.GetContextPath, AContextPath]);
      if sc.GetContextPath = AContextPath then
        Result := sc;
    end;
end;

{ TContextHandler }

constructor TContextHandler.Create;
begin
  inherited Create;
  FJettyServletContext := TJettyServletContext.Create(Self);
  FJettyServletContext.ServerInfo := Self.UnitName + '.' + Self.ClassName;
end;

destructor TContextHandler.Destroy;
begin
  FJettyServletContext.Free;
  inherited Destroy;
end;

//校验上下文路径
procedure TContextHandler.BeforeHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse; var CanHandle: Boolean);
begin
  Messager.Debug('开始前置处理（校验上下文路径）...');
  CanHandle := False;
  if Self.GetContextPath = ARequest.GetContextPath then
    begin
      CanHandle := True;
      Exit;
    end;
  Messager.Debug('上下文路径不匹配（%s）.', [ARequest.GetRequestURL]);
end;

procedure TContextHandler.SetContextPath(const APath: string);
begin
  FJettyServletContext.ContextPath := APath;
end;

function TContextHandler.GetContextPath: string;
begin
  Result := FJettyServletContext.GetContextPath;
end;

function TContextHandler.GetInitParameters: ICMConstantParameterDataList;
begin
  Result := FJettyServletContext.GetInitParameters;
end;

function TContextHandler.GetAttributes: ICMParameterDataList;
begin
  Result := FJettyServletContext.GetAttributes;
end;

procedure TContextHandler.SetServerInfo(const AServerInfo: string);
begin
  FJettyServletContext.ServerInfo := AServerInfo;
end;

function TContextHandler.GetServerInfo: string;
begin
  Result := FJettyServletContext.GetServerInfo;
end;

{ TServletContextHandler }

procedure TServletContextHandler.AddFilter(AFilter: IFilterHolder);
begin

end;

procedure TServletContextHandler.AddServlet(AHolder: IServletHolder);
begin
  FJettyServletContext.AddServlet(AHolder);
end;

function TServletContextHandler.GetServletContext: IJettyServletContext;
begin
  Result := FJettyServletContext;
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

procedure TServletHandler.BeforeHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse; var CanHandle: Boolean);
var
  sh: IServletHolder;
  servlet: IServlet;
begin
  Messager.Debug('开始前置处理（校验 servlet url-pattern）...');
  CanHandle := False;
  //
  sh := FJettyServletContext.GetServlets.GetByMatchingPath(ARequest.GetServletPath);
  if Assigned(sh) then
    begin
      servlet := sh.GetServlet;
      if Assigned(servlet) then
        begin
          ARequest.SetTargetServlet(servlet);
          CanHandle := True;
          Exit;
        end;
      Exit;
    end;
  Messager.Debug('上下文路径不匹配（%s）.', [ARequest.GetRequestURL]);
end;

procedure TServletHandler.DoHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse);
var
  servlet: IServlet;
begin
  Messager.Debug('DoHandle()... [RequestURL: %s]', [ARequest.GetRequestURL]);
  //
  servlet := ARequest.GetTargetServlet;
  if Assigned(servlet) then
    begin
      Messager.Debug('DoHandle() servlet.Service() begin...');
      servlet.Service(ARequest, AResponse);
      Messager.Debug('DoHandle() servlet.Service() end.');
    end
  else
    Messager.Debug('DoHandle() 没有目标 Servlet 不进行处理. [ServletPath: %s]', [ARequest.GetServletPath]);
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

