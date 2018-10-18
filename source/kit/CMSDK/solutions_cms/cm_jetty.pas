{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_servlet

    This is not a complete unit, for testing

    //

    仿 jetty 的简单声明。

 **********************************************************************}

unit cm_jetty;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults, Dialogs,
  cm_sysutils, cm_interfaces, cm_messager, cm_generics, cm_parameter, cm_threadutils,
  cm_servlet, cm_servletutils;

type

  { ILifeCycle
    // The lifecycle interface for generic components.
    Classes implementing this interface have a defined life cycle defined by the methods of this interface.
  }
  ILifeCycle = interface(ICMBase)
    ['{6F11D00F-825E-4DA2-9F85-8DDF9918EF2C}']
    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;
    function IsStopped: Boolean;
  end;

  IConnector = interface(ILifeCycle)
    ['{A2BB56BE-3154-4B64-AAF2-9DE89E46E111}']
    function GetProtocol: string;
    function GetPort: Word;
  end;

  TConnectorList = TList<IConnector>;

  (************************* Holder ***************************************************************)

  IHolder = interface(ILifeCycle)
    ['{7FCDA1DD-FF50-45FD-86B6-D9CB21378785}']
    procedure SetName(const AName: string);
    function GetName: string;
    function GetInitParameters: ICMConstantParameterDataList;
    function IsAsyncSupported: Boolean;
    procedure SetAsyncSupported(Suspendable: Boolean);
  end;

  IFilterHolder = interface(IHolder)
    ['{1178C633-64AF-40B6-AA1F-64D6F4365BDF}']
    procedure SetFilter(AFilter: IFilter);
    function GetFilter: IFilter;
    //
    procedure AddURLPattern(const AURLPattern: string);
    function GetURLPatterns: TStrings;
  end;

  { TFilterHolderList }

  TFilterHolderList = class(TCMHashInterfaceList<IFilterHolder>)
  public
    function GetByMatchingPath(const AServletPath: string): IFilterHolder;
  end;

  IListenerHolder = interface(IHolder)
    ['{465B175A-9B87-4551-9675-EDC08B1C2352}']
    procedure SetListener(AListener: IListener);
    function GetListener: IListener;
  end;

  TListenerHolderList = TCMHashInterfaceList<IListenerHolder>;

  IServletHolder = interface(IHolder)
    ['{2011D13B-71A3-4B41-8EB5-3EA454F213DD}']
    procedure SetServlet(AServlet: IServlet);
    function GetServlet: IServlet;
    //为方便实现容器，下述不再一味模仿 jetty 。
    procedure AddURLPattern(const AURLPattern: string);
    function GetURLPatterns: TStrings;
    function Initialized: Boolean;
    function GetServletConfig: IServletConfig;
  end;

  { TServletHolderList
    // 一个 ServletHolder 集合，实现对 servlet 路径的匹配。
  }
  TServletHolderList = class(TCMHashInterfaceList<IServletHolder>)
  public
    function IsMatchingPath(const AServletPath: string): Boolean; //为方便实现 RequestDispatcher
    function GetByMatchingPath(const AServletPath: string): IServletHolder;
  end;

  (************************* Handler **************************************************************)

  IJettyServletRequest = interface;
  IJettyServletResponse = interface;
  IJettyServletContext = interface;
  IServer = interface;

  { IHandler
    // A Jetty Server Handler.
    // A Handler instance is required by a Server to handle incoming requests.
    // A Handler may:
      1.Completely generate the Response
      2.Examine/modify the request and call another Handler (see HandlerWrapper).
      3.Pass the request to one or more other Handlers (see HandlerCollection).
      4.Handlers are passed the servlet API request and response object, but are not Servlets.
    The servlet container is implemented by handlers for context, security, session and servlet that
    modify the request objectbefore passing it to the next stage of handling.
  }
  IHandler = interface(ILifeCycle)
    ['{A3994D45-7A79-4A41-89FC-95202BDF6256}']
    procedure SetServer(AServer: IServer);
    function GetServer: IServer;
    procedure Handle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse);
  end;

  THandlerList = TList<IHandler>;

  IHandlerContainer = interface(IHandler)
    ['{91ED7C60-217D-4F84-ADBD-8F58C00E501B}']
    function GetHandlers: THandlerList;
  end;

  { IHandlerWrapper
    // A HandlerWrapper acts as a Handler but delegates the handle method and life cycle events to a delegate.
    // This is primarily used to implement the Decorator pattern.
  }
  IHandlerWrapper = interface(IHandlerContainer)
    ['{91ED7C60-217D-4F84-ADBD-8F58C00E501B}']
    procedure SetHandler(AHandler: IHandler);
    function GetHandler: IHandler;
    procedure InsertHandler(AWrapper: IHandlerWrapper); //Replace the current handler with another HandlerWrapper linked to the current handler.
  end;

  IHandlerCollection = interface(IHandlerContainer)
    ['{EF50BBE1-1ABF-4C79-83F5-7888D945A323}']
    procedure AddHandler(AHandler: IHandler);
    procedure RemoveHandler(AHandler: IHandler);
  end;

  IContextHandler = interface(IHandlerContainer)
    ['{03B536EF-8834-4389-9A00-BD9DFB5952EE}']
    procedure SetContextPath(const APath: string);
    function GetContextPath: string;
    function GetInitParameters: ICMConstantParameterDataList;
    function GetAttributes: ICMParameterDataList;
    procedure SetServerInfo(const AServerInfo: string);
    function GetServerInfo: string;
  end;

  IServletContextHandler = interface(IContextHandler)
    ['{EEAB8590-7C93-4727-9F8D-438454DD4805}']
    //procedure AddFilter(AFilter: IFilterHolder);
    procedure AddServlet(AHolder: IServletHolder); overload;
    function AddServlet(const AName: string; AServlet: IServlet): IServletHolder; overload;
    function GetServletContext: IJettyServletContext;
  end;

  IServletHandler = interface(IHandlerContainer)
    ['{2F2719A8-316E-4B04-8FB5-D0945A68A0AC}']
    //procedure AddFilter(AFilter: IFilterHolder);
    //procedure AddListener(AListener: IListenerHolder);
    procedure AddServlet(AServlet: IServletHolder);
    //function GetFilter(const AName: string): IFilterHolder;
    //function GetFilters: TFilterHolderList;
    //function GetListeners: TListenerHolderList;
    function GetServlet(const AName: string): IServletHolder;
    function GetServlets: TServletHolderList;
    function GetServletContext: IServletContext;
  end;

  (*-----------------------------------------------------------------------------------------------
     以下：为实现的拓展定义，与基本类型
   -----------------------------------------------------------------------------------------------*)

  { IJettyServletContext
    // 拓展的 ServletContext
    // 由上下文记录，以便处理器从中提取内容处理。
  }
  IJettyServletContext = interface(IServletContext)
    ['{0D7D47E8-05EF-4657-8E4B-ED8FB97B5931}']
    procedure AddServlet(AHolder: IServletHolder);
    function GetServlet(const AName: string): IServletHolder;
    function GetServlets: TServletHolderList;
    //
    procedure AddFilter(AHolder: IFilterHolder);
    function GetFilters: TFilterHolderList;
  end;

  TJettyServletContextList = TCMInterfaceList<IJettyServletContext>;

  TJettyServletContextEnumerator = TCMInterfaceListEnumerator<IJettyServletContext>;

  //--------- servlet 适应性扩展定义 ---------------------------------------------------------------

  { IJettyServletRequest
    // 应对 RequestDispatcher，声明皆内部使用
  }
  IJettyServletRequest = interface(IServletRequest)
    ['{F14EA8C7-B431-43FB-88D3-87D6BE44DC87}']
    procedure SetTargetServlet(AServlet: IServlet);
    function GetTargetServlet: IServlet;
    procedure SetDirectMode(AValue: Boolean);
    function IsDirectMode: Boolean;
    function GetServer: IServer;
  end;

  { IJettyServletResponse
    // 如果使用forward跳转则其后面的response输出则不会执行，
    而用include来跳转，则include的servlet执行完后，再返回到原来的servlet执行response的输出
  }
  IJettyServletResponse = interface(IServletResponse)
    ['{6AD78E14-BEE2-42D2-833C-F927E1D00B84}']
    procedure SetForwarded;
    function IsForwarded: Boolean;
    procedure SetInclued;
    function IsInclued: Boolean;
  end;

  IServer = interface(IHandlerContainer)
    ['{F5085010-2151-463B-82BD-3126B9549F19}']
    procedure AddConnector(AConnector: IConnector);
    procedure RemoveConnector(AConnector: IConnector);
    function GetConnectors: TConnectorList;
    function GetThreadPool: TExecuteThreadBool;
    //应对 RequestDispatcher
    function GetServletContext(const AContextPath: string): IJettyServletContext;
  end;

  { TJettyServletRequest
    // 拓展的 ServletRequest
  }
  TJettyServletRequest = class(TServletRequest, IJettyServletRequest)
  private
    FServer: IServer;
    FServlet: IServlet;
    FDirectMode: Boolean;
  public
    constructor Create(const AURL: string; AServer: IServer);
    property Parameters: ICMParameterDataList read FParameters write FParameters;
  public
    function GetRequestDispatcher(const APath: string): IRequestDispatcher; override;
  public
    procedure SetTargetServlet(AServlet: IServlet);
    function GetTargetServlet: IServlet;
    procedure SetDirectMode(AValue: Boolean);
    function IsDirectMode: Boolean;
    function GetServer: IServer;
  end;

  { TJettyServletResponse }

  TJettyServletResponse = class(TServletResponse, IJettyServletResponse)
  private
    FForwarded, FInclued: Boolean;
  public
    constructor Create;
    procedure SetForwarded;
    function IsForwarded: Boolean;
    procedure SetInclued;
    function IsInclued: Boolean;
  end;

  { TJettyServletContext }

  TJettyServletContext = class(TServletContext, IJettyServletContext)
  private
    FHandler: IHandler;
    FServletHolders: TServletHolderList;
    FFilterHolders: TFilterHolderList;
  public
    constructor Create(AHandler: IHandler);
    destructor Destroy; override;
  public //IJettyServletContext
    procedure AddServlet(AHolder: IServletHolder);
    function GetServlet(const AName: string): IServletHolder;
    function GetServlets: TServletHolderList;
    procedure AddFilter(AHolder: IFilterHolder);
    function GetFilters: TFilterHolderList;
  public
    function GetNamedDispatcher(const AName: string): IRequestDispatcher; override;
    function GetRequestDispatcher(const APath: string): IRequestDispatcher; override;
  end;

  { TJettyRequestDispatcher
    // 实现思路：通过获取相应 IHandler 后创建 RequestDispatcher 否则应返回 nil。
  }
  TJettyRequestDispatcher = class(TCMMessageable, IRequestDispatcher)
  private
    FServlet: IServletHolder;
    FHandler: IHandler;
  public
    constructor Create(AServlet: IServletHolder; AHandler: IHandler);
    destructor Destroy; override;
  public
    procedure Forward(ARequest: IServletRequest; AResponse: IServletResponse);
    procedure Include(ARequest: IServletRequest; AResponse: IServletResponse);
  end;

  { TJettyFilterChain }

  TJettyFilterChain = class(TCMMessageable, IFilterChain)
  private

  public
    procedure DoFilter(ARequest: IServletRequest; AResponse: IServletResponse);
  end;

implementation

{ TJettyServletContext }

constructor TJettyServletContext.Create(AHandler: IHandler);
begin
  FHandler := AHandler;
  FServletHolders := TServletHolderList.Create;
  FFilterHolders := TFilterHolderList.Create;
end;

destructor TJettyServletContext.Destroy;
begin
  FHandler := nil;
  FServletHolders.Free;
  FFilterHolders.Free;
  inherited Destroy;
end;

procedure TJettyServletContext.AddServlet(AHolder: IServletHolder);
begin
  FServletHolders.Add(AHolder.GetName, AHolder);
end;

function TJettyServletContext.GetServlet(const AName: string): IServletHolder;
begin
  Result := FServletHolders.Find(AName);
end;

function TJettyServletContext.GetServlets: TServletHolderList;
begin
  Result := FServletHolders;
end;

procedure TJettyServletContext.AddFilter(AHolder: IFilterHolder);
begin
  FFilterHolders.Add(AHolder.GetName, AHolder);
end;

function TJettyServletContext.GetFilters: TFilterHolderList;
begin
  Result := FFilterHolders;
end;

function TJettyServletContext.GetNamedDispatcher(const AName: string): IRequestDispatcher;
var
  i: Integer;
  sh: IServletHolder;
begin
  Result := nil;
  Messager.Debug('GetNamedDispatcher(%s)...', [AName]);
  for i:=0 to FServletHolders.Count-1 do
    begin
      sh := FServletHolders[i];
      if sh.GetServlet.GetServletConfig.GetServletName = AName then
        begin
          Result := TJettyRequestDispatcher.Create(sh, FHandler);
          Messager.Debug('GetNamedDispatcher(%s) OK.', [AName]);
          Exit;
        end;
    end;
end;

function TJettyServletContext.GetRequestDispatcher(const APath: string): IRequestDispatcher;
var
  path, contextPath, servletPath: string;
  servletContext: IJettyServletContext;
  servletHolder: IServletHolder;
begin
  Result := nil;
  Messager.Debug('GetRequestDispatcher(%s)...', [APath]);
  if APath[1] = '/' then
    begin
      path := Copy(APath, 2, 1024);
      contextPath := LCutStr(path, '/');
      servletPath := '/' + path;
    end
  else
    begin
      contextPath := Self.GetContextPath;
      servletPath := '/' + APath;
    end;
  servletContext := FHandler.GetServer.GetServletContext(contextPath);
  if Assigned(servletContext) then
    begin
      servletHolder := servletContext.GetServlets.GetByMatchingPath(servletPath);
      if Assigned(servletHolder) then
        begin
          Result := TJettyRequestDispatcher.Create(servletHolder, FHandler);
          Messager.Debug('GetRequestDispatcher(%s) OK.', [APath]);
        end;
    end;
end;

{ TJettyRequestDispatcher }

constructor TJettyRequestDispatcher.Create(AServlet: IServletHolder; AHandler: IHandler);
begin
  inherited Create;
  FServlet := AServlet;
  FHandler := AHandler;
end;

destructor TJettyRequestDispatcher.Destroy;
begin
  FHandler := nil;
  inherited Destroy;
end;

procedure TJettyRequestDispatcher.Forward(ARequest: IServletRequest; AResponse: IServletResponse);
var
  req: IJettyServletRequest;
  rps: IJettyServletResponse;
begin
  AResponse.GetContent.Clear;
  if Supports(ARequest, IJettyServletRequest, req) and Supports(AResponse, IJettyServletResponse, rps) then
    begin
      req.SetDirectMode(True);
      try
        req.SetTargetServlet(FServlet.GetServlet);
        FHandler.Handle(req, rps);
        rps.SetForwarded;
      finally
        req.SetDirectMode(False);
      end;
    end;
end;

procedure TJettyRequestDispatcher.Include(ARequest: IServletRequest; AResponse: IServletResponse);
var
  req: IJettyServletRequest;
  rps, inclueRps: IJettyServletResponse;
  i: Integer;
  n: string;
  p: ICMParameterData;
begin
  inclueRps := TJettyServletResponse.Create;
  if Supports(ARequest, IJettyServletRequest, req) and Supports(AResponse, IJettyServletResponse, rps) then
    begin
      req.SetDirectMode(True);
      try
        //---------------------------------------------------
        FHandler.Handle(req, inclueRps);
        for i:=0 to inclueRps.GetContent.Count-1 do
          begin
            n := inclueRps.GetContent.GetName(i);
            p := inclueRps.GetContent.Get(i);
            if (n <> '') and (not p.IsNull) then
              rps.GetContent.SetData(n, p);
          end;
        rps.SetInclued;
        //---------------------------------------------------
      finally
        req.SetDirectMode(False);
      end;
    end;
end;

{ TJettyServletRequest }

constructor TJettyServletRequest.Create(const AURL: string; AServer: IServer);
begin
  inherited Create(AURL);
  FServer := AServer;
  FServlet := nil;
  FDirectMode := False;
end;

//TODO 与 ServletContext.GetRequestDispatcher() 重复代码
function TJettyServletRequest.GetRequestDispatcher(const APath: string): IRequestDispatcher;
var
  path, contextPath, servletPath: string;
  servletContext: IJettyServletContext;
  servletHolder: IServletHolder;
begin
  Result := nil;
  TCMMessageManager.GetInstance.GetMessager(Self).Debug('GetRequestDispatcher(%s)...', [APath]);
  if APath[1] = '/' then
    begin
      path := Copy(APath, 2, 1024);
      contextPath := LCutStr(path, '/');
      servletPath := '/' + path;
    end
  else
    begin
      contextPath := Self.GetContextPath;
      servletPath := '/' + APath;
    end;
  servletContext := FServer.GetServletContext(contextPath);
  if Assigned(servletContext) then
    begin
      servletHolder := servletContext.GetServlets.GetByMatchingPath(servletPath);
      if Assigned(servletHolder) then
        begin
          Result := TJettyRequestDispatcher.Create(servletHolder, FServer);
          TCMMessageManager.GetInstance.GetMessager(Self).Debug('GetRequestDispatcher(%s) OK.', [APath]);
        end;
    end;
end;

procedure TJettyServletRequest.SetTargetServlet(AServlet: IServlet);
begin
  FServlet := AServlet;
end;

function TJettyServletRequest.GetTargetServlet: IServlet;
begin
  Result := FServlet;
end;

procedure TJettyServletRequest.SetDirectMode(AValue: Boolean);
begin
  FDirectMode := AValue;
end;

function TJettyServletRequest.IsDirectMode: Boolean;
begin
  Result := FDirectMode;
end;

function TJettyServletRequest.GetServer: IServer;
begin
  Result := FServer;
end;

{ TJettyServletResponse }

constructor TJettyServletResponse.Create;
begin
  inherited Create;
  FForwarded := False;
  FInclued := False;
end;

procedure TJettyServletResponse.SetForwarded;
begin
  FForwarded := True;
end;

function TJettyServletResponse.IsForwarded: Boolean;
begin
  Result := FForwarded;
end;

procedure TJettyServletResponse.SetInclued;
begin
  FInclued := True;
end;

function TJettyServletResponse.IsInclued: Boolean;
begin
  Result := FInclued;
end;

{ TServletHolderList }

//TODO
//servlet 路径匹配规则：
//1.精确匹配
//2.路径匹配，先最长路径匹配，再最短路径匹配
//3.扩展名匹配
//4.缺省匹配
function TServletHolderList.IsMatchingPath(const AServletPath: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=0 to Self.Count-1 do
    if Self[i].GetURLPatterns.IndexOf(AServletPath) >= 0 then
      begin
        Result := True;
        Exit;
      end;
end;

function TServletHolderList.GetByMatchingPath(const AServletPath: string): IServletHolder;
var
  i: Integer;
begin
  Result := nil;
  for i:=0 to Self.Count-1 do
    if Self[i].GetURLPatterns.IndexOf(AServletPath) >= 0 then
      begin
        Result := Self[i];
        Exit;
      end;
end;

{ TJettyFilterChain }

procedure TJettyFilterChain.DoFilter(ARequest: IServletRequest; AResponse: IServletResponse);
begin

end;

{ TFilterHolderList }

function TFilterHolderList.GetByMatchingPath(const AServletPath: string): IFilterHolder;
var
  i: Integer;
begin
  Result := nil;
  for i:=0 to Self.Count-1 do
    if Self[i].GetURLPatterns.IndexOf(AServletPath) >= 0 then
      begin
        Result := Self[i];
        Exit;
      end;
end;

end.

