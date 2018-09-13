f{
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
  Classes, SysUtils, StrUtils, Dialogs,
  cm_messager, cm_parameter, cm_ParameterUtils, cm_threadutils, cm_netutils,
  cm_servlet, cm_servletutils,
  cm_jetty;

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
    FURLPatterns: TStrings;
    FServlet: IServlet;
  public
    constructor Create;
    procedure AddURLPattern(const AURLPattern: string);
    function GetURLPatterns: TStrings;
    procedure SetServlet(AServlet: IServlet);
    function GetServlet: IServlet;
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
    procedure DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse); virtual;
    procedure ThisHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse); virtual;
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
    FServletContext: TServletContext;
    FFilterHolders: TFilterHolderList;
    FServletHolders: TServletHolderList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse); override;
    procedure ThisHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse); override;
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
    FServletContext: IServletContext;
  public
    constructor Create(AServletContext: IServletContext);
    destructor Destroy; override;
  public //IServletHandler
    procedure AddFilter(AFilter: TFilterHolder);
    procedure AddListener(AListener: TListenerHolder);
    procedure AddServlet(AHolder: TServletHolder);
    function GetFilter(const AName: string): TFilterHolder;
    function GetFilters: TFilterHolderArray;
    function GetListeners: TListenerHolderArray;
    function GetServlet(const AName: string): TServletHolder;
    function GetServletContext: IServletContext;
    function GetServlets: TServletHolderArray;
  end;

  (***************************************** Server ***********************************************)

  { TServer }

  TServer = class(THandlerWrapper, IServer)
  private
    FConnectors: TConnectorList;
    FThreadPool: TExecuteThreadBool;
  protected
    //procedure DoStart; override;
    //procedure DoStop; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Handle(ARequest: IServletRequest; AResponse: IServletResponse); overload;
  protected
    procedure DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse); override;
  public
    procedure AddConnector(AConnector: IConnector);
    procedure RemoveConnector(AConnector: IConnector);
    function GetConnectors: TConnectorList;
    function GetThreadPool: TExecuteThreadBool;
  end;


implementation

{ TLifeCycle }

procedure TLifeCycle.DoStart;
begin
  //
end;

procedure TLifeCycle.DoStop;
begin
  //
end;

constructor TLifeCycle.Create;
begin
  inherited Create;
  FIsRunning := False;
  FIsStopped := False;
end;

procedure TLifeCycle.Start;
begin
  if not FIsRunning then
    begin
      DoStart;
      FIsRunning := True;
      FIsStopped := False;
    end;
end;

procedure TLifeCycle.Stop;
begin
  if not FIsStopped then
    begin
      DoStop;
      FIsRunning := False;
      FIsStopped := True;
    end;
end;

function TLifeCycle.IsRunning: Boolean;
begin
  Result := FIsRunning;
end;

function TLifeCycle.IsStopped: Boolean;
begin
  Result := FIsStopped;
end;

{ TConnector }

constructor TConnector.Create(const AProtocol: string; APort: Word);
begin
  inherited Create;
  FProtocol := AProtocol;
  FPort := APort;
end;

function TConnector.GetProtocol: string;
begin
  Result := FProtocol;
end;

function TConnector.GetPort: Word;
begin
  Result := FPort;
end;

{ THolder }

constructor THolder.Create;
begin
  inherited Create;
  FName := '';
  FInitParameters := TCMParameterDataList.Create;
end;

procedure THolder.SetName(const AName: string);
begin
  FName := AName;
end;

function THolder.GetName: string;
begin
  Result := FName;
end;

function THolder.GetInitParameters: ICMConstantParameterDataList;
begin
  Result := FInitParameters;
end;

{ TFilterHolder }

constructor TFilterHolder.Create;
begin
  inherited Create;
  FFilter := nil;
end;

procedure TFilterHolder.SetFilter(AFilter: IFilter);
begin
  FFilter := AFilter;
end;

function TFilterHolder.GetFilter: IFilter;
begin
  Result := FFilter;
end;

{ TListenerHolder }

constructor TListenerHolder.Create;
begin
  inherited Create;
  FListener := nil;
end;

procedure TListenerHolder.SetListener(AListener: IListener);
begin
  FListener := AListener;
end;

function TListenerHolder.GetListener: IListener;
begin
  Result := FListener;
end;

{ TServletHolder }

constructor TServletHolder.Create;
begin
  inherited Create;
  FURLPatterns := TStringList.Create;
  FServlet := nil;
end;

procedure TServletHolder.AddURLPattern(const AURLPattern: string);
begin
  FURLPatterns.Add(AURLPattern);
end;

function TServletHolder.GetURLPatterns: TStrings;
begin
  Result := FURLPatterns;
end;

procedure TServletHolder.SetServlet(AServlet: IServlet);
begin
  FServlet := AServlet;
end;

function TServletHolder.GetServlet: IServlet;
begin
  Result := FServlet;
end;

{ THandler }

constructor THandler.Create;
begin
  inherited Create;
  FServer := nil;
end;

procedure THandler.SetServer(AServer: IServer);
begin
  FServer := AServer;
end;

function THandler.GetServer: IServer;
begin
  Result := FServer;
end;

{ THandlerContainer }

constructor THandlerContainer.Create;
begin
  inherited Create;
  FHandlers := THandlerList.Create;
end;

destructor THandlerContainer.Destroy;
begin
  FHandlers.Free;
  inherited Destroy;
end;

function THandlerContainer.GetHandlers: THandlerList;
begin
  Result := FHandlers;
end;

{ THandlerWrapper }

constructor THandlerWrapper.Create;
begin
  inherited Create;
  FWrapper := nil;
end;

procedure THandlerWrapper.DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
var
  h: IHandler;
begin
  h := GetHandler;
  if Assigned(h) then
    h.Handle(ATarget, ARequest, AResponse)
  else
    ThisHandle(ATarget, ARequest, AResponse);
end;

procedure THandlerWrapper.ThisHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
begin
  //
end;

procedure THandlerWrapper.Handle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
var
  h: IHandler;
  //i: Integer;
begin
  Messager.Debug('Handle(%s, *, *)...', [ATarget]);
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
          h := FWrapper.GetHandler;
          if Assigned(h) then
            h.Handle(ATarget, ARequest, AResponse);
        end;
    end
  else
    begin
      DoHandle(ATarget, ARequest, AResponse);
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
  FServletContext := TJettyServletContext.Create(Self);
  FServletContext.ServerInfo := Self.UnitName + '.' + Self.ClassName;
  FFilterHolders := TFilterHolderList.Create;
  FServletHolders := TServletHolderList.Create;
end;

destructor TServletContextHandler.Destroy;
begin
  FServletContext.Free;
  FFilterHolders.Free;
  FServletHolders.Free;
  inherited Destroy;
end;

procedure TServletContextHandler.DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
begin
  inherited DoHandle(ATarget, ARequest, AResponse);
end;

procedure TServletContextHandler.ThisHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
var
  FURL: TCMURL;
  i: Integer;
  servlet: IServlet;
  servletConfig: IServletConfig;
begin
  FURL := TCMURL.Create(ATarget);
  Messager.Debug('ContextPath:%s URL.path:%s', [GetContextPath, FURL.Path]);
  if StartsStr(GetContextPath, FURL.Path) then
    begin
      Messager.Debug('--1----------------');
      for i:=0 to FServletHolders.Count-1 do
        begin
          //匹配 servlet
          Messager.Debug('--%s--%s', [GetContextPath + FServletHolders[i].GetURLPatterns[0], FURL.GetFullPath]);
          if SameText(GetContextPath + FServletHolders[i].GetURLPatterns[0], FURL.GetFullPath) then
            begin
              Messager.Debug('--2----------------');
              servlet := FServletHolders[i].GetServlet;
              if Assigned(servlet) then
                begin
                  //调用 init() 注入 servlet config
                  servletConfig := TServletConfig.Create('test.servlet', FServletContext);
                  servlet.Init(servletConfig);
                  servlet.Service(ARequest, AResponse);
                end;
            end;
        end;
    end;
end;

procedure TServletContextHandler.SetContextPath(const APath: string);
begin
  FServletContext.ContextPath := APath;
end;

function TServletContextHandler.GetContextPath: string;
begin
  Result := FServletContext.GetContextPath;
end;

function TServletContextHandler.GetInitParameters: ICMConstantParameterDataList;
begin
  Result := FServletContext.GetInitParameters;
end;

function TServletContextHandler.GetAttributes: ICMParameterDataList;
begin
  Result := FServletContext.GetAttributes;
end;

procedure TServletContextHandler.SetServerInfo(const AServerInfo: string);
begin
  FServletContext.ServerInfo := AServerInfo;
end;

procedure TServletContextHandler.AddFilter(AFilter: IFilterHolder);
begin
  FFilterHolders.Add(AFilter);
end;

procedure TServletContextHandler.AddServlet(AHolder: IServletHolder);
begin
  FServletHolders.Add(AHolder);
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

procedure TServer.Handle(ARequest: IServletRequest; AResponse: IServletResponse);
begin
  Self.Handle(ARequest.GetRequestURL, ARequest, AResponse);
end;

procedure TServer.DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
var
  FURL: TCMURL;
  i: Integer;
begin
  FURL := TCMURL.Create(ATarget);
  for i:=0 to FConnectors.Count-1 do
    begin
      //protocol、port相等时进行下一步（如 servlet context ）. host TODO
      if SameText(FConnectors[i].GetProtocol, FURL.Protocol) and (IntToStr(FConnectors[i].GetPort) = FURL.Port) then
        begin
          inherited DoHandle(ATarget, ARequest, AResponse);
          //AResponse.s
          Exit;
        end;
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

constructor TServletHandler.Create(AServletContext: IServletContext);
begin
  FServletContext := AServletContext;
end;

destructor TServletHandler.Destroy;
begin
  FServletContext := nil;
  inherited Destroy;
end;

procedure TServletHandler.AddFilter(AFilter: TFilterHolder);
begin

end;

procedure TServletHandler.AddListener(AListener: TListenerHolder);
begin

end;

procedure TServletHandler.AddServlet(AHolder: TServletHolder);
begin

end;

function TServletHandler.GetFilter(const AName: string): TFilterHolder;
begin

end;

function TServletHandler.GetFilters: TFilterHolderArray;
begin

end;

function TServletHandler.GetListeners: TListenerHolderArray;
begin

end;

function TServletHandler.GetServlet(const AName: string): TServletHolder;
begin

end;

function TServletHandler.GetServletContext: IServletContext;
begin

end;

function TServletHandler.GetServlets: TServletHolderArray;
begin

end;



end.

