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
  Classes, SysUtils, Generics.Collections, Generics.Defaults,
  cm_interfaces, cm_messager, cm_parameter, cm_threadutils,
  cm_servlet, cm_servletutils;

type

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
    //boolean	isAsyncSupported​()
    //void	setAsyncSupported​(boolean suspendable)
  end;

  IFilterHolder = interface(IHolder)
    ['{1178C633-64AF-40B6-AA1F-64D6F4365BDF}']
    procedure SetFilter(AFilter: IFilter);
    function GetFilter: IFilter;
  end;

  TFilterHolderList = TList<IFilterHolder>;

  IListenerHolder = interface(IHolder)
    ['{465B175A-9B87-4551-9675-EDC08B1C2352}']
    procedure SetListener(AListener: IListener);
    function GetListener: IListener;
  end;

  TListenerHolderList = TList<IListenerHolder>;

  IServletHolder = interface(IHolder)
    ['{2011D13B-71A3-4B41-8EB5-3EA454F213DD}']
    procedure AddURLPattern(const AURLPattern: string);
    function GetURLPatterns: TStrings;
    procedure SetServlet(AServlet: IServlet);
    function GetServlet: IServlet;
  end;

  TServletHolderList = TList<IServletHolder>;

  (************************* Handler **************************************************************)

  IServer = interface;

  IHandler = interface(ILifeCycle)
    ['{CEB5A742-7382-4231-B06C-EACDC9AD395C}']
    procedure SetServer(AServer: IServer);
    function GetServer: IServer;
    procedure Handle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
  end;

  THandlerList = TList<IHandler>;

  IHandlerContainer = interface(IHandler)
    ['{91ED7C60-217D-4F84-ADBD-8F58C00E501B}']
    function GetHandlers: THandlerList;
  end;

  { IHandlerWrapper
    //A HandlerWrapper acts as a Handler but delegates the handle method and life cycle events to a delegate.
    //This is primarily used to implement the Decorator pattern.
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
  end;

  IServletContextHandler = interface(IContextHandler)
    ['{EEAB8590-7C93-4727-9F8D-438454DD4805}']
    procedure AddFilter(AFilter: IFilterHolder);
    procedure AddServlet(AHolder: IServletHolder);
  end;

  IServletHandler = interface(IHandlerContainer)
    ['{2F2719A8-316E-4B04-8FB5-D0945A68A0AC}']
    procedure AddFilter(AFilter: IFilterHolder);
    procedure AddListener(AListener: IListenerHolder);
    procedure AddServlet(AServlet: IServletHolder);
    function GetFilter(const AName: string): IFilterHolder;
    function GetFilters: TFilterHolderList;
    function GetListeners: TListenerHolderList;
    function GetServlet(const AName: string): IServletHolder;
    function GetServletContext: IServletContext;
    function GetServlets: TServletHolderList;
  end;

  (************************* Server ***************************************************************)

  IServer = interface(IHandlerContainer)
    ['{F5085010-2151-463B-82BD-3126B9549F19}']
    procedure AddConnector(AConnector: IConnector);
    procedure RemoveConnector(AConnector: IConnector);
    function GetConnectors: TConnectorList;
    function GetThreadPool: TExecuteThreadBool;
  end;

  (************************* base class ***********************************************************)

  { TJettyServletContext }

  TJettyServletContext = class(TServletContext)
  public
    function GetNamedDispatcher(const AName: string): IRequestDispatcher; override;
    function GetRequestDispatcher(const APath: string): IRequestDispatcher; override;
  end;

  { TJettyRequestDispatcher }

  TJettyRequestDispatcher = class(TCMMessageable, IRequestDispatcher)
  private
    FTarget: string;
    FHandler: IHandler;
  public
    constructor Create(const ATarget: string; AHandler: IHandler);
    destructor Destroy; override;
  public
    procedure Forward(ARequest: IServletRequest; AResponse: IServletResponse);
    procedure Include(ARequest: IServletRequest; AResponse: IServletResponse);
  end;


implementation

{ TJettyServletContext }

function TJettyServletContext.GetNamedDispatcher(const AName: string): IRequestDispatcher;
begin
  Result := nil;
end;

function TJettyServletContext.GetRequestDispatcher(const APath: string): IRequestDispatcher;
begin
  Result := nil;
end;

{ TJettyRequestDispatcher }

constructor TJettyRequestDispatcher.Create(const ATarget: string; AHandler: IHandler);
begin
  FTarget := ATarget;
  FHandler := AHandler;
end;

destructor TJettyRequestDispatcher.Destroy;
begin
  FHandler := nil;
  inherited Destroy;
end;

procedure TJettyRequestDispatcher.Forward(ARequest: IServletRequest; AResponse: IServletResponse);
begin
  AResponse.GetContent.Clear;
  FHandler.Handle(FTarget, ARequest, AResponse);
end;

procedure TJettyRequestDispatcher.Include(ARequest: IServletRequest; AResponse: IServletResponse);
var
  rps: IServletResponse;
  i: Integer;
  n: string;
  p: ICMParameterData;
begin
  rps := TServletResponse.Create;
  FHandler.Handle(FTarget, ARequest, rps);
  for i:=0 to rps.GetContent.Count-1 do
    begin
      n := rps.GetContent.GetName(i);
      p := rps.GetContent.Get(i);
      if (n <> '') and (not p.IsNull) then
        AResponse.GetContent.SetData(n, p);
    end;
end;

end.

