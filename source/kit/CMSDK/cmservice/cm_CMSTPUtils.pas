unit cm_CMSTPUtils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager, cm_generics, cm_parameter,
  cm_servlet, cm_jetty, cm_JettyBase, cm_JettyImpl, cm_JettyCMS,
  cm_cmstp;

type

  { TSimpleCMSTP
    // TODO 后继整理改进
  }
  TSimpleCMSTP = class(TCMMessageable, IServletCollection, ICMSTPService)
  private
    FList: TCMHashInterfaceList<IServlet>;
    FServer: TCMSServer;
    procedure InitCMSTPService;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function AddServlet(const ACode: string; AServlet: IServlet): Boolean;
    function AddFilter(const ACode: string; AFilter: IFilter): Boolean;
    function AddListener(const ACode: string; AListener: IListener): Boolean;
  public
    function CMSTP(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponse: ICMSTPResponse): Boolean;
  end;

implementation

uses Unit2;

{ TSimpleCMSTP }

procedure TSimpleCMSTP.InitCMSTPService;
var
  servletContextHandler: TServletContextHandler;
  connector: IConnector;
  s, s2: IServlet;
  sh, sh2: TServletHolder;
  servletHandler: IServletHandler;
begin
  //server
  FServer := TCMSServer.Create;
  CMSTPService := FServer;
  //servlet context
  servletContextHandler := TServletContextHandler.Create;
  servletContextHandler.SetContextPath('/test');
  DefaultMessager.Info('ServletContextHandler加入后:' + servletContextHandler.GetContextPath);

  s := TTestServlet.Create;
  sh := TServletHolder.Create(servletContextHandler.GetServletContext);
  sh.SetName('server 111');
  sh.AddURLPattern('/a/b');
  sh.SetServlet(s);
  servletContextHandler.AddServlet(sh);

  s2 := TTestServlet2.Create;
  sh2 := TServletHolder.Create(servletContextHandler.GetServletContext);
  sh2.SetName('server 222');
  sh2.AddURLPattern('/a/b2');
  sh2.SetServlet(s2);
  servletContextHandler.AddServlet(sh2);

  //-------------------------------------
  servletHandler := TServletHandler.Create(servletContextHandler.GetServletContext);;
  servletContextHandler.SetHandler(servletHandler);
  servletHandler.Start;
  //-------------------------------------

  FServer.SetHandler(servletContextHandler);

  //连接器
  connector := TConnector.Create('cmstp');
  FServer.AddConnector(connector);

  sh.Start;
  sh2.Start;
  servletContextHandler.Start;
  FServer.Start;
end;

constructor TSimpleCMSTP.Create;
begin
  FList := TCMHashInterfaceList<IServlet>.Create;
  FServer := nil;
  //CMSTPService := Self;
  InitCMSTPService;
end;

destructor TSimpleCMSTP.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TSimpleCMSTP.AddServlet(const ACode: string; AServlet: IServlet): Boolean;
begin
  Result := FList.Add(ACode, AServlet) >= 0;
end;

function TSimpleCMSTP.AddFilter(const ACode: string; AFilter: IFilter): Boolean;
begin
  Result := False;
end;

function TSimpleCMSTP.AddListener(const ACode: string; AListener: IListener): Boolean;
begin
  Result := False;
end;

function TSimpleCMSTP.CMSTP(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponse: ICMSTPResponse): Boolean;
begin
  Result := False;
  if not Assigned(FServer) then
    Self.InitCMSTPService;
  Result := FServer.CMSTP(AURL, ARequestParameters, TheResponse);
end;

end.

