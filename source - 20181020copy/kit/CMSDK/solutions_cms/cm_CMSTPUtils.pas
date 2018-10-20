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
  TSimpleCMSTP = class(TCMMessageable, IServletContainer, ICMSTP)
  private
    FList: TCMHashInterfaceList<IServlet>;
    FServer: TCMSServer;
    FServletContextHandler: TServletContextHandler;
    procedure InitCMSTPService;
    procedure InjectServlet; //把容器中的 Servlet 注入
  public
    constructor Create;
    destructor Destroy; override;
  public //IServletContainer
    function AddServlet(const ACode: string; AServlet: IServlet): Boolean;
    function AddFilter(const ACode: string; AFilter: IFilter): Boolean;
  public //ICMSTP
    function Post(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponse: ICMSTPResponse): Boolean;
  end;



implementation

uses Unit2;

{ TSimpleCMSTP }

procedure TSimpleCMSTP.InitCMSTPService;
var

  connector: IConnector;
  s, s2: IServlet;
  sh, sh2: TServletHolder;
  ish: IServletHolder;
  servletHandler: IServletHandler;
begin
  cm_servlet.ServletContainer := Self;

  //server
  FServer := TCMSServer.Create;
  //servlet context
  FServletContextHandler := TServletContextHandler.Create;
  FServletContextHandler.SetContextPath('/test');
  DefaultMessager.Info('ServletContextHandler加入后:' + FServletContextHandler.GetContextPath);

  s := TTestServlet.Create;
  //sh := TServletHolder.Create(servletContextHandler.GetServletContext);
  //sh.SetName('server 111');
  //sh.AddURLPattern('/a/b');
  //sh.SetServlet(s);
  //servletContextHandler.AddServlet(sh);

  //ish := FServletContextHandler.AddServlet('S11', s);
  //ish.AddURLPattern('/a/b');

  s2 := TTestServlet2.Create;
  {sh2 := TServletHolder.Create(FServletContextHandler.GetServletContext);
  sh2.SetName('server 222');
  sh2.AddURLPattern('/a/b2');
  sh2.SetServlet(s2);
  FServletContextHandler.AddServlet(sh2); }

  InjectServlet;

  //-------------------------------------
  servletHandler := TServletHandler.Create(FServletContextHandler.GetServletContext);
  FServletContextHandler.SetHandler(servletHandler);
  servletHandler.Start;
  //-------------------------------------

  FServer.SetHandler(FServletContextHandler);

  //连接器
  connector := TConnector.Create('cmstp');
  FServer.AddConnector(connector);

  sh.Start;
  sh2.Start;
  FServletContextHandler.Start;
  FServer.Start;
end;

procedure TSimpleCMSTP.InjectServlet;
var
  i: Integer;
  servlet: IServlet;
  servletHolder: IServletHolder;
begin
  for i:=0 to FList.Count-1 do
    begin
      servlet := FList[i];
      servletHolder := TServletHolder.Create(FServletContextHandler.GetServletContext);
      servletHolder.SetName('servlet ' + IntToStr(i));
      servletHolder.AddURLPattern('/a/b' + IntToStr(i));
      servletHolder.SetServlet(servlet);
      FServletContextHandler.AddServlet(servletHolder);
    end;
end;

constructor TSimpleCMSTP.Create;
begin
  FList := TCMHashInterfaceList<IServlet>.Create;
  FServer := nil;
  TCMSTPURLConnection.CMSTPService := Self;
  //InitCMSTPService;
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

function TSimpleCMSTP.Post(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponse: ICMSTPResponse): Boolean;
begin
  Result := False;
  if not Assigned(FServer) then
    Self.InitCMSTPService;
  Result := FServer.CMSTP(AURL, ARequestParameters, TheResponse);
end;

end.

