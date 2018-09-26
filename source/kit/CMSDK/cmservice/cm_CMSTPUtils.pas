unit cm_CMSTPUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager,
  cm_servlet,
  cm_jetty, cm_JettyBase, cm_JettyImpl, cm_JettyCMS,
  cm_cmstp;


  procedure InitCMSTPService;

implementation

uses Unit2;

procedure InitCMSTPService;
var
  server: TCMSServer;
  servletContextHandler: TServletContextHandler;
  connector: IConnector;
  s, s2: IServlet;
  sh, sh2: TServletHolder;
  servletHandler: IServletHandler;
begin
  //server
  server := TCMSServer.Create;
  CMSTPService := server;
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

  server.SetHandler(servletContextHandler);

  //连接器
  connector := TConnector.Create('cmstp');
  server.AddConnector(connector);

  sh.Start;
  sh2.Start;
  servletContextHandler.Start;
  server.Start;
end;

end.

