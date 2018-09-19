unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_servlet;

type

  { TTestServlet }

  TTestServlet = class(TGenericServlet)
  public
    procedure Init; override;
    procedure Service(ARequest: IServletRequest; AResponse: IServletResponse); override;
  end;

  { TTestServlet2 }

  TTestServlet2 = class(TGenericServlet)
  public
    procedure Init; override;
    procedure Service(ARequest: IServletRequest; AResponse: IServletResponse); override;
  end;

implementation

{ TTestServlet }

procedure TTestServlet.Init;
begin
  Messager.Info('Init()...');
end;

procedure TTestServlet.Service(ARequest: IServletRequest; AResponse: IServletResponse);
var
  rd: IRequestDispatcher;
begin
  Messager.Info('Service() hello world!');
  Messager.Info('Service() ServletName:' + Self.GetServletName);
  Messager.Info('Service() ServerInfo:' + Self.GetServletConfig.GetServletContext.GetServerInfo);

  AResponse.GetContent.SetString('test', 'haha');
  Messager.Info('Service() --GetRequestDispatcher()----------------------------');
  //rd := Self.GetServletContext.GetRequestDispatcher('/a/b2');

  //rd.Forward(ARequest, AResponse);

  Messager.Info('Service() over.');
end;

{ TTestServlet2 }

procedure TTestServlet2.Init;
begin
  Messager.Info('Init()...');
end;

procedure TTestServlet2.Service(ARequest: IServletRequest; AResponse: IServletResponse);
begin
  Messager.Info('Service() hello world! 222');
  AResponse.GetContent.SetString('test', 'haha 222');
end;

end.

