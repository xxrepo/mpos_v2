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
    procedure Service(ARequest: IServletRequest; AResponse: IServletResponse); override;
  end;

implementation

{ TTestServlet }

procedure TTestServlet.Service(ARequest: IServletRequest; AResponse: IServletResponse);
begin
  Messager.Info('hello world!');
  Messager.Info(Self.GetServletName);
  Messager.Info('------------------------------');
  Messager.Info(Self.GetServletConfig.GetServletContext.GetServerInfo);

  AResponse.GetContent.SetString('test', 'haha');
  Messager.Info('------------------------------');
  Self.GetServletContext.GetRequestDispatcher('/a/b');

  Messager.Info('over.');
end;

end.

