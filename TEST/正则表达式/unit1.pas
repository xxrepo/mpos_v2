unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  cm_sysutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    procedure println(msg: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  s: string;
begin
  println('-----------------------------');
  //1 精确匹配
  //println(Format('1  %s', [BoolToStr(CheckStr('^([a-z]|[A-Z]|[0-9])*.jsp$', 'a111.jsp'), True)]));
  //2 路径匹配
  s := '/user/*';
  println(Format('1  %s', [BoolToStr(ValidateStr('^/user/([a-z]|[A-Z]|[0-9])*$', '/user/a111'), True)]));
  //3 扩展名匹配
  s := '*.jsp';
  println(Format('1  %s', [BoolToStr(ValidateStr('^([a-z]|[A-Z]|[0-9])*.jsp$', 'a111.jsp'), True)]));
  //4 缺省匹配
  s := '/';
  println('-----------------------------');
end;

procedure TForm1.println(msg: string);
begin
  Memo1.Lines.Add(msg);
end;

end.

