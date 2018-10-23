unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private

  public

  end;

  IA = interface
    ['{2C514E35-6979-4EE0-8D69-229E1CCEE26B}']
    procedure aa;
  end;

  { TA }

  TA = class(TInterfacedObject, IA)
  public
    destructor Destroy; override;
    procedure aa;
  end;

var
  Form1: TForm1;
  aIntf: IA;

  //测试接口怎样指派实例与其释放的关系

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  aObj: TA;
begin
  aObj := TA.Create;
  aIntf := aObj;
  aObj.aa;


  //当实例未指定接口变更时应显式释放，否则应自动释放，显式释放反而出错

  //aObj.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  aIntf := nil;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  aIntf.aa;
end;

{ TA }

destructor TA.Destroy;
begin
  Form1.Memo1.Lines.Add('TA.Destroy');
  inherited Destroy;
end;

procedure TA.aa;
begin
  Form1.Memo1.Lines.Add('aa');
end;

end.

