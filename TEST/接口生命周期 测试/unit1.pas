unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  aList: TFPList;

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

procedure TForm1.Button4Click(Sender: TObject);
begin
  aIntf := TA.Create;
  aIntf.aa;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  ai: IA;
begin
  ai := TA.Create;
  ai.aa;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  ai: IA;
begin
  ai := TA.Create;
  aList.Add(ai);
  ai.aa;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  aList := TFPList.Create;
end;

{ TA }

destructor TA.Destroy;
begin
  if Assigned(Form1) then
    if Form1.Showing then
      Form1.Memo1.Lines.Add('TA.Destroy');
  inherited Destroy;
end;

procedure TA.aa;
begin
  Form1.Memo1.Lines.Add('aa');
end;

end.

