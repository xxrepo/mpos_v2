unit Unit1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  cm_generics;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private

  public

  end;

  IA = interface
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
  aa: IA;
  ap: Pointer;
  list: TCMHashInterfaceList<IA>;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  a: IA;
begin
  a := TA.Create;
  a.aa;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  aa := TA.Create;
  ap := aa;
  aa.aa;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  aa := nil;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  //ap := IA(TA.Create);
  if assigned(ap) then
    Form1.Memo1.Lines.Add('----------')
  else
    IA(ap).aa;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  IA(ap) := nil;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  a: IA;
begin
  list := TCMHashInterfaceList<IA>.Create;
  a := TA.Create;
  a.aa;
  list.Add('aaa', a);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  list.Find('aaa').aa;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  list.Remove(list.Find('aaa'));
  Form1.Memo1.Lines.Add('list count:' + IntToStr(list.Count));
end;

{ TA }

destructor TA.Destroy;
begin
  Form1.Memo1.Lines.Add('TA.Destroy');
  inherited Destroy;
end;

procedure TA.aa;
begin
  Form1.Memo1.Lines.Add('TA.aa');
end;

end.

