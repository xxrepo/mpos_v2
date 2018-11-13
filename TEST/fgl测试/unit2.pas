unit Unit2;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  cm_generics;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure println(msg: string);
  end;

  IB = interface
    ['{53E8396C-0D35-457A-A107-5598EE237B5E}']
    procedure bb;
  end;

  { TA }

  { TB }

  TB = class(TInterfacedObject, IB)
  public
    constructor Create;
    destructor Destroy; override;
    procedure bb;
  end;

  THList = class(TGFPHashInterfaceList<IB>);

var
  Form2: TForm2;
  hsList: THList;

implementation

{$R *.frm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  hsList := THList.Create;
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  b: IB;
  s: string;
begin
  b := TB.Create;
  b.bb;
  s := FloatToStr(now);
  hsList.Add(s, b);

  println(IntToStr(hsList.IndexOf(b)));
  println(IntToStr(hsList.FindIndexOf(s)));
  //hsList.Find(s).bb;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  hsList[0].bb;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  hsList.Delete(0);
end;


procedure TForm2.Button5Click(Sender: TObject);
begin
  hsList.Clear;
end;

var
  bb: IB;

procedure TForm2.Button6Click(Sender: TObject);
begin
  bb := TB.Create;
  hsList.Add(FloatToStr(now), bb);
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  bb := nil;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  hsList.Remove(bb);
end;

procedure TForm2.println(msg: string);
begin
  Memo1.Lines.Add(msg);
end;

{ TB }

constructor TB.Create;
begin
  Form2.println('TB.Create');
end;

destructor TB.Destroy;
begin
  if Assigned(Form2) then
    if Form2.Showing then
      Form2.println('TB.Destroy');
  inherited Destroy;
end;

procedure TB.bb;
begin
  Form2.println('TB.bb');
end;

end.

