unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  cm_classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure println(msg: string);
  end;

  IA = interface
    ['{9F4F5C57-C94E-42FF-BE9C-C5AA819DFCC5}']
    procedure aa;
  end;

  { TA }

  TA = class(TInterfacedObject, IA)
  public
    constructor Create;
    destructor Destroy; override;
    procedure aa;
  end;

var
  Form1: TForm1;
  FList, FList2: TCMInterfaceList;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FList := TCMInterfaceList.Create;
end;

procedure TForm1.println(msg: string);
begin
  Memo1.Lines.Add(msg);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  a: IA;
begin
  a := TA.Create;
  FList.Add(a);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FList2 := FList.Clone;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FList.Clear;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FList2.Clear;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  IA(FList2[0]).aa;
end;

{ TA }

constructor TA.Create;
begin
  Form1.println('TA.Create');
end;

destructor TA.Destroy;
begin
  Form1.println('TA.Destroy');
  inherited Destroy;
end;

procedure TA.aa;
begin
  Form1.println('TA.aa');
end;

end.

