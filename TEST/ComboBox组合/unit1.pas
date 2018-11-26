unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComboEx,
  Unit2, Unit3;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckComboBox1: TCheckComboBox;
    ComboBox1: TComboBox;
    ComboBoxEx1: TComboBoxEx;
    Edit1: TEdit;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ComboBox1DropDown(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure println(msg: string);
  end;



var
  Form1: TForm1;
  t: tthread;
  CMComboBox: TCMComboEdit;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
begin
  CMComboBox := TCMComboEdit.Create(Self);
  CMComboBox.Parent := Panel1;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CMComboBox.Items.Add('aa');
  CMComboBox.Items.Add('bb');
  CMComboBox.Items.Add('cc');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CMComboBox.Width := 200;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  //ComboBox1.Items;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  x: TX;
begin
  x := TX.Create(Self);
  x.Parent := Panel1;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  CMComboBox.SelectOnly := True;
end;

procedure TForm1.ComboBox1DropDown(Sender: TObject);
begin
  Memo1.Lines.Add('ComboBox1DropDown');
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  Memo1.Lines.Add('ComboBox1Select');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PrintMsg := @println;
end;

procedure TForm1.println(msg: string);
begin
  Memo1.Lines.Add(msg);
end;



end.

