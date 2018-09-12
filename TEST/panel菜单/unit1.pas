unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, db,
  LMessages, Grids,
  testMenu,
  cm_controlutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Panel2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure selMenu(Sender: TControl; ACol, ARow: Integer);
    procedure unselMenu(Sender: TControl; ACol, ARow: Integer);
    procedure menuClick(Sender: TObject);
  public

  end;

  { TT }

  TT = class(TPanel)
  public
    procedure CMMouseEnter(var Message :TLMessage); message CM_MOUSEENTER;
  end;

var
  Form1: TForm1;
  pMemu: TCMPanelMenu;

implementation

{$R *.frm}

{ TT }

procedure TT.CMMouseEnter(var Message: TLMessage);
begin
  ShowMessage('ll');
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  pMemu := TCMPanelMenu.Create(Panel1, 4);
  for i:=0 to 20 do
    pMemu.addBtn('aaa' + IntToStr(i), nil, i);
end;

var
  t: TT;

procedure TForm1.Button2Click(Sender: TObject);
begin
  t := TT.Create(Self);
  t.Parent := Panel1;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  layout: TCMFlowLayout;
  i: Integer;
  p: TPanel;
begin
  //for i:=Panel1.ControlCount-1 downto 0 do
  //  Panel1.RemoveControl(Panel1.Controls[i]);

  layout := TCMFlowLayout.Create(Panel1);
  layout.BorderSpacing := 30;
  layout.LeftSpacing := 10;
  layout.TopSpacing := 10;
  //layout.LineMaxCount := 10;
  layout.FlowLayoutOrientation := floLeftToRight;
  for i:=1 to 60 do
    begin
      p := TPanel.Create(Panel1);
      p.Parent := Panel1;
      p.Width := 50 + Random(50);
      p.Height := 20 + Random(50);
      p.Caption := 'aaa' + IntToStr(i);
      p.Color := clInfoBk;
      layout.AddLayoutControl(p);
    end;
  layout.ReLayout;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  pp: TPanel;
begin
  pp := TPanel.Create(Self);
  pp.Parent := Panel1;
  pp.Top := 30;
  pp.Left := 30;
end;

var
  gridLayout: TCMGridLayout;

procedure TForm1.Button7Click(Sender: TObject);
var
  i: Integer;
  p: TPanel;
begin
  //for i:=Panel1.ControlCount-1 downto 0 do
  //  Panel1.RemoveControl(Panel1.Controls[i]);

  gridLayout := TCMGridLayout.Create(Panel1, 5, 5);
  gridLayout.BorderSpacing := 30;
  gridLayout.LeftSpacing := 10;
  gridLayout.TopSpacing := 10;
  for i:=1 to 20 do
    begin
      p := TPanel.Create(Panel1);
      p.Parent := Panel1;
      p.Width := 50 + Random(50);
      p.Height := 20 + Random(50);
      p.Caption := 'aaa' + IntToStr(i);
      p.Color := clInfoBk;
      gridLayout.PutLayoutControl(p);
    end;
  gridLayout.ReLayout;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  gridLayout.AlignAtGrid := True;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  gridLayout.ColWidths[0] := 120;
  gridLayout.RowHeights[1] := 40;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  gridLayout.ColCount := 3;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  gridLayout.AlignAtGrid := False;
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Edit1.Text := Format('%d %d', [X, Y]);
end;

procedure TForm1.Panel2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Edit2.Text := Format('%d %d', [X, Y]);
end;

procedure TForm1.selMenu(Sender: TControl; ACol, ARow: Integer);
begin
  //Memo1.Lines.Add('%d-%d', [ACol, ARow]);
  Sender.Color := clPurple;
end;

procedure TForm1.unselMenu(Sender: TControl; ACol, ARow: Integer);
begin
  Sender.Color := clInfoBk;
end;

var
  gridMenu: TCMGridLayoutMenu;

procedure TForm1.menuClick(Sender: TObject);
begin
  gridMenu.SelectMenuItem(TControl(Sender));
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  i: Integer;
  p: TPanel;
begin
  //for i:=Panel1.ControlCount-1 downto 0 do
  //  Panel1.RemoveControl(Panel1.Controls[i]);

  gridMenu := TCMGridLayoutMenu.Create(Panel1, 5, 5);
  gridMenu.BorderSpacing := 30;
  gridMenu.LeftSpacing := 10;
  gridMenu.TopSpacing := 10;

  gridMenu.OnMenuItemSelectEnter := @selMenu;
  gridMenu.OnMenuItemSelectLeave := @unselMenu;

  gridMenu.AlignAtGrid := True;
  for i:=1 to 20 do
    begin
      p := TPanel.Create(Panel1);
      p.Parent := Panel1;
      p.Width := 50 + Random(50);
      p.Height := 20 + Random(50);
      p.Caption := 'aaa' + IntToStr(i);
      p.Color := clInfoBk;
      p.OnClick := @menuClick;
      gridMenu.PutLayoutControl(p);
    end;
  gridMenu.ReLayout;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(pMemu) then
    case Key of
    37: pMemu.left;
    38: pMemu.up;
    39: pMemu.right;
    40: pMemu.down;
    end
  else if Assigned(gridMenu) then
    case Key of
    37: gridMenu.left;
    38: gridMenu.up;
    39: gridMenu.right;
    40: gridMenu.down;
    end;
end;


end.

