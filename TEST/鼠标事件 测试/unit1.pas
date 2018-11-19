unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Grids, Types,
  LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseEnter(Sender: TObject);
    procedure Panel1MouseLeave(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean)
      ;
    procedure Panel1MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure Panel1MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure StringGrid1MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  private

  public
    procedure println(msg: string);
  end;

  TMouseButtonX = (mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2, None);

const
  MouseButtonNames: array[TMouseButton] of string = ('mbLeft', 'mbRight', 'mbMiddle', 'mbExtra1', 'mbExtra2');

var
  Form1: TForm1;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  println(MouseButtonNames[Button]);
  println(Format('%d - %d', [X, Y]));
  if ssDouble in Shift then
    println('aaaaaaaaaaaaaa');
end;

procedure TForm1.Panel1MouseEnter(Sender: TObject);
var
  p: TPoint;
begin
  println('Panel1MouseEnter');
  p := Panel1.ScreenToClient(Mouse.CursorPos);
  println(Format('CursorPos:'#9'%d - %d', [Mouse.CursorPos.X, Mouse.CursorPos.Y]));
  println(Format('ScreenToClient:'#9'%d - %d', [p.X, p.Y]));
end;

procedure TForm1.Panel1MouseLeave(Sender: TObject);
begin
  println('Panel1MouseLeave');
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Self.Caption := Format('%d - %d', [X, Y]);
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  println('Panel1MouseUp');
end;

procedure TForm1.Panel1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  println('Panel1MouseWheel');
end;

procedure TForm1.Panel1MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  println('Panel1MouseWheelDown');
end;

procedure TForm1.Panel1MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  println('Panel1MouseWheelUp');
end;

procedure TForm1.StringGrid1MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  println('Panel1MouseWheelUp');
  Handled := False;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  //InitMouse;  //Mouse单元
  //ShowMouse;
  println(Format('%d - %d', [Mouse.CursorPos.X, Mouse.CursorPos.Y]));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  p: TPoint;
begin
  //SetMouseXY(8, 8);
  p.X := 8;
  p.Y := 8;
  Mouse.CursorPos := p;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  b: TMouseButton;
  x: TMouseButtonX;
begin
  //mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2

  x := mbExtra1;
  b := TMouseButton(x);

  println(MouseButtonNames[b]);
  println( IntToStr(Ord(b)) );
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  println( IntToStr( Ord('A') ) );
end;

procedure TForm1.println(msg: string);
begin
  Memo1.Lines.Add(msg);
end;

end.

