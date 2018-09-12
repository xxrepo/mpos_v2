unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  cm_controls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Image1: TImage;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    procedure testClick(Sender: TObject);
  public

  end;

  { TA }

  TA = class
  private
    s: string;
  public
    procedure aa; virtual;
    property ss: string read s write s;
  end;

  { TB }

  TB = class(TA)
  public
    procedure aa; override;
  end;

  { TC }

  TC = class(TB)
  public
    procedure aa; override;
  end;

var
  Form1: TForm1;
  PicturePanel: TCMImagePanel;

implementation

{$R *.frm}

{ TC }

procedure TC.aa;
var
  pro: TThreadMethod;
begin
  TMethod(pro).Code := @TA.aa;
  TMethod(pro).Data := Self;
  //
  ss := 'ABC';
  pro();
  ShowMessage('TC.aa' + s + ' ' + Self.ClassName);
end;

{ TB }

procedure TB.aa;
begin
  inherited aa;
  ShowMessage('TB.aa' + s + ' ' + Self.ClassName);
end;

{ TA }

procedure TA.aa;
begin
  ShowMessage('TA.aa ' + s + ' ' + Self.ClassName);
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  sg: TGraphic;
begin
  sg := TPNGImage.Create;
  sg.LoadFromFile('11.png');

  Panel1.Canvas.StretchDraw(Panel1.ClientRect, sg);



  //Image1.Picture.LoadFromFile('');
  Image1.StretchInEnabled := False;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  //PicturePanel.Stretch := True;
  PicturePanel.ImageAlign := alBottom;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  //PicturePanel.Stretch := True;
  PicturePanel.ImageAlign := alClient;
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  //PicturePanel.Stretch := True;
  PicturePanel.ImageAlign := alNone;
end;

procedure TForm1.Button13Click(Sender: TObject);
var
  r: TRect;
begin
  r := Rect(10, 10, 80, 40);
  PicturePanel.Stretch := True;
  PicturePanel.ImageRect := r;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TC.Create.aa;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  PicturePanel := TCMImagePanel.Create(Self);
  PicturePanel.Parent := Self;
  PicturePanel.OnClick := @testClick;
  PicturePanel.Width := 220;
  PicturePanel.Height := 100;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  PicturePanel.Caption := 'ABCabc';
  PicturePanel.Color := clLime;

  PicturePanel.Picture.LoadFromFile('11.png');

end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  PicturePanel.Stretch := True;
  //PicturePanel.Proportional := True;
  //PicturePanel.Center := True;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  //PicturePanel.Stretch := True;
  PicturePanel.ImageAlign := alLeft;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  r, r2: TRect;
begin
  r.SetLocation(10, 20);
  r.Width := 30;
  r.Height := 40;

  r2 := r;

  ShowMessageFmt('%d %d'#10'%d %d', [r2.Left, r2.Top, r2.Width, r2.Height]);
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  //PicturePanel.Stretch := True;
  PicturePanel.ImageAlign := alTop;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  //PicturePanel.Stretch := True;
  PicturePanel.ImageAlign := alRight;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
  ShowMessage('aa');
end;

procedure TForm1.testClick(Sender: TObject);
begin
  ShowMessage('testClick'#10 + Sender.ClassName);
end;

end.

