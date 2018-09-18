unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ubarcodes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BarcodeQR1: TBarcodeQR;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Image1: TImage;
    Image2: TImage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  r: TRect;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  r := Bounds(Image1.ClientRect.Left+30, Image1.ClientRect.Top+30, Image1.ClientRect.Width-60, Image1.ClientRect.Height-60);
  Image2.Canvas.CopyRect(Image2.ClientRect, Image1.Canvas, r);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Image1.Canvas.Brush.Color := clYellow;
  Image1.Canvas.FillRect(r);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Image2.Canvas.CopyRect(Image2.ClientRect, BarcodeQR1.Canvas, r);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  BarcodeQR1.Canvas.Brush.Color := clYellow;
  BarcodeQR1.Canvas.FillRect(r);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  BarcodeQR1.Text := '1';
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  BarcodeQR1.Text := 'aaaaaaaadsdsfadsfsdafdsfasdfsdgdsfsdafsdafafgdsf';
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  x, y: Integer;
begin
  //ShowMessage(IntToStr(BarcodeQR1.Canvas.Height-1));

  for y:=BarcodeQR1.Height-1 downto 0 do
    for x:=0 to BarcodeQR1.Width-1 do
      begin
        if BarcodeQR1.Canvas.Pixels[x,y] = BarcodeQR1.ForegroundColor then
          begin
            BarcodeQR1.Canvas.Brush.Color := clYellow;
            BarcodeQR1.Canvas.FillRect(x,y, BarcodeQR1.Width, BarcodeQR1.Height);
            Exit;
          end;
      end;
end;

end.

