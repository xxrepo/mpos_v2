unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ubarcodes, LR_Class;

type

  { TForm1 }

  TForm1 = class(TForm)
    BarcodeQR1: TBarcodeQR;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    frReport1: TfrReport;
    Image1: TImage;
    Image2: TImage;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

  { TCMBarcodeQR }

  TCMBarcodeQR = class(TBarcodeQR)
  protected
    procedure intfPaintOnCanvas(const aTargetCanvas: TCanvas; const aRect: TRect); reintroduce;
  end;

var
  Form1: TForm1;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Image1.Canvas.FillRect(1, 1, 100, 100);
end;

var
  b: TCMBarcodeQR;

procedure TForm1.Button2Click(Sender: TObject);
begin
  b := TCMBarcodeQR.Create(nil);
  b.Text := '123';
  //b.Repaint;
  //b.Parent := Self;
  b.intfPaintOnCanvas(Image1.Canvas, Image1.ClientRect);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  p: TfrPage;
  v: TfrPictureView;
begin
  b.Text := 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaAFKSDAJFLSAJD LKFJSALKJGLK;SJ;GLKJKLAJLKDSJAFLJDSLAGHLKDSHGLSAHDGHASKHJK';
  b.Width := Image1.Width;
  b.Height := Image1.Height;
  Image2.Canvas.FillRect(0,0,0,0); //HandleAllocated = True
  b.intfPaintOnCanvas(Image2.Canvas, Image2.ClientRect);
  //
  Image2.Picture.SaveToFile('c:/11.jpg');
  frReport1.Pages.Clear;
  p := frReport1.Pages.Add();
  v := TfrPictureView.Create(p);

  //v.Picture.LoadFromFile('c:/11.jpg');
  v.Picture.Assign(Image2.Picture);

  v.Left := 10;
  v.Top := 10;
  v.Width := 100;
  v.Height := 100;

  frReport1.ShowReport;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Image1.Canvas.CopyRect(Image1.ClientRect, b.Canvas, b.ClientRect);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BarcodeQR1.Visible := False;
end;

{ TCMBarcodeQR }

procedure TCMBarcodeQR.intfPaintOnCanvas(const aTargetCanvas: TCanvas; const aRect: TRect);
begin
  inherited intfPaintOnCanvas(aTargetCanvas, aRect);
end;

end.

