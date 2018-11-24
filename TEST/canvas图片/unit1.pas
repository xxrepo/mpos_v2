unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  pic: TPicture;
begin
  //Panel1.Canvas.;
  //Image1.Picture;
  pic := TPicture.Create;
  pic.LoadFromFile('d:/set.png');
  //Panel1.Canvas.Brush.Bitmap := TPortableNetworkGraphic.Create;
  Panel1.Canvas.Brush.Bitmap := pic.PNG;

  Panel1.Canvas.Brush.Bitmap.LoadFromFile('d:/set.png');
  Panel1.Canvas.FillRect(1,1,60,60);
end;

end.

