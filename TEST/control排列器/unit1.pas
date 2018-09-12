unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Grids,
  Unit2;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGrid1: TStringGrid;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //
  //StringGrid1.Rows[0].Count;
  //Panel1.Alignment := ;
end;

end.

