unit uProductSelectBoard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, DBGrids, Grids, Controls, LCLType, StdCtrls, DateUtils,
  cm_messager, cm_Plat, cm_sysutils,
  uForm, uSale, uDAO,
  uProductPO;

type

  { TProductSelectBoard }

  TProductSelectBoard = class(TCMMessageableComponent, IProductSelectBoard)
  private
    FGrid: TStringGrid;
    procedure SetShow(list: TProductList);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure DoDBClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ToSelect(list: TProductList): TProduct;

  end;



implementation

{ TProductSelectBoard }

function TProductSelectBoard.ToSelect(list: TProductList): TProduct;
var
  Form: TPOSForm;
begin
  Result := nil;
  try
    Form := TPOSForm.Create(nil);
    Form.Width := 640;
    Form.Height := 480;
    Form.BorderStyle := bsNone;
    Form.KeyPreview := True;
    Form.OnKeyDown := @FormKeyDown;

    FGrid := TStringGrid.Create(Form);
    FGrid.Parent := Form;
    FGrid.Align := TAlign.alClient;
    FGrid.Options := FGrid.Options + [goRowSelect];
    FGrid.ColCount := 3;
    FGrid.FixedCols := 0;
    FGrid.Cells[0, 0] := 'GID';
    FGrid.Cells[1, 0] := 'ProductName';
    FGrid.Cells[2, 0] := 'Price';
    FGrid.ColWidths[0] := 84;
    FGrid.ColWidths[1] := Form.Width - 84 - 64;
    FGrid.ColWidths[2] := 64;
    FGrid.ScrollBars := ssAutoVertical;

    SetShow(list);
    FGrid.OnDblClick := @DoDBClick;

    if Form.ShowModal() = mrOk then
      Result := TProduct(list[FGrid.Row - 1]);
  finally
    Form.Free;
  end;
end;

procedure TProductSelectBoard.SetShow(list: TProductList);
var
  i: integer;
begin

  FGrid.RowCount := list.Count + 1;
  for i := 0 to list.Count - 1 do
  begin
    FGrid.Cells[0, i + 1] := IntToStr(TProduct(list[i]).GID);
    FGrid.Cells[1, i + 1] := TProduct(list[i]).ProductName;
    FGrid.Cells[2, i + 1] := DefFmtCurrStr(TProduct(list[i]).RetailPrice);
  end;
end;

procedure TProductSelectBoard.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
    begin
      Key := 0;
      TPOSForm(Sender).ModalResult := mrOk;
    end;
    VK_ESCAPE:
    begin
      Key := 0;
      TPOSForm(Sender).ModalResult := mrCancel;
    end;
  end;
end;

procedure TProductSelectBoard.DoDBClick(Sender: TObject);
begin
  //TForm(Sender).ModalResult := mrOk;
end;

constructor TProductSelectBoard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FGrid := TStringGrid.Create(Self);

  //FGrid.Options := FGrid.Options + [goRowSelect];
  //FGrid.Cells[0, 0] := 'GID';
  //FGrid.Cells[1, 0] := 'ProductName';
end;

destructor TProductSelectBoard.Destroy;
begin
  //FGrid.Free;
  inherited Destroy;
end;

end.







