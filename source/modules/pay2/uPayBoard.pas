unit uPayBoard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, DBGrids, Grids, Controls, LCLType, StdCtrls, DateUtils,
  cm_messager, cm_Plat,
  uForm, uSale, uDAO,
  uPay, uFramePay, uPayUtils;

type

  { TPayBoard }

  TPayBoard = class(TCMMessageableComponent, IPayBoard)
  private
    FPayFrame: TPayFrame;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ToPay(const AOrderUUID: string; const AAmount: currency): boolean;
    function SelectsDefinitely(AAmount: Currency; out thePayCode: string; out theAmount: Currency): Boolean;
  end;



implementation

{ TPayBoard }

function TPayBoard.ToPay(const AOrderUUID: string; const AAmount: currency): boolean;
var
  Form: TPOSForm;
begin
  Result := False;
  try
    Form := TPOSForm.Create(nil);
    Form.Width := FPayFrame.Width;
    Form.Height := FPayFrame.Height;
    Form.BorderStyle := bsNone;
    Form.KeyPreview := True;

    FPayFrame.Parent := Form;
    Form.OnKeyDown := @FormKeyDown;



    if Form.ShowModal() = mrOk then
      Result := True;
  finally
    Form.Free;
  end;
end;

function TPayBoard.SelectsDefinitely(AAmount: Currency; out thePayCode: string; out theAmount: Currency): Boolean;
begin
  Result := False;

end;



procedure TPayBoard.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: TPOSForm(Sender).ModalResult := mrCancel;
    VK_RETURN:
    begin
      Key := 0;
      //TPOSForm(Sender).ModalResult := mrOk;
    end;

  end;
end;


constructor TPayBoard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPayFrame := TPayFrame.Create(Self);
end;

destructor TPayBoard.Destroy;
begin
  //FGrid.Free;
  inherited Destroy;
end;

end.







