unit uPayForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uForm, uPay, cm_Plat,
  cm_dialogs, cm_interfaces;

type

  { TPayForm }

  TPayForm = class(TPOSForm, IPayBoard)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    FPromptPanel: TPanel;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    labNeedAmount: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    PayPanel: TPanel;
    procedure edtRealAmountKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    FMsgBar: TCMMsgBar;
    FPayUUID: string;
    FPayCode: string;
    FAmount: currency;
    FListener: IPayBoardListener;
  public
    function GetServiceCode: string;
    procedure PromptMessage(AEventType: TEventType; const AMsg: string);
    procedure SetPayView(AVO: TPayView);
    procedure StartPay(const APayUUID: string);
    procedure StopPay;
    procedure SetListener(AListener: IPayBoardListener);
  end;

  { TPayBoardEvent }

  TPayBoardEvent = class(TCMEvent, IPayBoardEvent)
  private
    FPayUUID, FPayCode: string;
    FAmount: currency;
  public
    constructor Create(ASource: TObject; const APayUUID, APayCode: string; AAmount: currency);
    function GetPayUUID: string;
    function GetPayCode: string;
    function GetAmount: currency;
  end;

const
  ServiceCode: string = 'PayBoard';

var
  PayForm: TPayForm;

implementation

uses cm_sysutils, uSystem;

{$R *.frm}

{ TPayBoardEvent }

constructor TPayBoardEvent.Create(ASource: TObject; const APayUUID, APayCode: string; AAmount: currency);
begin
  inherited Create(ASource);
  FPayUUID := APayUUID;
  FPayCode := APayCode;
  FAmount := AAmount;
end;

function TPayBoardEvent.GetPayUUID: string;
begin
  Result := FPayUUID;
end;

function TPayBoardEvent.GetPayCode: string;
begin
  Result := FPayCode;
end;

function TPayBoardEvent.GetAmount: currency;
begin
  Result := FAmount;
end;

{ TPayForm }

procedure TPayForm.Panel1Click(Sender: TObject);
begin
  FPayCode := IntToStr(TPanel(Sender).Tag);
  FListener.Changed(TPayBoardEvent.Create(Self, FPayUUID, FPayCode, FAmount));
end;

function TPayForm.GetServiceCode: string;
begin
  Result := ServiceCode;
end;

procedure TPayForm.PromptMessage(AEventType: TEventType; const AMsg: string);
begin
  FMsgBar.ShowMessage(AEventType, AMsg);
end;

procedure TPayForm.SetPayView(AVO: TPayView);
var
  i: integer;
  sum: currency;
begin
  Memo1.Clear;
  labNeedAmount.Caption := DefFmtCurrStr(AVO.OrderAmount);
  sum := 0;
  for i := 0 to AVO.PaidInfoList.Count - 1 do
  begin
    sum := sum + AVO.PaidInfoList[i].PayAmount;
    Memo1.Append(Format('%s %.2f', [AVO.PaidInfoList[i].PayName, AVO.PaidInfoList[i].PayAmount]));
  end;
  Label15.Caption := DefFmtCurrStr(AVO.OrderAmount - sum);
end;

procedure TPayForm.StartPay(const APayUUID: string);
begin
  FPayUUID := APayUUID;
  //FAmount := AAmount;
  //Result := True;
  Self.BoundsRect := AppSystem.GetServiceRect;
  ShowModal;
end;

procedure TPayForm.StopPay;
begin
  Close;
end;

procedure TPayForm.FormCreate(Sender: TObject);
begin
  Self.BorderStyle := bsNone;
  FMsgBar := TCMMsgBar.Create(Self);
  InterfaceRegister.PutInterface(IPayBoard, PayForm);
end;

procedure TPayForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    if AppSystem.GetMsgBox().MessageBox('确定要退出支付吗?', '温馨提示', 4{MB_YESNO}) = 6{IDYES} then
      Close;
  end;
  FMsgBar.Hide;
end;

procedure TPayForm.FormShow(Sender: TObject);
const
  PayDefault = 'pay.defPayCode';
var
  defPayCode: string;
begin
  //if assigned(FListener) then
  //  if not AppSystem.GetParameter.Get(PayDefault).IsNull then
  //    if AppSystem.GetParameter.Get(PayDefault).AsString = EmptyStr then
  //    begin
  //      defPayCode := AppSystem.GetParameter.Get(PayDefault).AsString;
  //      FListener.Changed(TPayBoardEvent.Create(Self, FPayUUID, defPayCode, FAmount));
  //    end;
end;

procedure TPayForm.edtRealAmountKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  realAmount, needAmount: currency;
begin
  //needAmount := strtocurrdef(labNeedAmount.Caption, 0, CMFormatSettings);
  //if ((realAmount - needAmount) > 100) then
  //  FMsgBar.ShowError('找零金额不能大于100元');
end;

procedure TPayForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FListener.Closed(TPayBoardEvent.Create(Self, FPayUUID, FPayCode, FAmount));
end;

procedure TPayForm.SetListener(AListener: IPayBoardListener);
begin
  FListener := AListener;
end;

end.


