unit uPOSPayCash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager, cm_sysutils, cm_Plat,
  uPay, uPayUtils, uSystem,
  uDAO, uCashPayPO, uCashPayDAO, uCashPayDAOImpl;

type

  { TPOSCashPay }

  TPOSCashPay = class(TCMMessageable, IPayService)
  private
    FPayRequestCashDAO: ICashPayRecordDAO;
  public
    constructor Create;
  public
    function GetName: string;
    function ToPay(ARequest: IPayServiceRequest; out theResponseList: TPayServiceResponseList): boolean;
    function ToQueryPay(const AServicePayUUID: string): IPayServiceResponse;
  end;

const
  CashPayName: string = 'CashPay';

implementation

{ TPOSCashPay }

constructor TPOSCashPay.Create;
begin
  FPayRequestCashDAO := TPOSDAOFactory.GetInstance.GetDAOObject(TCashPayRecordDAO);
end;

function TPOSCashPay.GetName: string;
begin
  Result := CashPayName;
end;

//TODO
function TPOSCashPay.ToPay(ARequest: IPayServiceRequest; out theResponseList: TPayServiceResponseList): boolean;
var
  servicePayUUID, s: string;
  payType: byte;
  payAmount, realAmount, returnAmount: currency;
  rspObj, rspObj2: TPayServiceResponse;
  PayBoard: IPayBoard;

  APayRecord: TPayRecordCash;
begin
  Result := False;

  payAmount := ARequest.GetPayAmount;
  Messager.Debug('CashPay: %.2f...', [payAmount]);


  APayRecord := TPayRecordCash.Create;
  APayRecord.UUID := CreateGUIDStr;
  APayRecord.AssignUUID:= ARequest.GetPayUUID;
  APayRecord.Amount:=ARequest.GetPayAmount;
  if not FPayRequestCashDAO.Save(APayRecord) then
  begin
      Messager.Debug('xxxx...', [payAmount]);

    Exit;
  end;

  s := AppSystem.GetMsgBox.InputBox('实收现金', Format('NeedAmount: %.2f', [payAmount]), DefFmtCurrStr(payAmount));

  Messager.Debug('CashPay: s:%s...', [s]);
  if not TryStrToCurr(s, realAmount, CMFormatSettings) then
  begin
    if InterfaceRegister.OutInterface(IPayBoard, PayBoard) then
      PayBoard.PromptMessage(etError, '输入金额不正确.');
    rspObj.PayAmount := 0;
    rspObj.PayRemark := '输入金额不正确.';
    Exit;
  end;
  Messager.Debug('1111111111111');
  if (realAmount = 0) then
  begin
    if InterfaceRegister.OutInterface(IPayBoard, PayBoard) then
      PayBoard.PromptMessage(etError, '实收金额不能为零.');
    rspObj.PayAmount := 0;
    rspObj.PayRemark := '实收金额不能为零.';
    Exit;
  end;
  Messager.Debug('2222222222');
  returnAmount := realAmount - payAmount;
  if (returnAmount > 100) then
  begin
    if InterfaceRegister.OutInterface(IPayBoard, PayBoard) then
      PayBoard.PromptMessage(etError, '找零金额不可以大于100.');
    rspObj.PayAmount := 0;
    rspObj.PayRemark := '找零金额不可以大于100.';
    Exit;
  end;
  Messager.Debug('33333333');
  payType := 1;
  servicePayUUID := CreateGUIDStr;
  rspObj := TPayServiceResponse.Create(servicePayUUID, payType, realAmount);

  if (returnAmount > 0) then
  begin
    payType := 1;
    servicePayUUID := CreateGUIDStr;
    rspObj2 := TPayServiceResponse.Create(servicePayUUID, payType, -returnAmount);
  end;
  Messager.Debug('44444');
  theResponseList := TPayServiceResponseList.Create;
  Messager.Debug('44444a');
  theResponseList.Add(rspObj);
  if (returnAmount > 0) then
    theResponseList.Add(rspObj2);
  Messager.Debug('44444b');
  Result := True;
  Messager.Debug('5555');
end;

function TPOSCashPay.ToQueryPay(const AServicePayUUID: string): IPayServiceResponse;
begin
  Result := nil;
end;

end.

