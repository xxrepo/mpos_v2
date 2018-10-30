unit uCashPay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager, cm_sysutils,
  uPay, uPayUtils;

type

  { TPOSCashPay }

  TPOSCashPay = class(TCMMessageable, IPayService)
  public
    function GetName: string;
    function ToPay(ARequest: IPayServiceRequest; out theResponseList: TPayServiceResponseList): boolean;
    function ToQueryPay(const AServicePayUUID: string): IPayServiceResponse;
  end;

const
  CashPayName: string = 'CashPay';

implementation

{ TPOSCashPay }

function TPOSCashPay.GetName: string;
begin
  Result := CashPayName;
end;

//TODO
function TPOSCashPay.ToPay(ARequest: IPayServiceRequest; out theResponseList: TPayServiceResponseList): boolean;
var
  servicePayUUID, s: string;
  payType: byte;
  payAmount, RealAmount: currency;
  rspObj, rspObj2: TPayServiceResponse;
begin
  Result := False;
  //test
  Messager.Debug('CashPay: %.2f...', [ARequest.GetPayAmount]);


  //TODO pay deal

  //s := AppSystem.GetMsgBox.InputBox('实收现金', 'RealAmount:', DefFmtCurrStr(ARequest.GetPayAmount));
  //RealAmt := TryStrToCurr(s, theAmount, CMFormatSettings);

  payType := 1;
  payAmount := ARequest.GetPayAmount;

  servicePayUUID := CreateGUIDStr;
  rspObj := TPayServiceResponse.Create(servicePayUUID, payType, payAmount - 100);

  servicePayUUID := CreateGUIDStr;
  rspObj2 := TPayServiceResponse.Create(servicePayUUID, payType, 100);
  Messager.Debug('2222');
  //test
  if ARequest.GetPayAmount > 100 then
  begin
    rspObj.PayAmount := 0;
    rspObj.PayRemark := '不能支付100元以上的,OK?';
    Exit;
  end;
  Messager.Debug('333');
  theResponseList := TPayServiceResponseList.Create;
  theResponseList.Add(rspObj);
  theResponseList.Add(rspObj2);
  Messager.Debug('4444');
  Result := True;
end;

function TPOSCashPay.ToQueryPay(const AServicePayUUID: string): IPayServiceResponse;
begin
  Result := nil;
end;

end.

