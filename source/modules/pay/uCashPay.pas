unit uCashPay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces,
  cm_messager, cm_sysutils,
  uPay, uPayUtils;

type

  { TPOSCashPay }

  TPOSCashPay = class(TCMBase, IPayService)
  public
    function GetName: string;
    function ToPay(APayServiceRequest: IPayServiceRequest; out thePayServiceResponse: IPayServiceResponse): Boolean;
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
function TPOSCashPay.ToPay(APayServiceRequest: IPayServiceRequest; out thePayServiceResponse: IPayServiceResponse): Boolean;
var
  servicePayUUID: string;
  payType: Byte;
  payAmount: Currency;
  rspObj: TPayServiceResponse;
begin
  Result := False;
  //test
  cm_messager.DefaultMessager.Info('现金支付%.2f元...', [APayServiceRequest.GetPayAmount]);
  //
  servicePayUUID := CreateGUIDStr;
  //TODO pay deal

  payType := 1;
  payAmount := APayServiceRequest.GetPayAmount;

  rspObj := TPayServiceResponse.Create(servicePayUUID, payType, payAmount);
  thePayServiceResponse := rspObj;
  //test
  if APayServiceRequest.GetPayAmount > 10 then
    begin
      rspObj.PayAmount := 0;
      rspObj.PayRemark := '不能支付10元以上的,OK?';
      Exit;
    end;
  Result := True;
end;

function TPOSCashPay.ToQueryPay(const AServicePayUUID: string): IPayServiceResponse;
begin
  Result := nil;
end;

end.

