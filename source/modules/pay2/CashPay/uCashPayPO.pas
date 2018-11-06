unit uCashPayPO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TPayRecordCash }

  TPayRecordCash = class(TPersistent)
  private
    FAmount: currency;
    FAssignUUID: string;
    FRemark: string;
    FTime: TDateTime;
    FUUID: string;
  published
    property UUID: string read FUUID write FUUID;//生成
    property AssignUUID: string read FAssignUUID write FAssignUUID;
    property Amount: currency read FAmount write FAmount;
    property Time_: TDateTime read FTime write FTime;
    property Remark: string read FRemark write FRemark;
  end;

  TPayInfoCash = class(TCMMessageable)
  public
    ServiceUUID: string;//支付结果标识ID
    UUID: string;//同上一致
    PayType: (Type_Pay, Type_Odd);
    Amount: currency;
    Time: TDateTime;
    Remark: string;
  end;


implementation

{ TPayRecordCash }


end.

