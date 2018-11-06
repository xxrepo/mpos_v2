unit uPayPO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  cm_messager;

type

  { TPayPO }

  TPayRecordPO = class(TPersistent)
  private
    FAmount: currency;
    FOrderUUID: string;
    FPayUUID: string;
    FRemark: string;
    FTime: TDatetime;
  public
    property PayUUID: string read FPayUUID write FPayUUID;
    property OrderUUID: string read FOrderUUID write FOrderUUID;
    property Amount: currency read FAmount write FAmount;
    property Time_: TDatetime read FTime write FTime;
    property Remark: string read FRemark write FRemark;
  end;

  TPayRecordPOList = class(TThreadList<TPayRecordPO>);

  { TPayRecordPO }

  TPayAssignPO = class(TPersistent)
  private
    FAmount: currency;
    FPayUUID: string;
    FAssignUUID: string;
    FRemark: string;
    FServiceCode: string;
    FState: byte;
    FTime: TDatetime;
  public
    property PayUUID: string read FPayUUID write FPayUUID;
    property AssignUUID: string read FAssignUUID write FAssignUUID;
    property ServiceCode: string read FServiceCode write FServiceCode;
    property Amount: currency read FAmount write FAmount;
    property State: byte read FState write FState;
    property Time_: TDatetime read FTime write FTime;
    property Remark: string read FRemark write FRemark;
  end;

  TPayAssignPOList = class(TThreadList<TPayAssignPO>);

  { TPayInfoPO }

  TPayInfoPO = class(TPersistent)
  private
    FAmount: currency;
    FAssignUUID: string;
    FRemark: string;
    FServiceCode: string;
    FTime: TDatetime;
    FType: byte;
  public
    property AssignUUID: string read FAssignUUID write FAssignUUID;
    property ServiceUUID: string read FServiceCode write FServiceCode;
    property Type_: byte read FType write FType;
    property Amount: currency read FAmount write FAmount;
    property Time_: TDatetime read FTime write FTime;
    property Remark: string read FRemark write FRemark;
  end;

  TPayInfoPOList = class(TThreadList<TPayInfoPO>);

  {TPayTypePO}
  TPayTypePO = class(TPersistent)
  private
    FAllowRefund: boolean;
    FCaption: string;
    FCode: string;
    FConfigInfo: string;
    FIsDefault: boolean;
    FIsEnabled: boolean;
    FIsSingle: boolean;
    FName: string;
    FRemark: string;
    FSeqNo: integer;
  public
    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
    property Caption: string read FCaption write FCaption;
    property SeqNo: integer read FSeqNo write FSeqNo;
    property IsEnabled: boolean read FIsEnabled write FIsEnabled;
    property IsSingle: boolean read FIsSingle write FIsSingle;
    property IsDefault: boolean read FIsDefault write FIsDefault;
    property AllowRefund: boolean read FAllowRefund write FAllowRefund;
    property ConfigInfo: string read FConfigInfo write FConfigInfo;
    property Remark: string read FRemark write FRemark;
  end;

  {TPayTypePO}
  //TPayTypePO = class(TPersistent, IPayTypeInfo)
  //private
  //  FAllowRefund: boolean;
  //  FCaption: string;
  //  FCode: string;
  //  FConfigInfo: string;
  //  FIsDefault: boolean;
  //  FIsEnabled: boolean;
  //  FIsSingle: boolean;
  //  FName: string;
  //  FRemark: string;
  //  FSeqNo: integer;
  //public
  //  property Code: string read FCode write FCode;
  //  property Name: string read FName write FName;
  //  property Caption: string read FCaption write FCaption;
  //  property SeqNo: integer read FSeqNo write FSeqNo;
  //  property IsEnabled: boolean read FIsEnabled write FIsEnabled;
  //  property IsSingle: boolean read FIsSingle write FIsSingle;
  //  property IsDefault: boolean read FIsDefault write FIsDefault;
  //  property AllowRefund: boolean read FAllowRefund write FAllowRefund;
  //  property ConfigInfo: string read FConfigInfo write FConfigInfo;
  //  property Remark: string read FRemark write FRemark;
  //end;

  TPayTypePOList = class(TThreadList<TPayTypePO>);




implementation


end.
