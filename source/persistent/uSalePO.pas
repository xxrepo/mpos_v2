{

 Defining persistant object

 **********************************************************************}

unit uSalePO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  Generics.Collections;

type

  TSaleDetail = class(TPersistent)
  private
    FUUID: string;
    FOrderUUID: string;
    FGID: Integer;
    FCommodityName: string;
    FSettlementPrice: Currency;
    FQuantity: Currency;
    FSettlementAmount: Currency;
    FRemark: string;
  published
    property UUID: string read FUUID write FUUID;
    property OrderUUID: string read FOrderUUID write FOrderUUID;
    property GID: Integer read FGID write FGID;
    property CommodityName: string read FCommodityName write FCommodityName;
    property SettlementPrice: Currency read FSettlementPrice write FSettlementPrice; //实际结算价
    property Quantity: Currency read FQuantity write FQuantity;
    property SettlementAmount: Currency read FSettlementAmount write FSettlementAmount; //实际结算金额 应对不可数商品、舍数等
    property Remark: string read FRemark write FRemark;
  end;

  TSaleOrder = class(TPersistent)
  private
    FUUID: string;
    FSumSettlementAmount: Currency;
    FSellerID: string;
    FSettlementTime: TDateTime;
    FRemark: string;
  published
    property UUID: string read FUUID write FUUID;
    property SumSettlementAmount: Currency read FSumSettlementAmount write FSumSettlementAmount; //实际结算总金额 应对不可数商品、舍数等
    property SellerID: string read FSellerID write FSellerID;
    property SettlementTime: TDateTime read FSettlementTime write FSettlementTime; //结算时间
    property Remark: string read FRemark write FRemark;
  end;

  TSaleDetailList = TObjectList<TSaleDetail>;

  { TSaleOrderEx }

  TSaleOrderEx = class(TSaleOrder)
  private
    FDetails: TSaleDetailList;
  public
    constructor Create;
    destructor Destroy; override;
    property Details: TSaleDetailList read FDetails;
  end;


implementation

{ TSaleOrderEx }

constructor TSaleOrderEx.Create;
begin
  FDetails := TSaleDetailList.Create(True);
end;

destructor TSaleOrderEx.Destroy;
begin
  FDetails.Free;
  inherited Destroy;
end;

end.

