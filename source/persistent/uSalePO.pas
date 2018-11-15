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
    FGID: integer;
    FCommodityName: string;
    FSettlementPrice: currency;
    FQuantity: currency;
    FSettlementAmount: currency;
    FRemark: string;
  published
    property UUID: string read FUUID write FUUID;
    property OrderUUID: string read FOrderUUID write FOrderUUID;
    property GID: integer read FGID write FGID;
    property CommodityName: string read FCommodityName write FCommodityName;
    property SettlementPrice: currency read FSettlementPrice write FSettlementPrice; //实际结算价
    property Quantity: currency read FQuantity write FQuantity;
    property SettlementAmount: currency read FSettlementAmount write FSettlementAmount; //实际结算金额 应对不可数商品、舍数等
    property Remark: string read FRemark write FRemark;
  end;

  TSaleDetailList = TObjectList<TSaleDetail>;

  TSaleOrder = class(TPersistent)
  private
    FUUID: string;
    FType: byte; //单据类型0: 销售单, 1: 冲退单, 2: 提货单; 3 凭码兑奖; 11:外卖订单
    FSumSettlementAmount: currency;
    FSellerID: string;
    FSettlementTime: TDateTime;
    FRemark: string;
  published
    property UUID: string read FUUID write FUUID;
    property SumSettlementAmount: currency read FSumSettlementAmount write FSumSettlementAmount;
    //实际结算总金额 应对不可数商品、舍数等
    property SellerID: string read FSellerID write FSellerID;
    property SettlementTime: TDateTime read FSettlementTime write FSettlementTime; //结算时间
    property Type_: byte read FType write FType;
    property Remark: string read FRemark write FRemark;
  end;


  TSaleOrderList = TObjectList<TSaleOrder>;



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
