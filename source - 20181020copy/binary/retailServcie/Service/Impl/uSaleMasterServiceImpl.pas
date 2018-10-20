unit uSaleMasterServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_Messager, uSaleOrder,
  uStoreSaleOrder, uSaleMasterService,
  uStoreSaleMainDAO, uStoreSaleMainDAOImpl,
  uStoreSaleSubDAO, uStoreSaleSubDAOImpl,
  uStoreSalePayDAO, uStoreSalePayDAOImpl,
  uStoreSaleInvDAO, uStoreSaleInvDAOImpl,
  uStoreSalePromotionDAO, uStoreSalePromotionDAOImpl,
  uStoreMemberOrderDAO, uStoreMemberOrderDAOImpl,
  uShopProductDAO, uShopProductDAOImpl,
  uDAO;

type

  { TSaleMasterService }

  TSaleMasterService = class(TCMMessageableComponent, ISaleMasterService)
  private
    function DoConvertInstance(const ASaleMain: TStoreSaleMain; out ASaleMaster: TSaleMaster): boolean; overload;
    function DoConvertInstance(const ASaleMaster: TSaleMaster; out ASaleMain: TStoreSaleMain): boolean; overload;
  public
    function Add(): TSaleMaster;
    function Get(GUID: string): TSaleMaster;
    function SQL(ASaleOrder: TSaleMaster): string;
    function Save(ASaleOrder: TSaleMaster): boolean;
  end;


implementation

{ TSaleMasterService }

function TSaleMasterService.DoConvertInstance(const ASaleMain: TStoreSaleMain; out ASaleMaster: TSaleMaster): boolean;
begin

  Result := False;
  try
    ASaleMaster := TSaleMaster.Create();

    ASaleMaster.GUID := ASaleMain._UUID;
    ASaleMaster.Code := ASaleMain._FlowNo;
    ASaleMaster.Date := ASaleMain._FilDate;
    ASaleMaster.PosNo := ASaleMain._PosNo;
    ASaleMaster.ShopCode := ASaleMain._ShopCode;
    ASaleMaster.CompanyCode := ASaleMain._CompanyCode;
    ASaleMaster.OrderType := ASaleMain._OrderType;             //类型  0 销售单 1 冲单 2 提货单 3 凭码兑奖-- 11 外卖订单
    ASaleMaster.SrcGUID := ASaleMain._SRCUUID;

    //会员相关信息
    ASaleMaster.Score := ASaleMain._Score;
    ASaleMaster.MemberId := ASaleMain._MemberID;
    ASaleMaster.DiscountID := ASaleMain._DiscountID;//优惠信息唯一编码 {会员价专用}
    //优惠券信息
    ASaleMaster.CardCode := ASaleMain._CardCode;
    ASaleMaster.CardNo := ASaleMain._CardNo;
    ASaleMaster.CardType := ASaleMain._CardType;
    ASaleMaster.CardNeedDestory := ASaleMain._isDestory;
    //汇总数据
    ASaleMaster.TotalAmt := ASaleMain._Total;                          //订单总金额（不含折扣）
    ASaleMaster.TotalQty := ASaleMain._TotalQty;                       //汇总数量
    ASaleMaster.RealAmt := ASaleMain._RealAmt;                         //应收金额 ＝ 订单总金额－折扣金额
    ASaleMaster.RecCount := ASaleMain._RECCNT;                         //记录数
    ASaleMaster.DiscountAmount := ASaleMain._DiscountAmount;           //折扣金额
    ASaleMaster.GiveZeroAmt := ASaleMain._GiveZeroAmt;                   //找零
    ASaleMaster.CustPayCash := ASaleMain._CustPayCash;                   //用户实付金额
    ASaleMaster.PayedSum := ASaleMain._PayedSum;                         //已支付金额
    ASaleMaster.FixDiscAmt := ASaleMain._FixDiscAmt;                     //实际优惠金额
    ASaleMaster.FixRealAmt := ASaleMain._FixRealAmt;                     //实际应收金额

    ASaleMaster.Casher := ASaleMain._CASHIER;
    ASaleMaster.CanModify := True;
    Result := True;
  except
    on e: Exception do
      Messager.Error('TSaleMasterService.DoConvertInstance: %s %s', [e.ClassName, e.Message]);
  end;

end;

function TSaleMasterService.DoConvertInstance(const ASaleMaster: TSaleMaster; out ASaleMain: TStoreSaleMain): boolean;
begin

  Result := False;
  try
    ASaleMain := TStoreSaleMain.Create();

    ASaleMain._UUID := ASaleMaster.GUID;
    ASaleMain._FlowNo := ASaleMaster.Code;
    ASaleMain._FilDate := ASaleMaster.Date;
    ASaleMain._PosNo := ASaleMaster.PosNo;
    ASaleMain._ShopCode := ASaleMaster.ShopCode;
    ASaleMain._CompanyCode := ASaleMaster.CompanyCode;
    ASaleMain._OrderType := ASaleMaster.OrderType;             //类型  0 销售单 1 冲单 2 提货单 3 凭码兑奖-- 11 外卖订单
    ASaleMain._SRCUUID := ASaleMaster.SrcGUID;

    //会员相关信息
    ASaleMain._Score := ASaleMaster.Score;
    ASaleMain._MemberID := ASaleMaster.MemberId;
    ASaleMain._DiscountID := ASaleMaster.DiscountID;//优惠信息唯一编码 {会员价专用}
    //优惠券信息
    ASaleMain._CardCode := ASaleMaster.CardCode;
    ASaleMain._CardNo := ASaleMaster.CardNo;
    ASaleMain._CardType := ASaleMaster.CardType;
    ASaleMain._isDestory := ASaleMaster.CardNeedDestory;
    //汇总数据
    ASaleMain._Total := ASaleMaster.TotalAmt;           //订单总金额（不含折扣）
    ASaleMain._TotalQty := ASaleMaster.TotalQty;//汇总数量
    ASaleMain._RealAmt := ASaleMaster.RealAmt;         //应收金额 ＝ 订单总金额－折扣金额
    ASaleMain._RECCNT := ASaleMaster.RecCount;          //记录数
    ASaleMain._DiscountAmount := ASaleMaster.DiscountAmount;  //折扣金额
    ASaleMain._GiveZeroAmt := ASaleMaster.GiveZeroAmt;     //找零
    ASaleMain._CustPayCash := ASaleMaster.CustPayCash;     //用户实付金额
    ASaleMain._PayedSum := ASaleMaster.PayedSum;       //已支付金额
    ASaleMain._FixDiscAmt := ASaleMaster.FixDiscAmt; //实际优惠金额
    ASaleMain._FixRealAmt := ASaleMaster.FixRealAmt; //实际应收金额

    ASaleMain._CASHIER := ASaleMaster.Casher;
    ASaleMain._BoSaleLock := ASaleMaster.CanModify;
    Result := True;
  except
    on e: Exception do
      Messager.Error('TSaleMasterService.DoConvertInstance: %s %s', [e.ClassName, e.Message]);
  end;

end;

function TSaleMasterService.Add(): TSaleMaster;
begin

end;

function TSaleMasterService.Get(GUID: string): TSaleMaster;
var
  ASaleMaster: TSaleMaster = nil;
  ASaleMain: TStoreSaleMain = nil;
  ASaleSubs: TSaleSubs = nil;
begin
  Result := nil;
  try
    try
      ASaleMain := TStoreSaleMainDAO(TPOSDAOFactory.GetInstance.GetDAOObject(TStoreSaleMainDAO)).Get(GUID);
      if not (Self.DoConvertInstance(ASaleMain, ASaleMaster)) then
      begin
        Messager.Info('PO转BO失败, 对象不存在');
        Exit;
      end;

      Result := ASaleMaster;
    except
      on e: Exception do
      begin
        if Assigned(ASaleMaster) then
          ASaleMaster.Free;
        Messager.Error('TShopProductDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
      end;
    end;

  finally
    if Assigned(ASaleMain) then
      ASaleMain.Free;
  end;
end;

function TSaleMasterService.SQL(ASaleOrder: TSaleMaster): string;
begin

end;

function TSaleMasterService.Save(ASaleOrder: TSaleMaster): boolean;
begin

end;

end.
