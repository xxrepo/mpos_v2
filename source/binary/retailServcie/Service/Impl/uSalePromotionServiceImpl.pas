unit uSalePromotionServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_Messager, uSaleOrder, uStoreSaleOrder, uSalePromotionService,
  //uStoreSaleMainDAO, uStoreSaleMainDAOImpl,
  //uStoreSaleSubDAO, uStoreSaleSubDAOImpl,
  //uStoreSalePayDAO, uStoreSalePayDAOImpl,
  //uStoreSaleInvDAO, uStoreSaleInvDAOImpl,
  //uStoreSalePromotionDAO, uStoreSalePromotionDAOImpl,
  //uStoreMemberOrderDAO, uStoreMemberOrderDAOImpl,
  //uShopProductDAO, uShopProductDAOImpl,
  uDAO;

type

  { TSalePromotionService }

  TSalePromotionService = class(TCMMessageableComponent, ISalePromotionService)
  private
    function DoConvertInstance(const AStoreSalePromotion: TStoreSalePromotion; out ASalePromotion: TSalePromotion): boolean; overload;
    function DoConvertInstance(const ASalePromotion: TSalePromotion; out AStoreSalePromotion: TStoreSalePromotion): boolean; overload;
  public
    function Add(): TSalePromotion;
    function Get(GUID: string): TSalePromotion;
    function GetByParentGUID(AGUID: string): TSalePromotions;
    function SQL(ASaleOrder: TSalePromotion): string;
    function Save(ASaleOrder: TSalePromotion): boolean;
  end;



implementation

uses
  uServiceFactory;

{ TSalePromotionService }

function TSalePromotionService.DoConvertInstance(const AStoreSalePromotion: TStoreSalePromotion; out ASalePromotion: TSalePromotion): boolean;
begin
  Result := False;
  try
    ASalePromotion := TSalePromotion.Create();

    //ASaleSub.GUID := AStoreSaleSub._GUID;
    //ASaleSub.ParentGUID := AStoreSaleSub._ParentGUID;
    //ASaleSub.ItemNo := AStoreSaleSub._ITEMNO;
    //ASaleSub.SrcItemNo := AStoreSaleSub._SRCITEMNO;
    //ASaleSub.GID := AStoreSaleSub._GID;
    //ASaleSub.GDCODE := AStoreSaleSub._GDCODE;
    //ASaleSub.Quantity := AStoreSaleSub._QTY;
    //ASaleSub.Price := AStoreSaleSub._Price;
    //ASaleSub.ProdPrice := AStoreSaleSub._ProdPrice;
    //ASaleSub.AvgPrice := AStoreSaleSub._AvgPrice;
    //ASaleSub.InputCode := AStoreSaleSub._InputBarCode;
    //ASaleSub.IsChengePrice := AStoreSaleSub._IsChangePrice;

    //if not (ASaleSub.GDCODE = '') then
    //  ASaleSub.Product := TShopProductDAO(TPOSDAOFactory.GetInstance.GetDAOObject(TShopProductDAO)).GetByGDCode(ASaleSub.GDCODE);

    //ASaleSub.InnerPackings := ServiceFactory.get;

    Result := True;
  except
    on e: Exception do
      Messager.Error('DoConvertInstance: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TSalePromotionService.DoConvertInstance(const ASalePromotion: TSalePromotion; out AStoreSalePromotion: TStoreSalePromotion): boolean;
begin

end;

function TSalePromotionService.Add(): TSalePromotion;
begin

end;

function TSalePromotionService.Get(GUID: string): TSalePromotion;
begin
  Result := nil;
  try

  except
    on e: Exception do
    begin
      Messager.Error('Get: %s %s', [e.ClassName, e.Message]);
    end;
  end;
end;

function TSalePromotionService.GetByParentGUID(AGUID: string): TSalePromotions;
var
  i: integer;
  ASaleSub: TSaleSub = nil;
  ASaleSubs: TSaleSubs = nil;
  AStoreSaleSubs: TStoreSaleSubs = nil;
begin
  Result := nil;
  //try
  //  try
  //    ASaleSubs := TSaleSubs.Create();
  //    AStoreSaleSubs := TStoreSaleSubDAO(TPOSDAOFactory.GetInstance.GetDAOObject(TStoreSaleSubDAO)).GetByParentGUID(AGUID);
  //    for i := 0 to AStoreSaleSubs.Count - 1 do
  //      if DoConvertInstance(AStoreSaleSubs[i], ASaleSub) then
  //        ASaleSubs.Add(ASaleSub);
  //    Result := ASaleSubs;
  //  except
  //    on e: Exception do
  //    begin
  //      if Assigned(ASaleSubs) then
  //        FreeAndNil(ASaleSubs);
  //      Messager.Error('GetByParentGUID: %s %s', [e.ClassName, e.Message]);
  //    end;
  //  end;
  //finally
  //  if Assigned(AStoreSaleSubs) then
  //    FreeAndNil(AStoreSaleSubs);
  //end;
end;

function TSalePromotionService.SQL(ASaleOrder: TSalePromotion): string;
begin

end;

function TSalePromotionService.Save(ASaleOrder: TSalePromotion): boolean;
begin

end;

end.
