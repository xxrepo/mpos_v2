unit uSalePayServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_Messager, uSaleOrder, uStoreSaleOrder, uSalePayService,
  //uStoreSaleMainDAO, uStoreSaleMainDAOImpl,
  //uStoreSaleSubDAO, uStoreSaleSubDAOImpl,
  //uStoreSalePayDAO, uStoreSalePayDAOImpl,
  //uStoreSaleInvDAO, uStoreSaleInvDAOImpl,
  //uStoreSalePromotionDAO, uStoreSalePromotionDAOImpl,
  //uStoreMemberOrderDAO, uStoreMemberOrderDAOImpl,
  //uShopProductDAO, uShopProductDAOImpl,
  uDAO;

type

  { TSalePayService }

  TSalePayService = class(TCMMessageableComponent, ISalePayService)
  private
    function DoConvertInstance(const AStoreSalePay: TStoreSalePay; out ASalePay: TSalePay): boolean; overload;
    function DoConvertInstance(const ASalePay: TSalePay; out AStoreSalePay: TStoreSalePay): boolean; overload;
  public
    function Add(): TSalePay;
    function Get(GUID: string): TSalePay;
    function GetByParentGUID(AGUID: string): TSalePays;
    function SQL(ASaleOrder: TSalePay): string;
    function Save(ASaleOrder: TSalePay): boolean;
  end;



implementation

uses
  uServiceFactory;

{ TSalePayService }

function TSalePayService.DoConvertInstance(const AStoreSalePay: TStoreSalePay; out ASalePay: TSalePay): boolean;
begin
  Result := False;
  try
    ASalePay := TSalePay.Create();

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

function TSalePayService.DoConvertInstance(const ASalePay: TSalePay; out AStoreSalePay: TStoreSalePay): boolean;
begin

end;

function TSalePayService.Add(): TSalePay;
begin

end;

function TSalePayService.Get(GUID: string): TSalePay;
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

function TSalePayService.GetByParentGUID(AGUID: string): TSalePays;
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

function TSalePayService.SQL(ASaleOrder: TSalePay): string;
begin

end;

function TSalePayService.Save(ASaleOrder: TSalePay): boolean;
begin

end;

end.
