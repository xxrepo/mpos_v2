unit uMemberOrderServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_Messager, uSaleOrder, uStoreSaleOrder, uMemberOrderService,
  uStoreSaleMainDAO, uStoreSaleMainDAOImpl,
  uStoreSaleSubDAO, uStoreSaleSubDAOImpl,
  uStoreSalePayDAO, uStoreSalePayDAOImpl,
  uStoreSaleInvDAO, uStoreSaleInvDAOImpl,
  uStoreSalePromotionDAO, uStoreSalePromotionDAOImpl,
  uStoreMemberOrderDAO, uStoreMemberOrderDAOImpl,
  uShopProductDAO, uShopProductDAOImpl,
  uDAO;

type

  { TSaleSubService }

  TMemberOrderService = class(TCMMessageableComponent, IMemberOrderService)
  private
    function DoConvertInstance(const AStoreMemberOrder: TStoreMemberOrder; out AMemberOrder: TMemberOrder): boolean; overload;
    function DoConvertInstance(const AMemberOrder: TMemberOrder; out AStoreMemberOrder: TStoreMemberOrder): boolean; overload;
  public
    function Add(): TMemberOrder;
    function Get(GUID: string): TMemberOrder;
    function GetByParentGUID(AGUID: string): TMemberOrders;
    function SQL(Instance: TMemberOrder): string;
    function Save(Instance: TMemberOrder): boolean;
  end;



implementation

uses
  uServiceFactory;

{ TMemberOrderService }

function TMemberOrderService.DoConvertInstance(const AStoreMemberOrder: TStoreMemberOrder; out AMemberOrder: TMemberOrder): boolean;
begin
  Result := False;
  try
    AMemberOrder := TMemberOrder.Create();

    //AMemberOrder.GUID := AStoreMemberOrder._GUID;
    //AMemberOrder.ParentGUID := AStoreMemberOrder._ParentGUID;
    //AMemberOrder.ItemNo := AStoreMemberOrder._ITEMNO;
    //AMemberOrder.SrcItemNo := AStoreMemberOrder._SRCITEMNO;
    //AMemberOrder.GID := AStoreMemberOrder._GID;
    //AMemberOrder.GDCODE := AStoreMemberOrder._GDCODE;
    //AMemberOrder.Quantity := AStoreMemberOrder._QTY;
    //AMemberOrder.Price := AStoreMemberOrder._Price;
    //AMemberOrder.ProdPrice := AStoreMemberOrder._ProdPrice;
    //AMemberOrder.AvgPrice := AStoreMemberOrder._AvgPrice;
    //AMemberOrder.InputCode := AStoreMemberOrder._InputBarCode;
    //AMemberOrder.IsChengePrice := AStoreMemberOrder._IsChangePrice;

    //if not (AMemberOrder.GDCODE = '') then
    //  AMemberOrder.Product := TShopProductDAO(TPOSDAOFactory.GetInstance.GetDAOObject(TShopProductDAO)).GetByGDCode(AMemberOrder.GDCODE);

    //AMemberOrder.InnerPackings := ServiceFactory.get;


    Result := True;
  except
    on e: Exception do
      Messager.Error('TSaleMasterService.DoConvertInstance: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TMemberOrderService.DoConvertInstance(const AMemberOrder: TMemberOrder; out AStoreMemberOrder: TStoreMemberOrder): boolean;
begin

end;

function TMemberOrderService.Add(): TMemberOrder;
begin

end;

function TMemberOrderService.Get(GUID: string): TMemberOrder;
begin
  Result := nil;
  try

  except
    on e: Exception do
    begin
      Messager.Error('TShopProductDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
    end;
  end;
end;

function TMemberOrderService.GetByParentGUID(AGUID: string): TMemberOrders;
var
  i: integer;
  AMemberOrder: TMemberOrder = nil;
  AMemberOrders: TMemberOrders = nil;
  AStoreMemberOrders: TStoreMemberOrders = nil;
begin
  Result := nil;
  try
    try
      AMemberOrders := TMemberOrders.Create();
      AStoreMemberOrders := TStoreMemberOrderDAO(TPOSDAOFactory.GetInstance.GetDAOObject(TStoreMemberOrderDAO)).GetByParentGUID(AGUID);
      for i := 0 to AStoreMemberOrders.Count - 1 do
        if DoConvertInstance(AStoreMemberOrders[i], AMemberOrder) then
          AMemberOrders.Add(AMemberOrder);
      Result := AMemberOrders;
    except
      on e: Exception do
      begin
        if Assigned(AMemberOrders) then
          FreeAndNil(AMemberOrders);
        Messager.Error('TShopProductDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
      end;
    end;
  finally
    if Assigned(AStoreMemberOrders) then
      FreeAndNil(AStoreMemberOrders);
  end;
end;

function TMemberOrderService.SQL(Instance: TMemberOrder): string;
begin

end;

function TMemberOrderService.Save(Instance: TMemberOrder): boolean;
begin

end;

end.
