unit uSaleOrderServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, cm_messager, uSaleOrder, uFormService, uSaleOrderService,
  uRetailServiceForm, cm_interfaces, uRetailService,
  uStoreSaleMainDAO,
  uStoreSaleMainDAOImpl,
  uStoreSaleSubDAO,
  uStoreSaleSubDAOImpl,
  uStoreSalePayDAO,
  uStoreSalePayDAOImpl,
  uStoreSaleInvDAO,
  uStoreSaleInvDAOImpl,
  uStoreSalePromotionDAO,
  uStoreSalePromotionDAOImpl,
  uStoreMemberOrderDAO,
  uStoreMemberOrderDAOImpl,
  uShopProductDAO,
  uShopProductDAOImpl,
  uDAO;

type

  { TRetailService }

  { TSaleOrderService }

  TSaleOrderService = class(TCMMessageableComponent, ISaleOrderService)
  private
  public
  public
    //----- ISaleOrderService Impl ----------------------------------
    function Add(): TSaleOrder;
    function Get(GUID: string): TSaleOrder;
    function SQL(ASaleOrder: TSaleOrder): string;
    function Save(ASaleOrder: TSaleOrder): boolean;
  end;

implementation

uses
  uServiceFactory;



{ TSaleOrderService }

function TSaleOrderService.Add(): TSaleOrder;
begin
  Result := nil;
  try
    Result := TSaleOrder.Create(nil);
  except
    on e: Exception do
      Messager.Error('Add: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TSaleOrderService.Get(GUID: string): TSaleOrder;
var
  AMaster: TSaleMaster = nil;
  ADetails: TSaleSubs = nil;
  //TSaleMaster
begin
  Result := nil;
  try
    AMaster := ServiceFactory.GetSaleMasterService().Get(GUID);
    if not Assigned(AMaster) then
    begin
      Messager.Error('AMaster不存在');
      Exit;
    end;

    ADetails := ServiceFactory.GetSaleSubService().GetByParentGUID(AMaster.GUID);




    Result := TSaleOrder.Create(nil, AMaster, ADetails, nil, nil, nil);

  except
    on e: Exception do
      Messager.Error('Add: %s %s', [e.ClassName, e.Message]);
  end;

end;

function TSaleOrderService.SQL(ASaleOrder: TSaleOrder): string;
begin

end;

function TSaleOrderService.Save(ASaleOrder: TSaleOrder): boolean;
begin

end;

end.





