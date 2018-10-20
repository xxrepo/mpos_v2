unit uSaleSubServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_Messager, uSaleOrder, uStoreSaleOrder, uSaleSubService,
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

  TSaleSubService = class(TCMMessageableComponent, ISaleSubService)
  private
    function DoConvertInstance(const AStoreSaleSub: TStoreSaleSub; out ASaleSub: TSaleSub): boolean; overload;
    function DoConvertInstance(const ASaleSub: TSaleSub; out AStoreSaleSub: TStoreSaleSub): boolean; overload;
  public
    function Add(): TSaleSub;
    function Get(GUID: string): TSaleSub;
    function GetByParentGUID(AGUID: string): TSaleSubs;
    function SQL(Instance: TSaleSub): string;
    function Save(Instance: TSaleSub): boolean;
  end;



implementation

uses
  uServiceFactory;

{ TSaleSubService }

function TSaleSubService.DoConvertInstance(const AStoreSaleSub: TStoreSaleSub; out ASaleSub: TSaleSub): boolean;
begin
  Result := False;
  try
    ASaleSub := TSaleSub.Create();

    ASaleSub.GUID := AStoreSaleSub._GUID;
    ASaleSub.ParentGUID := AStoreSaleSub._ParentGUID;
    ASaleSub.ItemNo := AStoreSaleSub._ITEMNO;
    ASaleSub.SrcItemNo := AStoreSaleSub._SRCITEMNO;
    ASaleSub.GID := AStoreSaleSub._GID;
    ASaleSub.GDCODE := AStoreSaleSub._GDCODE;
    ASaleSub.Quantity := AStoreSaleSub._QTY;
    ASaleSub.Price := AStoreSaleSub._Price;
    ASaleSub.ProdPrice := AStoreSaleSub._ProdPrice;
    ASaleSub.AvgPrice := AStoreSaleSub._AvgPrice;
    ASaleSub.InputCode := AStoreSaleSub._InputBarCode;
    ASaleSub.IsChengePrice := AStoreSaleSub._IsChangePrice;

    if not (ASaleSub.GDCODE = '') then
      ASaleSub.Product := TShopProductDAO(TPOSDAOFactory.GetInstance.GetDAOObject(TShopProductDAO)).GetByGDCode(ASaleSub.GDCODE);

    //ASaleSub.InnerPackings := ServiceFactory.get;


    Result := True;
  except
    on e: Exception do
      Messager.Error('TSaleMasterService.DoConvertInstance: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TSaleSubService.DoConvertInstance(const ASaleSub: TSaleSub; out AStoreSaleSub: TStoreSaleSub): boolean;
begin

end;

function TSaleSubService.Add(): TSaleSub;
begin

end;

function TSaleSubService.Get(GUID: string): TSaleSub;
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

function TSaleSubService.GetByParentGUID(AGUID: string): TSaleSubs;
var
  i: integer;
  ASaleSub: TSaleSub = nil;
  ASaleSubs: TSaleSubs = nil;
  AStoreSaleSubs: TStoreSaleSubs = nil;
begin
  Result := nil;
  try
    try
      ASaleSubs := TSaleSubs.Create();
      AStoreSaleSubs := TStoreSaleSubDAO(TPOSDAOFactory.GetInstance.GetDAOObject(TStoreSaleSubDAO)).GetByParentGUID(AGUID);
      for i := 0 to AStoreSaleSubs.Count - 1 do
        if DoConvertInstance(AStoreSaleSubs[i], ASaleSub) then
          ASaleSubs.Add(ASaleSub);
      Result := ASaleSubs;
    except
      on e: Exception do
      begin
        if Assigned(ASaleSubs) then
          FreeAndNil(ASaleSubs);
        Messager.Error('TShopProductDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
      end;
    end;
  finally
    if Assigned(AStoreSaleSubs) then
      FreeAndNil(AStoreSaleSubs);
  end;
end;

function TSaleSubService.SQL(Instance: TSaleSub): string;
begin
  Result := '';
end;

function TSaleSubService.Save(Instance: TSaleSub): boolean;
begin

end;

end.
