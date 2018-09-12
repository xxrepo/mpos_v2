unit uSaleSubInvServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_Messager, uSaleOrder, uStoreSaleOrder, uSaleSubInvService,
  uStoreSaleMainDAO, uStoreSaleMainDAOImpl,
  uStoreSaleSubDAO, uStoreSaleSubDAOImpl,
  uStoreSalePayDAO, uStoreSalePayDAOImpl,
  uStoreSaleInvDAO, uStoreSaleInvDAOImpl,
  uStoreSalePromotionDAO, uStoreSalePromotionDAOImpl,
  uStoreMemberOrderDAO, uStoreMemberOrderDAOImpl,
  uShopProductDAO, uShopProductDAOImpl,
  uDAO;

type

  { TSaleSubInvService }

  TSaleSubInvService = class(TCMMessageableComponent, ISaleSubInvService)
  private
    function DoConvertInstance(const AStoreSaleInv: TStoreSaleInv; out ASaleSubInv: TSaleSubInv): boolean; overload;
    function DoConvertInstance(const ASaleSubInv: TSaleSubInv; out AStoreSaleInv: TStoreSaleInv): boolean; overload;
  public
    function Add(): TSaleSubInv;
    function Get(GUID: string): TSaleSubInv;
    function GetByParentGUID(AGUID: string): TSaleSubInvs;
    function SQL(Instance: TSaleSubInv): string;
    function Save(Instance: TSaleSubInv): boolean;
  end;



implementation

uses
  uServiceFactory;

{ TSaleSubInvService }

function TSaleSubInvService.DoConvertInstance(const AStoreSaleInv: TStoreSaleInv; out ASaleSubInv: TSaleSubInv): boolean;
begin
  Result := False;
  try
    ASaleSubInv := TSaleSubInv.Create();

    //ASaleSubInv.GUID := AStoreSaleSubInv._GUID;
    //ASaleSubInv.ParentGUID := AStoreSaleSubInv._ParentGUID;
    //ASaleSubInv.ItemNo := AStoreSaleSubInv._ITEMNO;
    //ASaleSubInv.SrcItemNo := AStoreSaleSubInv._SRCITEMNO;
    //ASaleSubInv.GID := AStoreSaleSubInv._GID;
    //ASaleSubInv.GDCODE := AStoreSaleSubInv._GDCODE;
    //ASaleSubInv.Quantity := AStoreSaleSubInv._QTY;
    //ASaleSubInv.Price := AStoreSaleSubInv._Price;
    //ASaleSubInv.ProdPrice := AStoreSaleSubInv._ProdPrice;
    //ASaleSubInv.AvgPrice := AStoreSaleSubInv._AvgPrice;
    //ASaleSubInv.InputCode := AStoreSaleSubInv._InputBarCode;
    //ASaleSubInv.IsChengePrice := AStoreSaleSubInv._IsChangePrice;

    //if not (ASaleSubInv.GDCODE = '') then
    //  ASaleSubInv.Product := TShopProductDAO(TPOSDAOFactory.GetInstance.GetDAOObject(TShopProductDAO)).GetByGDCode(ASaleSubInv.GDCODE);

    //ASaleSubInv.InnerPackings := ServiceFactory.get;


    Result := True;
  except
    on e: Exception do
      Messager.Error('TSaleMasterService.DoConvertInstance: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TSaleSubInvService.DoConvertInstance(const ASaleSubInv: TSaleSubInv; out AStoreSaleInv: TStoreSaleInv): boolean;
begin

end;

function TSaleSubInvService.Add(): TSaleSubInv;
begin

end;

function TSaleSubInvService.Get(GUID: string): TSaleSubInv;
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

function TSaleSubInvService.GetByParentGUID(AGUID: string): TSaleSubInvs;
var
  i: integer;
  ASaleSubInv: TSaleSubInv = nil;
  ASaleSubInvs: TSaleSubInvs = nil;
  AStoreSaleInvs: TStoreSaleInvs = nil;
begin
  Result := nil;
  try
    try
      ASaleSubInvs := TSaleSubInvs.Create();
      AStoreSaleInvs := TStoreSaleInvDAO(TPOSDAOFactory.GetInstance.GetDAOObject(TStoreSaleInvDAO)).GetByParentGUID(AGUID);
      for i := 0 to AStoreSaleInvs.Count - 1 do
        if DoConvertInstance(AStoreSaleInvs[i], ASaleSubInv) then
          ASaleSubInvs.Add(ASaleSubInv);
      Result := ASaleSubInvs;
    except
      on e: Exception do
      begin
        if Assigned(ASaleSubInvs) then
          FreeAndNil(ASaleSubInvs);
        Messager.Error('TShopProductDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
      end;
    end;
  finally
    if Assigned(AStoreSaleInvs) then
      FreeAndNil(AStoreSaleInvs);
  end;
end;

function TSaleSubInvService.SQL(Instance: TSaleSubInv): string;
begin

end;

function TSaleSubInvService.Save(Instance: TSaleSubInv): boolean;
begin

end;

end.
