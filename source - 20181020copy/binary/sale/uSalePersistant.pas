unit uSalePersistant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uApp, uSale, uSaleBO, uSalePO,
  uDAO, uSaleDAO;

type

  { TTestSalePersistant }

  TTestSalePersistant = class(TSaleHandler)
  protected
    function DoBeforeHandle(ABill: TSaleBill): Boolean; override;
  end;

implementation

{ TTestSalePersistant }

function TTestSalePersistant.DoBeforeHandle(ABill: TSaleBill): Boolean;
var
  d: TSaleDetail;
  o: TSaleOrderEx;
  i: Integer;
  c: TSaleCommodity;
  dao: ISaleDAO;
begin
  Result := False;
  Messager.Debug('DoHandle()...');
  Messager.Info('进行销售持久化操作...');
  if not InterfaceRegister.OutInterface(ISaleDAO, dao) then
    begin
      Messager.Error('销售DAO未实例化！');
      Exit;
    end;
  //
  o := TSaleOrderEx.Create;
  o.UUID := ABill.UUID;
  o.SumSettlementAmount := ABill.SumSettlementAmount;
  o.SellerID := '123';
  o.SettlementTime := now;
  o.Remark := '';
  //
  for i:=0 to ABill.CommodityCount-1 do
    begin
      c := ABill.GetCommodity(i);
      d := TSaleDetail.Create;
      d.UUID := c.UUID;
      d.OrderUUID := ABill.UUID;
      d.GID := c.GID;
      d.CommodityName := c.Name;
      d.SettlementPrice := c.SettlementPrice;
      d.Quantity := c.Quantity;
      d.SettlementAmount := c.GetSettlementAmount;
      d.Remark := c.Remark;
      o.Details.Add(d);
    end;
  dao.SaveOrderEx(o);
  Result := True;
  o.Free;
end;

end.

