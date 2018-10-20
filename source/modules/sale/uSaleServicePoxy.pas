unit uSaleServicePoxy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  cm_sysutils, cm_interfaces,
  uApp,
  uSale, uSaleBO, uSaleDAO;

type

  { TTestSaleService }

  TTestSaleService = class(TSaleHandler)
  protected
    function DoBeforeHandle(ABill: TSaleBill): Boolean; override;
    function DoAfterHandle(ABill: TSaleBill): Boolean; override;
  end;

  { TTestPromotion }

  TTestPromotion = class(TSaleHandler, ISaleBillListener, ICMListener)
  private
    FSaleBoard: ISaleBoard;
  protected
    function DoBeforeHandle(ABill: TSaleBill): Boolean; override;
  public //ISaleBillListener
    procedure BillCreated(ABill: TSaleBill);
    procedure CommodityAdding(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanAdd: Boolean);
    procedure CommodityAdded(ABill: TSaleBill; ACommodity: TSaleCommodity);
    procedure CommodityRemoving(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanDelete: Boolean);
    procedure CommodityRemoved(ABill: TSaleBill; ACommodity: TSaleCommodity);
    procedure CommodityUpdating(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanUpdate: Boolean);
    procedure CommodityUpdated(ABill: TSaleBill; ACommodity: TSaleCommodity);
    procedure BillCleared(ABill: TSaleBill);
  end;


implementation

{ TTestSaleService }

function TTestSaleService.DoBeforeHandle(ABill: TSaleBill): Boolean;
begin
  Result := True;
  Messager.Debug('DoHandle()...');
  Messager.Info('进行销售业务处理...');
end;

function TTestSaleService.DoAfterHandle(ABill: TSaleBill): Boolean;
var
  dao: ISaleDAO;
begin
  Result := False;
  if InterfaceRegister.OutInterface(ISaleDAO, dao) then
    begin
      Result := dao.ExistOrder(ABill.UUID);
    end;
end;


{ TTestPromotion }

function TTestPromotion.DoBeforeHandle(ABill: TSaleBill): Boolean;
begin
  Result := False;
  Messager.Debug('DoHandle()...');
  Messager.Info('进行销售促销处理...');
end;

procedure TTestPromotion.BillCreated(ABill: TSaleBill);
begin
  Messager.Debug('BillCreated()...');
end;

procedure TTestPromotion.CommodityAdding(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanAdd: Boolean);
var
  commodity: TSaleCommodity;
begin
  Messager.Info('CommodityAdding()...');
  if ACommodity.BarCode = '63202' then
    begin
      if Assigned(FSaleBoard) or InterfaceRegister.OutInterface(ISaleBoard, FSaleBoard) then
        FSaleBoard.PromptMessage(etError, '63202是会员专属商品');
      CanAdd := False;
    end;
  if (ABill.CommodityCount < 1) and (ACommodity.Price <> 0) then
    begin
      commodity := TSaleCommodity.Create(CreateGUIDStr);
      commodity.Name := '加多宝 250ml/罐 [赠品] 买单即送';
      commodity.BarCode := '6320000123';
      commodity.Remark := '[赠品] 买单即送';
      commodity.Price := 0;
      commodity.Quantity := 1;
      //
      ABill.AddCommodity(commodity);
      commodity.Free;
    end;
end;

procedure TTestPromotion.CommodityAdded(ABill: TSaleBill; ACommodity: TSaleCommodity);
var
  commodity: TSaleCommodity;
begin
  Messager.Debug('CommodityAdded()...');
  if ACommodity.BarCode = '63201' then
    begin
      Messager.Info('买63201送一...');
      commodity := TSaleCommodity.Create(CreateGUIDStr);
      commodity.Name := '椰树椰子汁 500ml/罐 [赠品] 买63201送一';
      commodity.BarCode := '6320000123';
      commodity.Remark := '[赠品] 买63201送一';
      commodity.Price := 0;
      commodity.Quantity := 1;
      //
      ABill.AddCommodity(commodity);
      commodity.Free;
    end;
end;

procedure TTestPromotion.CommodityRemoving(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanDelete: Boolean);
begin
  Messager.Debug('CommodityRemoving()...');
  if ACommodity.BarCode = '6320000123' then
    begin
      if Assigned(FSaleBoard) or InterfaceRegister.OutInterface(ISaleBoard, FSaleBoard) then
        FSaleBoard.PromptMessage(etError, '赠品6320000123不能删除!');
      CanDelete := False;
    end;
end;

procedure TTestPromotion.CommodityRemoved(ABill: TSaleBill; ACommodity: TSaleCommodity);
begin
  Messager.Debug('CommodityRemoved()...');
end;

procedure TTestPromotion.CommodityUpdating(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanUpdate: Boolean);
begin
  Messager.Debug('CommodityUpdating()...');
  if ACommodity.Quantity > 100 then
    begin
      if Assigned(FSaleBoard) or InterfaceRegister.OutInterface(ISaleBoard, FSaleBoard) then
        FSaleBoard.PromptMessage(etError, '不能超过100件!');
      CanUpdate := False;
      Exit;
    end;
end;

procedure TTestPromotion.CommodityUpdated(ABill: TSaleBill; ACommodity: TSaleCommodity);
begin
  Messager.Debug('CommodityUpdated()...');
end;

procedure TTestPromotion.BillCleared(ABill: TSaleBill);
begin
  Messager.Debug('BillCleared()...');
end;




end.


