unit uSaleDAOImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  cm_sysutils, cm_DBUtils,
  uDAO, uSaleDAO,
  uSalePO;

type

  { TSaleDAO }

  TSaleDAO = class(TPOSDAO, ISaleDAO)
  private

  public
    function GetSaveOrderScript(AOrder: TSaleOrder): string;
    function GetSaveDetailScript(ADetail: TSaleDetail): string;
  public
    function SaveOrder(AOrder: TSaleOrder): Boolean;
    function SaveDetail(ADetail: TSaleDetail): Boolean;
    function SaveOrderEx(AOrderEx: TSaleOrderEx): Boolean;
    function ExistOrder(const AUUID: string): Boolean;
  end;

Resourcestring
  SaveOrderSQLFmt = 'insert into saleOrder(UUID,sumSettlementAmount,sellerID,settlementTime,remark)'
                  + ' values(%s,%s,%s,%s,%s);';
  SaveDetailSQLFmt = 'insert into saleDetail(UUID,orderUUID,GID,commodityName,settlementPrice,quantity,settlementAmount,remark)'
                               + ' values(%s,%s,%d,%s,%s,%s,%s,%s);';
  ExistOrderSQLFmt = 'select 1 from saleOrder where UUID=%s;';


implementation

{ TSaleDAO }

function TSaleDAO.GetSaveOrderScript(AOrder: TSaleOrder): string;
begin
  Result := Format(SaveOrderSQLFmt, [QuotedStr(AOrder.UUID), CurrToStr(AOrder.SumSettlementAmount, DataFmtSet),
                                     QuotedStr(AOrder.SellerID), QuotedStr(DataFmtDateTime(AOrder.SettlementTime)),
                                     QuotedStr(AOrder.Remark)]);
end;

function TSaleDAO.GetSaveDetailScript(ADetail: TSaleDetail): string;
begin
  Result := Format(SaveDetailSQLFmt, [QuotedStr(ADetail.UUID), QuotedStr(ADetail.OrderUUID), ADetail.GID, QuotedStr(ADetail.CommodityName),
                                      CurrToStr(ADetail.SettlementPrice, DataFmtSet),
                                      CurrToStr(ADetail.Quantity, DataFmtSet),
                                      CurrToStr(ADetail.SettlementAmount, DataFmtSet),
                                      QuotedStr(ADetail.Remark)]);
end;

function TSaleDAO.SaveOrder(AOrder: TSaleOrder): Boolean;
begin
  Result := GetDBHelper.Execute(GetSaveOrderScript(AOrder));
end;

function TSaleDAO.SaveDetail(ADetail: TSaleDetail): Boolean;
begin
  Result := GetDBHelper.Execute(GetSaveDetailScript(ADetail));
end;

function TSaleDAO.SaveOrderEx(AOrderEx: TSaleOrderEx): Boolean;
var
  b: TSQLBatch;
  i: Integer;
begin
  Result := False;
  b := GetDBHelper.GetSQLBatch;
  b.AddSQL(GetSaveOrderScript(AOrderEx));
  for i:=0 to AOrderEx.Details.Count-1 do
    begin
      b.AddSQL(GetSaveDetailScript(AOrderEx.Details[i]));
    end;
  Result := b.Execute;
  b.Free;
end;

function TSaleDAO.ExistOrder(const AUUID: string): Boolean;
var
  ds: TDataSet;
begin
  Result := False;
  ds := GetDBHelper.Query(ExistOrderSQLFmt, [QuotedStr(AUUID)]);
  if Assigned(ds) then
    begin
      Result := not ds.IsEmpty;
      ds.Free;
    end;
end;

end.

