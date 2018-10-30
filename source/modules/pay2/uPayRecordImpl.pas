unit uPayRecordImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  cm_messager,
  uDAO, uPayPO, uPayDAO;

type

  { TPayDAO }

  { TPayRecordDAO }

  TPayRecordDAO = class(TPOSDAO, IPayRecordDAO)
  public // IPayRecordDAO-----------------------------
    function existPayRecord(APayRecordPO: TPayRecordPO): boolean;
    function savePayRecord(APayRecordPO: TPayRecordPO): boolean;
    function getPayRecord(const APayUUID: string): TPayRecordPO;
    function getPayRecordList(const AOrderUUID: string): TPayRecordPOList;
    function GetPayedAmountByOrderUUID(const AOrderUUID: string): Currency;
  end;

implementation


function TPayRecordDAO.savePayRecord(APayRecordPO: TPayRecordPO): boolean;
const
  SQLFmt = 'INSERT INTO tbPayRecord(PayUUID, orderUUID, Amount, Time, Remark) VALUES(%s, %s, %s, %s, %s);';
begin
  Result := GetDBHelper.Execute(Format(SQLFmt, [QuotedStr(APayRecordPO.PayUUID), QuotedStr(APayRecordPO.OrderUUID),
    CurrToStr(APayRecordPO.Amount, DataFmtSet), QuotedStr(DataFmtDateTime(APayRecordPO.Time_)), QuotedStr(APayRecordPO.Remark)]));
end;

function TPayRecordDAO.existPayRecord(APayRecordPO: TPayRecordPO): boolean;
const
  SQLFmt = 'SELECT * FROM tbPayRecord WHERE PayUUID = %s;';
var
  ds: TDataSet;
begin
  Result := False;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(APayRecordPO.PayUUID)]);
  if Assigned(ds) then
  begin
    Result := not ds.IsEmpty;
    ds.Free;
  end;
end;

function TPayRecordDAO.getPayRecord(const APayUUID: string): TPayRecordPO;
const
  SQLFmt = 'SELECT * FROM tbPayRecord WHERE PayUUID = %s;';
var
  ds: TDataSet;
  payrec: TPayRecordPO;
begin
  Result := nil;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(APayUUID)]);
  if Assigned(ds) then
  begin
    if (not ds.IsEmpty) then
    begin
      payrec := TPayRecordPO.Create;
      payrec.PayUUID := ds.FieldByName('payUUID').AsString;
      payrec.OrderUUID := ds.FieldByName('orderUUID').AsString;
      payrec.Amount := ds.FieldByName('amount').AsCurrency;
      payrec.Time_ := ds.FieldByName('time').AsDateTime;
      payrec.Remark := ds.FieldByName('remark').AsString;
      Result := payrec;
    end;
    ds.Free;
  end;
end;

function TPayRecordDAO.getPayRecordList(const AOrderUUID: string): TPayRecordPOList;
const
  SQLFmt = 'SELECT * FROM tbPayRecord WHERE OrderUUID = %s;';
var
  ds: TDataSet;
  payrec: TPayRecordPO;
begin
  Result := TPayRecordPOList.Create;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(AOrderUUID)]);
  if Assigned(ds) then
  begin
    while (not ds.EOF) do
    begin
      payrec := TPayRecordPO.Create;
      payrec.PayUUID := ds.FieldByName('payUUID').AsString;
      payrec.OrderUUID := ds.FieldByName('orderUUID').AsString;
      payrec.Amount := ds.FieldByName('amount').AsCurrency;
      payrec.Time_ := ds.FieldByName('time').AsDateTime;
      payrec.Remark := ds.FieldByName('remark').AsString;
      Result.Add(payrec);
      ds.Next;
    end;
    ds.Free;
  end;
end;

function TPayRecordDAO.GetPayedAmountByOrderUUID(const AOrderUUID: string): Currency;
const
  SQLFmt = 'SELECT IFNULL(SUM(I.amount),0) as Amount '+
            'FROM tbPayRecord R '+
            'LEFT JOIN tbPayAssign as A ON R.PayUUID = A.PayUUID '+
            'LEFT JOIN tbPayInfo as I ON A.AssignUUID = I.AssignUUID '+
            'WHERE R.OrderUUID = %s';
var
  ds: TDataSet;
begin
  Result := 0;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(AOrderUUID)]);
  if Assigned(ds) then
    begin
      if (not ds.IsEmpty) then
        Result := ds.FieldByName('Amount').AsCurrency;
      ds.Free;
    end;
end;

end.








