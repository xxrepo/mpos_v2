unit uPayInfoImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  cm_messager,
  uDAO, uPayPO, uPayDAO;

type

  { TPayInfoDAO }

  TPayInfoDAO = class(TPOSDAO, IPayInfoDAO)
  public // IPayDAO-----------------------------
    function existPayInfo(APayInfoPO: TPayInfoPO): boolean;
    function savePayInfo(APayInfoPO: TPayInfoPO): boolean;
    function getPayInfo(const AServiceUUID: string): TPayInfoPO;
    function getPayInfoList(const AAssignUUID: string): TPayInfoPOList;
  end;

implementation

{ TPayDAO }
function TPayInfoDAO.existPayInfo(APayInfoPO: TPayInfoPO): boolean;
const
  SQLFmt = 'SELECT * FROM tbPayInfo WHERE AssignUUID = %s;';
var
  ds: TDataSet;
begin
  Result := False;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(APayInfoPO.AssignUUID)]);
  if Assigned(ds) then
  begin
    Result := not ds.IsEmpty;
    ds.Free;
  end;
end;

function TPayInfoDAO.savePayInfo(APayInfoPO: TPayInfoPO): boolean;
const
  SQLFmt = 'INSERT INTO tbPayInfo(assignUUID, serviceUUID, Type, Amount, Time, Remark) VALUES(%s, %s, %s, %s, %s, %s);';
begin
  Result := GetDBHelper.Execute(Format(SQLFmt, [QuotedStr(APayInfoPO.AssignUUID), QuotedStr(APayInfoPO.ServiceUUID),
    IntToStr(APayInfoPO.Type_), CurrToStr(APayInfoPO.Amount, DataFmtSet), QuotedStr(DataFmtDateTime(APayInfoPO.Time_)),
    QuotedStr(APayInfoPO.Remark)]));
end;

function TPayInfoDAO.getPayInfo(const AServiceUUID: string): TPayInfoPO;
const
  SQLFmt = 'SELECT * FROM tbPayInfo WHERE ServiceUUID = %s;';
var
  ds: TDataSet;
  pay: TPayInfoPO;
begin
  Result := nil;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(AServiceUUID)]);
  if Assigned(ds) then
  begin
    if (not ds.IsEmpty) then
    begin
      pay := TPayInfoPO.Create;
      pay.AssignUUID := ds.FieldByName('AssignUUID').AsString;
      pay.ServiceUUID := ds.FieldByName('ServiceUUID').AsString;
      pay.Type_ := ds.FieldByName('Type').AsInteger;
      pay.Amount := ds.FieldByName('amount').AsCurrency;
      pay.Time_ := ds.FieldByName('time').AsDateTime;
      pay.Remark := ds.FieldByName('remark').AsString;
      Result := pay;
    end;
    ds.Free;
  end;

end;

function TPayInfoDAO.getPayInfoList(const AAssignUUID: string): TPayInfoPOList;
const
  SQLFmt = 'SELECT * FROM tbPayInfo WHERE AssignUUID = %s;';
var
  ds: TDataSet;
  pay: TPayInfoPO;
begin
  Result := TPayInfoPOList.Create;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(AAssignUUID)]);
  if Assigned(ds) then
  begin
    while (not ds.EOF) do
    begin
      pay := TPayInfoPO.Create;
      pay.AssignUUID := ds.FieldByName('AssignUUID').AsString;
      pay.ServiceUUID := ds.FieldByName('ServiceUUID').AsString;
      pay.Type_ := ds.FieldByName('Type').AsInteger;
      pay.Amount := ds.FieldByName('amount').AsCurrency;
      pay.Time_ := ds.FieldByName('time').AsDateTime;
      pay.Remark := ds.FieldByName('remark').AsString;
      Result.Add(pay);
      ds.Next;
    end;
    ds.Free;
  end;
end;




end.


