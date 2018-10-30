unit uPayAssignDAOImpl;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  cm_messager,
  uDAO, uPayPO, uPayDAO;

type

  { TPayAssignDAO }

  TPayAssignDAO = class(TPOSDAO, IPayAssignDAO)
  public // IPayRecordDAO-----------------------------
    function existPayAssign(APayAssignPO: TPayAssignPO): boolean;
    function savePayAssign(APayAssignPO: TPayAssignPO): boolean;
    function getPayAssign(const AAssignUUID: string): TPayAssignPO;
    function getPayAssignList(const APayUUID: string): TPayAssignPOList;
    function GetPayedAmountByPayUUID(const APayUUID: string): currency;
  end;

implementation


function TPayAssignDAO.savePayAssign(APayAssignPO: TPayAssignPO): boolean;
const
  SQLFmt = 'INSERT INTO tbPayAssign(PayUUID, AssignUUID, serviceCode, Amount, state, Time, Remark) VALUES(%s, %s, %s, %s, %d, %s, %s);';
begin
  Result := GetDBHelper.Execute(Format(SQLFmt, [QuotedStr(APayAssignPO.PayUUID), QuotedStr(APayAssignPO.AssignUUID),
    QuotedStr(APayAssignPO.ServiceCode), CurrToStr(APayAssignPO.Amount, DataFmtSet), APayAssignPO.State,
    QuotedStr(DataFmtDateTime(APayAssignPO.Time_)), QuotedStr(APayAssignPO.Remark)]));
end;


function TPayAssignDAO.existPayAssign(APayAssignPO: TPayAssignPO): boolean;
const
  SQLFmt = 'SELECT * FROM tbPayAssign WHERE PayUUID = %s;';
var
  ds: TDataSet;
begin
  Result := False;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(APayAssignPO.AssignUUID)]);
  if Assigned(ds) then
  begin
    Result := not ds.IsEmpty;
    ds.Free;
  end;
end;

function TPayAssignDAO.getPayAssign(const AAssignUUID: string): TPayAssignPO;
const
  SQLFmt = 'SELECT * FROM tbPayAssign WHERE AssignUUID = %s;';
var
  ds: TDataSet;
  payrec: TPayAssignPO;
begin
  Result := nil;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(AAssignUUID)]);
  if Assigned(ds) then
  begin
    if (not ds.IsEmpty) then
    begin
      payrec := TPayAssignPO.Create;
      payrec.PayUUID := ds.FieldByName('payUUID').AsString;
      payrec.AssignUUID := ds.FieldByName('assignUUID').AsString;
      payrec.ServiceCode := ds.FieldByName('serviceCode').AsString;
      payrec.Amount := ds.FieldByName('amount').AsCurrency;
      payrec.Time_ := ds.FieldByName('time').AsDateTime;
      payrec.Remark := ds.FieldByName('remark').AsString;
      Result := payrec;
    end;
    ds.Free;
  end;
end;

function TPayAssignDAO.getPayAssignList(const APayUUID: string): TPayAssignPOList;
const
  SQLFmt = 'SELECT * FROM tbPayAssign WHERE payUUID = %s;';
var
  ds: TDataSet;
  payrec: TPayAssignPO;
begin
  Result := TPayAssignPOList.Create;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(APayUUID)]);
  if Assigned(ds) then
  begin
    while (not ds.EOF) do
    begin
      payrec := TPayAssignPO.Create;
      payrec.PayUUID := ds.FieldByName('payUUID').AsString;
      payrec.AssignUUID := ds.FieldByName('assignUUID').AsString;
      payrec.ServiceCode := ds.FieldByName('serviceCode').AsString;
      payrec.Amount := ds.FieldByName('amount').AsCurrency;
      payrec.Time_ := ds.FieldByName('time').AsDateTime;
      payrec.Remark := ds.FieldByName('remark').AsString;
      Result.Add(payrec);
      ds.Next;
    end;
    ds.Free;
  end;
end;

function TPayAssignDAO.GetPayedAmountByPayUUID(const APayUUID: string): currency;
const
  SQLFmt = 'SELECT IFNULL(SUM(I.amount),0) as Amount '+
            'FROM tbPayAssign as A '+
            'LEFT JOIN tbPayInfo as I ON A.AssignUUID = I.AssignUUID '+
            'WHERE A.PayUUID = %s';
var
  ds: TDataSet;
begin
  Result := 0;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(APayUUID)]);
  if Assigned(ds) then
    begin
      if (not ds.IsEmpty) then
        Result := ds.FieldByName('Amount').AsCurrency;
      ds.Free;
    end;
end;





end.














