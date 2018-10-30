unit uPayTypeImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  cm_messager,
  uDAO, uPayPO, uPayDAO;

type

  { TPayTypeDAO }

  TPayTypeDAO = class(TPOSDAO, IPayTypeDAO)
  public // IPayTypeDAO-----------------------------
    function existPayType(const APayCode: string): boolean;
    function getPayType(const APayCode: string): TPayTypePO;
    function getPayTypeList(): TPayTypePOList;
  end;

implementation

function TPayTypeDAO.existPayType(const APayCode: string): boolean;
const
  SQLFmt = 'SELECT * FROM tbPosPayMode WHERE APayCode = %s COLLATE NOCASE;';
var
  ds: TDataSet;
begin
  Result := False;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(APayCode)]);
  if Assigned(ds) then
  begin
    Result := not ds.IsEmpty;
    ds.Free;
  end;
end;

function TPayTypeDAO.getPayType(const APayCode: string): TPayTypePO;
const
  SQLFmt = 'SELECT * FROM tbPosPayMode WHERE APayCode = %s COLLATE NOCASE;';
var
  ds: TDataSet;
  payType: TPayTypePO;
begin
  Result := nil;
  ds := GetDBHelper.Query(SQLFmt, [QuotedStr(APayCode)]);
  if Assigned(ds) then
  begin
    if (not ds.IsEmpty) then
    begin
      payType := TPayTypePO.Create;
      payType.Code := ds.FieldByName('Code').AsString;
      payType.Name := ds.FieldByName('Name').AsString;
      payType.SeqNo := ds.FieldByName('Code').AsInteger;
      payType.Caption := ds.FieldByName('Caption').AsString;
      payType.ConfigInfo := ds.FieldByName('Code').AsString;

      payType.AllowRefund := ds.FieldByName('payUUID').AsBoolean;
      payType.IsDefault := ds.FieldByName('IsDefault').AsBoolean;
      payType.IsEnabled := ds.FieldByName('IsEnabled').AsBoolean;
      payType.IsSingle := ds.FieldByName('IsSingle').AsBoolean;

      payType.Remark := ds.FieldByName('remark').AsString;
      Result := payType;
    end;
    ds.Free;
  end;
end;

function TPayTypeDAO.getPayTypeList(): TPayTypePOList;
const
  SQLFmt = 'SELECT * FROM tbPosPayMode COLLATE NOCASE;';
var
  ds: TDataSet;
  payType: TPayTypePO;
begin
  Result := TPayTypePOList.Create;
  ds := GetDBHelper.Query(SQLFmt);
  if Assigned(ds) then
  begin
    while (not ds.EOF) do
    begin
      payType := TPayTypePO.Create;
      payType.Code := ds.FieldByName('Code').AsString;
      payType.Name := ds.FieldByName('Name').AsString;
      payType.SeqNo := ds.FieldByName('Code').AsInteger;
      payType.Caption := ds.FieldByName('Caption').AsString;
      payType.ConfigInfo := ds.FieldByName('Code').AsString;

      payType.AllowRefund := ds.FieldByName('payUUID').AsBoolean;
      payType.IsDefault := ds.FieldByName('IsDefault').AsBoolean;
      payType.IsEnabled := ds.FieldByName('IsEnabled').AsBoolean;
      payType.IsSingle := ds.FieldByName('IsSingle').AsBoolean;

      payType.Remark := ds.FieldByName('remark').AsString;
      Result.Add(payType);
      ds.Next;
    end;
    ds.Free;
  end;

end;

end.










