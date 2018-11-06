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
  try
    ds := GetDBHelper.Query(SQLFmt, [QuotedStr(APayCode)]);
    if Assigned(ds) then
    begin
      Result := not ds.IsEmpty;
      ds.Free;
    end;
  except
    on e: Exception do
      Messager.Error('existPayType: ', e)
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
  try
    ds := GetDBHelper.Query(SQLFmt, [QuotedStr(APayCode)]);
    if Assigned(ds) then
    begin
      if (not ds.IsEmpty) then
      begin
        payType := TPayTypePO.Create;
        payType.Code := ds.FieldByName('PayCode').AsString;
        payType.Name := ds.FieldByName('PayName').AsString;
        payType.SeqNo := ds.FieldByName('SeqNo').AsInteger;

        payType.AllowRefund := ds.FieldByName('AllowRefund').AsBoolean;
        payType.IsDefault := ds.FieldByName('IsDefault').AsBoolean;
        payType.IsEnabled := ds.FieldByName('IsEnabled').AsBoolean;
        payType.IsSingle := ds.FieldByName('IsSingle').AsBoolean;
        payType.ConfigInfo := ds.FieldByName('ConfigInfo').AsString;

        payType.Remark := ds.FieldByName('remark').AsString;
        Result := payType;
      end;
      ds.Free;
    end;
  except
    on e: Exception do
      Messager.Error('getPayType: ', e)
  end;
end;

function TPayTypeDAO.getPayTypeList(): TPayTypePOList;
const
  SQLFmt = 'SELECT * FROM tbPosPayMode;';
var
  ds: TDataSet;
  payType: TPayTypePO;
begin
  Result := TPayTypePOList.Create;
  try
    ds := GetDBHelper.Query(SQLFmt);
    if Assigned(ds) then
    begin
      while (not ds.EOF) do
      begin
        payType := TPayTypePO.Create;
        payType.Code := ds.FieldByName('PayCode').AsString;
        payType.Name := ds.FieldByName('PayName').AsString;
        payType.SeqNo := ds.FieldByName('SeqNo').AsInteger;

        payType.AllowRefund := ds.FieldByName('AllowRefund').AsBoolean;
        payType.IsDefault := ds.FieldByName('IsDefault').AsBoolean;
        payType.IsEnabled := ds.FieldByName('IsEnabled').AsBoolean;
        payType.IsSingle := ds.FieldByName('IsSingle').AsBoolean;
        payType.ConfigInfo := ds.FieldByName('ConfigInfo').AsString;

        payType.Remark := ds.FieldByName('remark').AsString;
        Result.Add(payType);
        ds.Next;
      end;
      ds.Free;
    end;
  except
    on e: Exception do
      Messager.Error('getPayTypeList: ', e)
  end;

end;

end.










