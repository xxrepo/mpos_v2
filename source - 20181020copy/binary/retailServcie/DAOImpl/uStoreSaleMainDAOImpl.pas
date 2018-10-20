unit uStoreSaleMainDAOImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, cm_messager, DB, cm_DB, uStoreSaleOrder, uStoreSaleMainDAO, uDAO;

type

  { TStoreSaleMainDAO }

  TStoreSaleMainDAO = class(TPOSDAO, IStoreSaleMainDAO)
  private
    FStatement: ICMStatement;
    function GetObject(SQL: string; Instance: TStoreSaleMain = nil): TStoreSaleMain;
    function GetObjects(SQL: string): TStoreSaleMains;
    function SetProperty(ADataSet: TDataSet; Instance: TStoreSaleMain): boolean;
  public
    constructor Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
    destructor Destroy;
  public
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreSaleMain): string;
    function GetUpdateScript(Instance: TStoreSaleMain): string;
    function GetDeleteScript(Instance: TStoreSaleMain): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreSaleMain): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(const AGUID: string): TStoreSaleMain; overload;
  end;


implementation

uses
  StrUtils;

{ TStoreSaleMainDAO }


constructor TStoreSaleMainDAO.Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
begin
  inherited Create(AOwner, AHandler);
  FStatement := AStatement;
end;

destructor TStoreSaleMainDAO.Destroy;
begin
  inherited Destroy;
end;

function TStoreSaleMainDAO.GetObject(SQL: string; Instance: TStoreSaleMain = nil): TStoreSaleMain;
var
  ADataSet: TDataSet;
begin
  Result := nil;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      if not Assigned(Instance) then
        Instance := TStoreSaleMain.Create();
      SetProperty(ADataSet, Instance);
    except
      on e: Exception do
        Messager.Error('TSaleOrderDAO.GetObject: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreSaleMainDAO.SetProperty(ADataSet: TDataSet; Instance: TStoreSaleMain): boolean;
begin
  Result := False;
  try
    Instance._UUID := ADataSet.FieldByName('UUID').AsString;
    Instance._CompanyCode := ADataSet.FieldByName('CompanyCode').AsString;
    Instance._ShopCode := ADataSet.FieldByName('ShopCode').AsString;
    Instance._FlowNo := ADataSet.FieldByName('FlowNo').AsString;
    Instance._PosNo := ADataSet.FieldByName('PosNo').AsString;
    Instance._FilDate := ADataSet.FieldByName('FilDate').AsDateTime;
    Instance._Cashier := ADataSet.FieldByName('Cashier').AsString;

    Instance._Total := ADataSet.FieldByName('Total').AsFloat;
    Instance._RealAmt := ADataSet.FieldByName('RealAmt').AsFloat;
    Instance._GiveZeroAmt := ADataSet.FieldByName('GiveZeroAmt').AsFloat;
    Instance._DiscountAmount := ADataSet.FieldByName('DiscountAmount').AsFloat;
    Instance._RecCnt := ADataSet.FieldByName('RecCnt').AsFloat;

    Instance._SCore := ADataSet.FieldByName('SCore').AsInteger;

    Instance._CardCode := ADataSet.FieldByName('CardCode').AsString;
    Instance._Memo := ADataSet.FieldByName('Memo').AsString;
    Instance._SrcUUID := ADataSet.FieldByName('SrcUUID').AsString;
    Instance._CardNo := ADataSet.FieldByName('CardNo').AsString;

    Instance._CustPayCash := ADataSet.FieldByName('CustPayCash').AsFloat;
    Instance._FixDiscAmt := ADataSet.FieldByName('FixDiscAmt').AsFloat;
    Instance._FixRealAmt := ADataSet.FieldByName('FixRealAmt').AsFloat;
    Instance._TotalQty := ADataSet.FieldByName('TotalQty').AsFloat;

    Instance._OrderType := ADataSet.FieldByName('OrderType').AsInteger;
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.SetInstanceProperty: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreSaleMainDAO.GetObjects(SQL: string): TStoreSaleMains;
var
  Instance: TStoreSaleMain;
  ADataSet: TDataSet;
begin
  Result := TStoreSaleMains.Create;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      while not ADataSet.EOF do
      begin
        Instance := TStoreSaleMain.Create();
        SetProperty(ADataSet, Instance);
        Result.Add(Instance);
        ADataSet.Next;
      end;
    except
      on e: Exception do
        Messager.Error('TSaleOrderDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreSaleMainDAO.GetInsertScript(Instance: TStoreSaleMain): string;
const
  ValueSQL: string = ' VALUES(%s,%s,%s,%s,%s,%s, %f,%f,%f,%f,%f,%d, %s,%s,%s,s, %f,%f,%f,%f, %d,%s);';
  FieldSQL: string =
    'INSERT INTO tbStoreSaleMain(CompanyCode, ShopCode, FLOWNO, POSNO, FILDATE, CASHIER, TOTAL, REALAMT, GIVEZEROAMT, DISCOUNTAMOUNT, RECCNT, SCORE, CARDCODE, MEMO, SRCUUID, CardNo, CustPayCash, FixDiscAmt, FixRealAmt, TOTALQTY, ORDERTYPE, UUID) ';
begin
  Result := Format(FieldSQL + ValueSQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode),
    QuotedStr(Instance._FlowNo), QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)),
    QuotedStr(Instance._Cashier), Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount,
    Instance._RecCnt, Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
    QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
    Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreSaleMainDAO.GetUpdateScript(Instance: TStoreSaleMain): string;
const
  SQL: string =
    'UPDATE tbStoreSaleMain SET CompanyCode = %s, ShopCode = %s, FLOWNO = %s, POSNO = %s, FILDATE = %s, CASHIER = %s, TOTAL = %f, REALAMT = %f, GIVEZEROAMT = %f, DISCOUNTAMOUNT = %f, RECCNT = %f, SCORE = %s, CARDCODE = %s, MEMO = %s, SRCUUID = %s, CardNo = %s, CustPayCash = %f, FixDiscAmt = %f, FixRealAmt = %f, TOTALQTY = %f, ORDERTYPE = %d WHERE UUID = %s;';
begin
  Result := Format(SQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode), QuotedStr(Instance._FlowNo),
    QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)), QuotedStr(Instance._Cashier),
    Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount, Instance._RecCnt,
    Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
    QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
    Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreSaleMainDAO.GetDeleteScript(Instance: TStoreSaleMain): string;
begin
  Result := '';
end;

function TStoreSaleMainDAO.Save(Instance: TStoreSaleMain): boolean;
var
  Guid: TGuid;
begin
  try
    if (Instance._UUID = '') then
    begin
      CreateGUID(Guid);
      Instance._UUID := ReplaceStr(ReplaceStr(GUIDToString(Guid), '{', ''), '}', '');
      Result := FStatement.Execute(GetInsertScript(Instance));
    end
    else
      Result := FStatement.Execute(GetUpdateScript(Instance))
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.SaveInstance: %s %s', [e.ClassName, e.Message]);
  end;

end;

function TStoreSaleMainDAO.Remove(AValue: string): boolean;
begin
  try
    Result := FStatement.Execute(Format('DELETE tbStoreSaleMain WHERE Code = %s;', [QuotedStr(AValue)]));
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.RemoveInstance: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreSaleMainDAO.Get(const AGUID: string): TStoreSaleMain;
begin
  Result := GetObject(Format('SELECT * FROM tbStoreSaleMain WHERE Code = %s;', [QuotedStr(AGUID)]));
end;


end.





