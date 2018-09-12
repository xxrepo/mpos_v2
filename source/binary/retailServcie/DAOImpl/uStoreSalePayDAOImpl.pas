unit uStoreSalePayDAOImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, cm_messager, DB, cm_DB, uStoreSaleOrder, uStoreSalePayDAO, uDAO;

type

  { TStoreSalePayDAO }

  TStoreSalePayDAO = class(TPOSDAO, IStoreSalePayDAO)
  private
    FStatement: ICMStatement;
    function GetObject(SQL: string): TStoreSalePay;
    function GetObjects(SQL: string): TStoreSalePays;
    function SetProperty(ADataSet: TDataSet; Instance: TStoreSalePay): boolean;
  public
    constructor Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
    destructor Destroy;
  public
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreSalePay): string;
    function GetUpdateScript(Instance: TStoreSalePay): string;
    function GetDeleteScript(Instance: TStoreSalePay): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreSalePay): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(AGUID: string): TStoreSalePay; overload;
    function GetByParentGUID(AGUID: string): TStoreSalePays; overload;
  end;


implementation

uses
  StrUtils;

{ TSaleOrderDAO }


constructor TStoreSalePayDAO.Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
begin
  inherited Create(AOwner, AHandler);
  FStatement := AStatement;
end;

destructor TStoreSalePayDAO.Destroy;
begin
  inherited Destroy;
end;

function TStoreSalePayDAO.GetObject(SQL: string): TStoreSalePay;
var
  ADataSet: TDataSet;
begin
  Result := nil;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      Result := TStoreSalePay.Create();
      SetProperty(ADataSet, Result);
    except
      on e: Exception do
        Messager.Error('TStoreSalePayDAO.GetObject: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreSalePayDAO.SetProperty(ADataSet: TDataSet; Instance: TStoreSalePay): boolean;
begin
  Result := False;
  try
    Instance._UUID := ADataSet.FieldByName('UUID').AsString;
  except
    on e: Exception do
      Messager.Error('TStoreSalePayDAO.SetProperty: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreSalePayDAO.GetObjects(SQL: string): TStoreSalePays;
var
  Instance: TStoreSalePay;
  ADataSet: TDataSet;
begin
  Result := TStoreSalePays.Create;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      while not ADataSet.EOF do
      begin
        Instance := TStoreSalePay.Create();
        SetProperty(ADataSet, Instance);
        Result.Add(Instance);
        ADataSet.Next;
      end;
    except
      on e: Exception do
        Messager.Error('TStoreSalePayDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreSalePayDAO.GetInsertScript(Instance: TStoreSalePay): string;
const
  ValueSQL: string = ' VALUES(%s,%s,%s,%s,%s,%s, %f,%f,%f,%f,%f,%d, %s,%s,%s,s, %f,%f,%f,%f, %d,%s);';
  FieldSQL: string =
    'INSERT INTO tbStoreSalePay(CompanyCode, ShopCode, FLOWNO, POSNO, FILDATE, CASHIER, TOTAL, REALAMT, GIVEZEROAMT, DISCOUNTAMOUNT, RECCNT, SCORE, CARDCODE, MEMO, SRCUUID, CardNo, CustPayCash, FixDiscAmt, FixRealAmt, TOTALQTY, ORDERTYPE, UUID) ';
begin
  //Result := Format(FieldSQL + ValueSQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode),
  //  QuotedStr(Instance._FlowNo), QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)),
  //  QuotedStr(Instance._Cashier), Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount,
  //  Instance._RecCnt, Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreSalePayDAO.GetUpdateScript(Instance: TStoreSalePay): string;
const
  SQL: string =
    'UPDATE tbStoreSalePay SET CompanyCode = %s, ShopCode = %s, FLOWNO = %s, POSNO = %s, FILDATE = %s, CASHIER = %s, TOTAL = %f, REALAMT = %f, GIVEZEROAMT = %f, DISCOUNTAMOUNT = %f, RECCNT = %f, SCORE = %s, CARDCODE = %s, MEMO = %s, SRCUUID = %s, CardNo = %s, CustPayCash = %f, FixDiscAmt = %f, FixRealAmt = %f, TOTALQTY = %f, ORDERTYPE = %d WHERE UUID = %s;';
begin
  //Result := Format(SQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode), QuotedStr(Instance._FlowNo),
  //  QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)), QuotedStr(Instance._Cashier),
  //  Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount, Instance._RecCnt,
  //  Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreSalePayDAO.GetDeleteScript(Instance: TStoreSalePay): string;
const
  SQL = 'DELETE tbStoreSalePay WHERE UUID = %s;';
begin
  Result := Format(SQL, [QuotedStr(Instance._UUID)]);
end;

function TStoreSalePayDAO.Save(Instance: TStoreSalePay): boolean;
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
      Messager.Error('TSaleOrderDAO.Save: %s %s', [e.ClassName, e.Message]);
  end;

end;

function TStoreSalePayDAO.Remove(AValue: string): boolean;
begin
  try
    Result := FStatement.Execute(Format('DELETE tbStoreSalePay WHERE Code = %s;', [QuotedStr(AValue)]));
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.Remove: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreSalePayDAO.Get(AGUID: string): TStoreSalePay;
begin
  Result := GetObject(Format('SELECT * FROM tbStoreSalePay WHERE UUID = %s;', [QuotedStr(AGUID)]));
end;

function TStoreSalePayDAO.GetByParentGUID(AGUID: string): TStoreSalePays;
begin
  Result := GetObjects(Format('SELECT * FROM tbStoreSalePay WHERE UUID = %s;', [QuotedStr(AGUID)]));
end;


end.




