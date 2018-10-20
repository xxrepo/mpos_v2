unit uStoreMemberOrderDAOImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, cm_messager, DB, cm_DB, uStoreSaleOrder, uStoreMemberOrderDAO, uDAO;

type

  { TStoreMemberOrderDAO }

  TStoreMemberOrderDAO = class(TPOSDAO, IStoreMemberOrderDAO)
  private
    FStatement: ICMStatement;
    function GetObject(SQL: string): TStoreMemberOrder;
    function GetObjects(SQL: string): TStoreMemberOrders;
    function SetProperty(ADataSet: TDataSet; Instance: TStoreMemberOrder): boolean;
  public
    constructor Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
    destructor Destroy;
  public
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreMemberOrder): string;
    function GetUpdateScript(Instance: TStoreMemberOrder): string;
    function GetDeleteScript(Instance: TStoreMemberOrder): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreMemberOrder): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(AGUID: string): TStoreMemberOrder; overload;
    function GetByParentGUID(AGUID: string): TStoreMemberOrders; overload;
  end;


implementation

uses
  StrUtils;

{ TSaleOrderDAO }


constructor TStoreMemberOrderDAO.Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
begin
  inherited Create(AOwner, AHandler);
  FStatement := AStatement;
end;

destructor TStoreMemberOrderDAO.Destroy;
begin
  inherited Destroy;
end;

function TStoreMemberOrderDAO.GetObject(SQL: string): TStoreMemberOrder;
var
  ADataSet: TDataSet;
begin
  Result := nil;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      Result := TStoreMemberOrder.Create();
      SetProperty(ADataSet, Result);
    except
      on e: Exception do
        Messager.Error('TStoreMemberOrderDAO.GetObject: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreMemberOrderDAO.SetProperty(ADataSet: TDataSet; Instance: TStoreMemberOrder): boolean;
begin
  Result := False;
  try
    Instance._UUID := ADataSet.FieldByName('UUID').AsString;

  except
    on e: Exception do
      Messager.Error('TStoreMemberOrderDAO.SetProperty: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreMemberOrderDAO.GetObjects(SQL: string): TStoreMemberOrders;
var
  Instance: TStoreMemberOrder;
  ADataSet: TDataSet;
begin
  Result := TStoreMemberOrders.Create;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      while not ADataSet.EOF do
      begin
        Instance := TStoreMemberOrder.Create();
        SetProperty(ADataSet, Instance);
        Result.Add(Instance);
        ADataSet.Next;
      end;
    except
      on e: Exception do
        Messager.Error('TStoreMemberOrderDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreMemberOrderDAO.GetInsertScript(Instance: TStoreMemberOrder): string;
const
  ValueSQL: string = ' VALUES(%s,%s,%s,%s,%s,%s, %f,%f,%f,%f,%f,%d, %s,%s,%s,s, %f,%f,%f,%f, %d,%s);';
  FieldSQL: string =
    'INSERT INTO tbStoreMemberOrder(CompanyCode, ShopCode, FLOWNO, POSNO, FILDATE, CASHIER, TOTAL, REALAMT, GIVEZEROAMT, DISCOUNTAMOUNT, RECCNT, SCORE, CARDCODE, MEMO, SRCUUID, CardNo, CustPayCash, FixDiscAmt, FixRealAmt, TOTALQTY, ORDERTYPE, UUID) ';
begin
  //Result := Format(FieldSQL + ValueSQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode),
  //  QuotedStr(Instance._FlowNo), QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)),
  //  QuotedStr(Instance._Cashier), Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount,
  //  Instance._RecCnt, Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreMemberOrderDAO.GetUpdateScript(Instance: TStoreMemberOrder): string;
const
  SQL: string =
    'UPDATE tbStoreMemberOrder SET CompanyCode = %s, ShopCode = %s, FLOWNO = %s, POSNO = %s, FILDATE = %s, CASHIER = %s, TOTAL = %f, REALAMT = %f, GIVEZEROAMT = %f, DISCOUNTAMOUNT = %f, RECCNT = %f, SCORE = %s, CARDCODE = %s, MEMO = %s, SRCUUID = %s, CardNo = %s, CustPayCash = %f, FixDiscAmt = %f, FixRealAmt = %f, TOTALQTY = %f, ORDERTYPE = %d WHERE UUID = %s;';
begin
  //Result := Format(SQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode), QuotedStr(Instance._FlowNo),
  //  QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)), QuotedStr(Instance._Cashier),
  //  Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount, Instance._RecCnt,
  //  Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreMemberOrderDAO.GetDeleteScript(Instance: TStoreMemberOrder): string;
const
  SQL = 'DELETE tbStoreSalePromotion WHERE UUID = %s;';
begin
  Result := Format(SQL, [QuotedStr(Instance._UUID)]);
end;

function TStoreMemberOrderDAO.Save(Instance: TStoreMemberOrder): boolean;
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

function TStoreMemberOrderDAO.Remove(AValue: string): boolean;
begin
  try
    Result := FStatement.Execute(Format('DELETE tbStoreMemberOrder WHERE Code = %s;', [QuotedStr(AValue)]));
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.Remove: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreMemberOrderDAO.Get(AGUID: string): TStoreMemberOrder;
begin
  Result := GetObject(Format('SELECT * FROM tbStoreMemberOrder WHERE Code = %s;', [QuotedStr(AGUID)]));
end;

function TStoreMemberOrderDAO.GetByParentGUID(AGUID: string): TStoreMemberOrders;
begin
  Result := GetObjects(Format('SELECT * FROM tbStoreMemberOrder WHERE Code = %s;', [QuotedStr(AGUID)]));
end;



end.




