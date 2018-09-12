unit uStoreSaleInvDAOImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, cm_messager, DB, cm_DB, uStoreSaleOrder, uStoreSaleInvDAO, uDAO;

type

  { TStoreSaleInvDAO }

  TStoreSaleInvDAO = class(TPOSDAO, IStoreSaleInvDAO)
  private
    FStatement: ICMStatement;
    function GetObject(SQL: string): TStoreSaleInv;
    function GetObjects(SQL: string): TStoreSaleInvs;
    function SetProperty(ADataSet: TDataSet; Instance: TStoreSaleInv): boolean;
  public
    constructor Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
    destructor Destroy;
  public
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreSaleInv): string;
    function GetUpdateScript(Instance: TStoreSaleInv): string;
    function GetDeleteScript(Instance: TStoreSaleInv): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreSaleInv): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(AGUID: string): TStoreSaleInv; overload;
    function GetByParentGUID(AGUID: string): TStoreSaleInvs; overload;
  end;


implementation

uses
  StrUtils;

{ TSaleOrderDAO }


constructor TStoreSaleInvDAO.Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
begin
  inherited Create(AOwner, AHandler);
  FStatement := AStatement;
end;

destructor TStoreSaleInvDAO.Destroy;
begin
  inherited Destroy;
end;

function TStoreSaleInvDAO.GetObject(SQL: string): TStoreSaleInv;
var
  ADataSet: TDataSet;
begin
  Result := nil;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      Result := TStoreSaleInv.Create();
      SetProperty(ADataSet, Result);
    except
      on e: Exception do
        Messager.Error('TStoreSaleInvDAO.GetObject: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreSaleInvDAO.SetProperty(ADataSet: TDataSet; Instance: TStoreSaleInv): boolean;
begin
  Result := False;
  try
    Instance._UUID := ADataSet.FieldByName('UUID').AsString;
  except
    on e: Exception do
      Messager.Error('TStoreSaleInvDAO.SetProperty: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreSaleInvDAO.GetObjects(SQL: string): TStoreSaleInvs;
var
  Instance: TStoreSaleInv;
  ADataSet: TDataSet;
begin
  Result := TStoreSaleInvs.Create;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      while not ADataSet.EOF do
      begin
        Instance := TStoreSaleInv.Create();
        SetProperty(ADataSet, Instance);
        Result.Add(Instance);
        ADataSet.Next;
      end;
    except
      on e: Exception do
        Messager.Error('TStoreSaleInvDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreSaleInvDAO.GetInsertScript(Instance: TStoreSaleInv): string;
const
  ValueSQL: string = ' VALUES(%s,%s,%s,%s,%s,%s, %f,%f,%f,%f,%f,%d, %s,%s,%s,s, %f,%f,%f,%f, %d,%s);';
  FieldSQL: string =
    'INSERT INTO tbStoreSaleInv(CompanyCode, ShopCode, FLOWNO, POSNO, FILDATE, CASHIER, TOTAL, REALAMT, GIVEZEROAMT, DISCOUNTAMOUNT, RECCNT, SCORE, CARDCODE, MEMO, SRCUUID, CardNo, CustPayCash, FixDiscAmt, FixRealAmt, TOTALQTY, ORDERTYPE, UUID) ';
begin
  //Result := Format(FieldSQL + ValueSQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode),
  //  QuotedStr(Instance._FlowNo), QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)),
  //  QuotedStr(Instance._Cashier), Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount,
  //  Instance._RecCnt, Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreSaleInvDAO.GetUpdateScript(Instance: TStoreSaleInv): string;
const
  SQL: string =
    'UPDATE tbStoreSaleInv SET CompanyCode = %s, ShopCode = %s, FLOWNO = %s, POSNO = %s, FILDATE = %s, CASHIER = %s, TOTAL = %f, REALAMT = %f, GIVEZEROAMT = %f, DISCOUNTAMOUNT = %f, RECCNT = %f, SCORE = %s, CARDCODE = %s, MEMO = %s, SRCUUID = %s, CardNo = %s, CustPayCash = %f, FixDiscAmt = %f, FixRealAmt = %f, TOTALQTY = %f, ORDERTYPE = %d WHERE UUID = %s;';
begin
  //Result := Format(SQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode), QuotedStr(Instance._FlowNo),
  //  QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)), QuotedStr(Instance._Cashier),
  //  Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount, Instance._RecCnt,
  //  Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreSaleInvDAO.GetDeleteScript(Instance: TStoreSaleInv): string;
const
  SQL = 'DELETE tbStoreSaleInv WHERE UUID = %s;';
begin
  Result := Format(SQL, [QuotedStr(Instance._UUID)]);
end;

function TStoreSaleInvDAO.Save(Instance: TStoreSaleInv): boolean;
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

function TStoreSaleInvDAO.Remove(AValue: string): boolean;
begin
  try
    Result := FStatement.Execute(Format('DELETE tbStoreSaleInv WHERE Code = %s;', [QuotedStr(AValue)]));
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.Remove: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreSaleInvDAO.Get(AGUID: string): TStoreSaleInv;
begin
  Result := GetObject(Format('SELECT * FROM tbStoreSaleInv WHERE Code = %s;', [QuotedStr(AGUID)]));
end;

function TStoreSaleInvDAO.GetByParentGUID(AGUID: string): TStoreSaleInvs;
begin
  Result := GetObjects(Format('SELECT * FROM tbStoreSaleInv WHERE Code = %s;', [QuotedStr(AGUID)]));
end;


end.





