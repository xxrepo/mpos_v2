unit uShopProductDAOImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, cm_messager, DB, cm_DB, uStoreSaleOrder, uShopProductDAO, uDAO;

type
  { TShopProductDAO }

  TShopProductDAO = class(TPOSDAO, IShopProductDAO)
  private
    FStatement: ICMStatement;
    function GetObject(SQL: string): TShopProduct;
    function GetObjects(SQL: string): TShopProducts;
    function SetProperty(ADataSet: TDataSet; Instance: TShopProduct): boolean;
  public
    constructor Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
    destructor Destroy;
  public
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TShopProduct): string;
    function GetUpdateScript(Instance: TShopProduct): string;
    function GetDeleteScript(Instance: TShopProduct): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TShopProduct): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function GetByGID(AGID: integer): TShopProduct; overload;
    function GetByGDCode(AGDCode: string): TShopProduct; overload;
  end;


implementation

uses
  StrUtils;

{ TSaleOrderDAO }


constructor TShopProductDAO.Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
begin
  inherited Create(AOwner, AHandler);
  FStatement := AStatement;
end;

destructor TShopProductDAO.Destroy;
begin
  inherited Destroy;
end;

function TShopProductDAO.GetObject(SQL: string): TShopProduct;
var
  ADataSet: TDataSet;
begin
  Result := nil;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      Result := TShopProduct.Create();
      SetProperty(ADataSet, Result);
    except
      on e: Exception do
        Messager.Error('TShopProductDAO.GetObject: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TShopProductDAO.SetProperty(ADataSet: TDataSet; Instance: TShopProduct): boolean;
begin
  Result := False;
  try
    Instance._GID := ADataSet.FieldByName('GID').AsInteger;
    Instance._BarCode := ADataSet.FieldByName('GID').AsString;
    Instance._IsBigPack := ADataSet.FieldByName('GID').AsBoolean;
    Instance._IsDisp := ADataSet.FieldByName('GID').AsInteger;
    Instance._MemberPrice := ADataSet.FieldByName('GID').AsFloat;
    Instance._PrcType := ADataSet.FieldByName('GID').AsInteger;
    Instance._ProductCode := ADataSet.FieldByName('GID').AsString;
    Instance._ProductID := ADataSet.FieldByName('GID').AsInteger;
    Instance._ProductName := ADataSet.FieldByName('GID').AsString;
    Instance._Py := ADataSet.FieldByName('GID').AsString;
    Instance._RelativeQty := ADataSet.FieldByName('GID').AsInteger;
    Instance._Remark := ADataSet.FieldByName('GID').AsString;
    Instance._RetailPrice := ADataSet.FieldByName('GID').AsFloat;
    Instance._ShortCode := ADataSet.FieldByName('GID').AsString;
    Instance._ShortName := ADataSet.FieldByName('GID').AsString;
    Instance._SortCode := ADataSet.FieldByName('GID').AsString;
    Instance._SortName := ADataSet.FieldByName('GID').AsString;
    Instance._SPEC := ADataSet.FieldByName('GID').AsString;
    Instance._SPProductID := ADataSet.FieldByName('GID').AsString;
    Instance._Status := ADataSet.FieldByName('GID').AsString;
    Instance._Unit := ADataSet.FieldByName('GID').AsString;
    Instance._UpdateState := ADataSet.FieldByName('GID').AsInteger;
  except
    on e: Exception do
      Messager.Error('TShopProductDAO.SetProperty: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TShopProductDAO.GetObjects(SQL: string): TShopProducts;
var
  Instance: TShopProduct;
  ADataSet: TDataSet;
begin
  Result := TShopProducts.Create;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      while not ADataSet.EOF do
      begin
        Instance := TShopProduct.Create();
        SetProperty(ADataSet, Instance);
        Result.Add(Instance);
        ADataSet.Next;
      end;
    except
      on e: Exception do
        Messager.Error('TShopProductDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TShopProductDAO.GetInsertScript(Instance: TShopProduct): string;
const
  ValueSQL: string = ' VALUES(%s,%s,%s,%s,%s,%s, %f,%f,%f,%f,%f,%d, %s,%s,%s,s, %f,%f,%f,%f, %d,%s);';
  FieldSQL: string =
    'INSERT INTO tbShopProduct(CompanyCode, ShopCode, FLOWNO, POSNO, FILDATE, CASHIER, TOTAL, REALAMT, GIVEZEROAMT, DISCOUNTAMOUNT, RECCNT, SCORE, CARDCODE, MEMO, SRCUUID, CardNo, CustPayCash, FixDiscAmt, FixRealAmt, TOTALQTY, ORDERTYPE, UUID) ';
begin
  //Result := Format(FieldSQL + ValueSQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode),
  //  QuotedStr(Instance._FlowNo), QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)),
  //  QuotedStr(Instance._Cashier), Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount,
  //  Instance._RecCnt, Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TShopProductDAO.GetUpdateScript(Instance: TShopProduct): string;
const
  SQL: string =
    'UPDATE tbShopProduct SET CompanyCode = %s, ShopCode = %s, FLOWNO = %s, POSNO = %s, FILDATE = %s, CASHIER = %s, TOTAL = %f, REALAMT = %f, GIVEZEROAMT = %f, DISCOUNTAMOUNT = %f, RECCNT = %f, SCORE = %s, CARDCODE = %s, MEMO = %s, SRCUUID = %s, CardNo = %s, CustPayCash = %f, FixDiscAmt = %f, FixRealAmt = %f, TOTALQTY = %f, ORDERTYPE = %d WHERE UUID = %s;';
begin
  //Result := Format(SQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode), QuotedStr(Instance._FlowNo),
  //  QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)), QuotedStr(Instance._Cashier),
  //  Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount, Instance._RecCnt,
  //  Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TShopProductDAO.GetDeleteScript(Instance: TShopProduct): string;
const
  SQL = 'DELETE tbStoreSalePromotion WHERE ProductCode = %s;';
begin
  Result := Format(SQL, [QuotedStr(Instance._ProductCode)]);
end;

function TShopProductDAO.Save(Instance: TShopProduct): boolean;
var
  Guid: TGuid;
begin
  try
    if (Instance._ProductCode = '') then
    begin
      CreateGUID(Guid);
      Instance._ProductCode := ReplaceStr(ReplaceStr(GUIDToString(Guid), '{', ''), '}', '');
      Result := FStatement.Execute(GetInsertScript(Instance));
    end
    else
      Result := FStatement.Execute(GetUpdateScript(Instance))
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.Save: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TShopProductDAO.Remove(AValue: string): boolean;
begin
  try
    Result := FStatement.Execute(Format('DELETE tbShopProduct WHERE Code = %s;', [QuotedStr(AValue)]));
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.Remove: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TShopProductDAO.GetByGID(AGID: integer): TShopProduct;
begin
  Result := GetObject(Format('SELECT * FROM tbShopProduct WHERE GID = %d;', [AGID]));
end;

function TShopProductDAO.GetByGDCode(AGDCode: string): TShopProduct;
begin
  Result := GetObject(Format('SELECT * FROM tbShopProduct WHERE GDCode = %s;', [QuotedStr(AGDCode)]));
end;


end.




