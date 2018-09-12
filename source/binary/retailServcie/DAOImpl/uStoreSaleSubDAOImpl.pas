unit uStoreSaleSubDAOImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, cm_messager, DB, cm_DB, uStoreSaleOrder, uStoreSaleSubDAO, uDAO;

type

  { TStoreSaleSubDAO }

  TStoreSaleSubDAO = class(TPOSDAO, IStoreSaleSubDAO)
  private
    FStatement: ICMStatement;
    function GetObject(SQL: string): TStoreSaleSub;
    function GetObjects(SQL: string): TStoreSaleSubs;
    function SetProperty(ADataSet: TDataSet; Instance: TStoreSaleSub): boolean;
  public
    constructor Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
    destructor Destroy; override;
  public
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreSaleSub): string;
    function GetUpdateScript(Instance: TStoreSaleSub): string;
    function GetDeleteScript(Instance: TStoreSaleSub): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreSaleSub): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(AGUID: string): TStoreSaleSub; overload;
    function GetByParentGUID(AGUID: string): TStoreSaleSubs; overload;
  end;


implementation

uses
  StrUtils;

{ TSaleOrderDAO }


constructor TStoreSaleSubDAO.Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
begin
  inherited Create(AOwner, AHandler);
  FStatement := AStatement;
end;

destructor TStoreSaleSubDAO.Destroy;
begin
  inherited Destroy;
end;

function TStoreSaleSubDAO.GetObject(SQL: string): TStoreSaleSub;
var
  ADataSet: TDataSet;
begin
  Result := nil;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      Result := TStoreSaleSub.Create();
      SetProperty(ADataSet, Result);
    except
      on e: Exception do
        Messager.Error('TStoreSaleSubDAO.GetObject: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreSaleSubDAO.SetProperty(ADataSet: TDataSet; Instance: TStoreSaleSub): boolean;
begin
  Result := False;
  try
    Instance._ParentGUID := ADataSet.FieldByName('UUID').AsString;
    Instance._ITEMNO := ADataSet.FieldByName('ITEMNO').AsInteger;
    Instance._GID := ADataSet.FieldByName('GID').AsInteger;             //商品GID =>商品条码     ???
    Instance._GDCODE := ADataSet.FieldByName('GDCODE').AsString;          //条码
    Instance._QTY := ADataSet.FieldByName('QTY').AsFloat;             //数量
    Instance._PRICE := ADataSet.FieldByName('PRICE').AsFloat;           // 折后价格 零售价
    Instance._ProdPrice := ADataSet.FieldByName('ProdPrice').AsFloat;       //原价
    Instance._AvgPrice := ADataSet.FieldByName('AvgPrice').AsFloat;       //分滩价格
    Instance._SRCITEMNO := ADataSet.FieldByName('SRCITEMNO').AsInteger;        //退单商品的原itemno
    Instance._IsChangePrice := ADataSet.FieldByName('IsChangePrice').AsBoolean;    //是否改价格
    Instance._InputBarCode := ADataSet.FieldByName('InputBarCode').AsString;       //输入码

    Instance._GUID := ADataSet.FieldByName('GUID').AsString;
  except
    on e: Exception do
      Messager.Error('TStoreSaleSubDAO.SetProperty: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreSaleSubDAO.GetObjects(SQL: string): TStoreSaleSubs;
var
  Instance: TStoreSaleSub;
  ADataSet: TDataSet;
begin
  Result := TStoreSaleSubs.Create;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      while not ADataSet.EOF do
      begin
        Instance := TStoreSaleSub.Create();
        SetProperty(ADataSet, Instance);
        Result.Add(Instance);
        ADataSet.Next;
      end;
    except
      on e: Exception do
        Messager.Error('TStoreSaleSubDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreSaleSubDAO.GetInsertScript(Instance: TStoreSaleSub): string;
const
  ValueSQL: string = ' VALUES(%s,%s,%s,%s,%s,%s, %f,%f,%f,%f,%f,%d, %s,%s,%s,s, %f,%f,%f,%f, %d,%s);';
  FieldSQL: string =
    'INSERT INTO tbStoreSaleSub(CompanyCode, ShopCode, FLOWNO, POSNO, FILDATE, CASHIER, TOTAL, REALAMT, GIVEZEROAMT, DISCOUNTAMOUNT, RECCNT, SCORE, CARDCODE, MEMO, SRCUUID, CardNo, CustPayCash, FixDiscAmt, FixRealAmt, TOTALQTY, ORDERTYPE, UUID) ';
begin
  //Result := Format(FieldSQL + ValueSQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode),
  //  QuotedStr(Instance._FlowNo), QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)),
  //  QuotedStr(Instance._Cashier), Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount,
  //  Instance._RecCnt, Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreSaleSubDAO.GetUpdateScript(Instance: TStoreSaleSub): string;
const
  SQL: string =
    'UPDATE tbStoreSaleSub SET CompanyCode = %s, ShopCode = %s, FLOWNO = %s, POSNO = %s, FILDATE = %s, CASHIER = %s, TOTAL = %f, REALAMT = %f, GIVEZEROAMT = %f, DISCOUNTAMOUNT = %f, RECCNT = %f, SCORE = %s, CARDCODE = %s, MEMO = %s, SRCUUID = %s, CardNo = %s, CustPayCash = %f, FixDiscAmt = %f, FixRealAmt = %f, TOTALQTY = %f, ORDERTYPE = %d WHERE UUID = %s;';
begin
  //Result := Format(SQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode), QuotedStr(Instance._FlowNo),
  //  QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)), QuotedStr(Instance._Cashier),
  //  Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount, Instance._RecCnt,
  //  Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreSaleSubDAO.GetDeleteScript(Instance: TStoreSaleSub): string;
begin
  Result := '';
end;

function TStoreSaleSubDAO.Save(Instance: TStoreSaleSub): boolean;
var
  Guid: TGuid;
begin
  try
    if (Instance._GUID = '') then
    begin
      CreateGUID(Guid);
      Instance._GUID := ReplaceStr(ReplaceStr(GUIDToString(Guid), '{', ''), '}', '');
      Result := FStatement.Execute(GetInsertScript(Instance));
    end
    else
      Result := FStatement.Execute(GetUpdateScript(Instance))
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.Save: %s %s', [e.ClassName, e.Message]);
  end;

end;

function TStoreSaleSubDAO.Remove(AValue: string): boolean;
begin
  try
    Result := FStatement.Execute(Format('DELETE tbStoreSaleSub WHERE Code = %s;', [QuotedStr(AValue)]));
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.Remove: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreSaleSubDAO.Get(AGUID: string): TStoreSaleSub;
begin
  Result := GetObject(Format('SELECT * FROM tbStoreSaleSub WHERE UUID = %s;', [QuotedStr(AGUID)]));
end;

function TStoreSaleSubDAO.GetByParentGUID(AGUID: string): TStoreSaleSubs;
begin
  Result := GetObjects(Format('SELECT * FROM tbStoreSaleSub WHERE Code = %s;', [QuotedStr(AGUID)]));
end;


end.





