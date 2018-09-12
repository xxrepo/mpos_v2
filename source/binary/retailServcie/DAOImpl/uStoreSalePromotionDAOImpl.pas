unit uStoreSalePromotionDAOImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, cm_messager, DB, cm_DB, uStoreSaleOrder, uStoreSalePromotionDAO, uDAO;

type

  { TStoreSalePromotionDAO }

  TStoreSalePromotionDAO = class(TPOSDAO, IStoreSalePromotionDAO)
  private
    FStatement: ICMStatement;
    function GetObject(SQL: string): TStoreSalePromotion;
    function GetObjects(SQL: string): TStoreSalePromotions;
    function SetProperty(ADataSet: TDataSet; Instance: TStoreSalePromotion): boolean;
  public
    constructor Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
    destructor Destroy;
  public
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreSalePromotion): string;
    function GetUpdateScript(Instance: TStoreSalePromotion): string;
    function GetDeleteScript(Instance: TStoreSalePromotion): string;

    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreSalePromotion): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(AGUID: string): TStoreSalePromotion; overload;
    function GetByParentGUID(AGUID: string): TStoreSalePromotions; overload;
  end;


implementation

uses
  StrUtils;

{ TSaleOrderDAO }


constructor TStoreSalePromotionDAO.Create(AOwner: TComponent; AHandler: ICMMessageHandler; AStatement: ICMStatement);
begin
  inherited Create(AOwner, AHandler);
  FStatement := AStatement;
end;

destructor TStoreSalePromotionDAO.Destroy;
begin
  inherited Destroy;
end;

function TStoreSalePromotionDAO.GetObject(SQL: string): TStoreSalePromotion;
var
  ADataSet: TDataSet;
begin
  Result := nil;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      Result := TStoreSalePromotion.Create();
      SetProperty(ADataSet, Result);
    except
      on e: Exception do
        Messager.Error('TStoreSalePromotionDAO.GetObject: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreSalePromotionDAO.SetProperty(ADataSet: TDataSet; Instance: TStoreSalePromotion): boolean;
begin
  Result := False;
  try
    Instance._UUID := ADataSet.FieldByName('UUID').AsString;
  except
    on e: Exception do
      Messager.Error('TStoreSalePromotionDAO.SetProperty: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreSalePromotionDAO.GetObjects(SQL: string): TStoreSalePromotions;
var
  Instance: TStoreSalePromotion;
  ADataSet: TDataSet;
begin
  Result := TStoreSalePromotions.Create;
  try
    try
      ADataSet := FStatement.Query(SQL);
      if ADataSet.IsEmpty then
        Exit;

      while not ADataSet.EOF do
      begin
        Instance := TStoreSalePromotion.Create();
        SetProperty(ADataSet, Instance);
        Result.Add(Instance);
        ADataSet.Next;
      end;
    except
      on e: Exception do
        Messager.Error('TStoreSalePromotionDAO.GetObjects: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TStoreSalePromotionDAO.GetInsertScript(Instance: TStoreSalePromotion): string;
const
  ValueSQL: string = ' VALUES(%s,%s,%s,%s,%s,%s, %f,%f,%f,%f,%f,%d, %s,%s,%s,s, %f,%f,%f,%f, %d,%s);';
  FieldSQL: string =
    'INSERT INTO tbStoreSalePromotion(CompanyCode, ShopCode, FLOWNO, POSNO, FILDATE, CASHIER, TOTAL, REALAMT, GIVEZEROAMT, DISCOUNTAMOUNT, RECCNT, SCORE, CARDCODE, MEMO, SRCUUID, CardNo, CustPayCash, FixDiscAmt, FixRealAmt, TOTALQTY, ORDERTYPE, UUID) ';
begin
  //Result := Format(FieldSQL + ValueSQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode),
  //  QuotedStr(Instance._FlowNo), QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)),
  //  QuotedStr(Instance._Cashier), Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount,
  //  Instance._RecCnt, Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreSalePromotionDAO.GetUpdateScript(Instance: TStoreSalePromotion): string;
const
  SQL: string =
    'UPDATE tbStoreSalePromotion SET CompanyCode = %s, ShopCode = %s, FLOWNO = %s, POSNO = %s, FILDATE = %s, CASHIER = %s, TOTAL = %f, REALAMT = %f, GIVEZEROAMT = %f, DISCOUNTAMOUNT = %f, RECCNT = %f, SCORE = %s, CARDCODE = %s, MEMO = %s, SRCUUID = %s, CardNo = %s, CustPayCash = %f, FixDiscAmt = %f, FixRealAmt = %f, TOTALQTY = %f, ORDERTYPE = %d WHERE UUID = %s;';
begin
  //Result := Format(SQL, [QuotedStr(Instance._CompanyCode), QuotedStr(Instance._CompanyCode), QuotedStr(Instance._FlowNo),
  //  QuotedStr(Instance._PosNo), QuotedStr(FormatDatetime('yyyy-MM-dd HH:mm:ss', Instance._FilDate)), QuotedStr(Instance._Cashier),
  //  Instance._Total, Instance._RealAmt, Instance._GiveZeroAmt, Instance._DiscountAmount, Instance._RecCnt,
  //  Instance._SCore, QuotedStr(Instance._CardCode), QuotedStr(Instance._Memo), QuotedStr(Instance._SrcUUID),
  //  QuotedStr(Instance._CardNo), Instance._CustPayCash, Instance._FixDiscAmt, Instance._FixRealAmt, Instance._TotalQty,
  //  Instance._OrderType, QuotedStr(Instance._UUID)]);
end;

function TStoreSalePromotionDAO.GetDeleteScript(Instance: TStoreSalePromotion): string;
const
  SQL = 'DELETE tbStoreSalePromotion WHERE UUID = %s;';
begin
  Result := Format(SQL, [QuotedStr(Instance._UUID)]);
end;

function TStoreSalePromotionDAO.Save(Instance: TStoreSalePromotion): boolean;
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

function TStoreSalePromotionDAO.Remove(AValue: string): boolean;
begin
  try
    Result := FStatement.Execute(Format('DELETE tbStoreSalePromotion WHERE Code = %s;', [QuotedStr(AValue)]));
  except
    on e: Exception do
      Messager.Error('TSaleOrderDAO.Remove: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TStoreSalePromotionDAO.Get(AGUID: string): TStoreSalePromotion;
begin
  Result := GetObject(Format('SELECT * FROM tbStoreSalePromotion WHERE Code = %s;', [QuotedStr(AGUID)]));
end;

function TStoreSalePromotionDAO.GetByParentGUID(AGUID: string): TStoreSalePromotions;
begin
  Result := GetObjects(Format('SELECT * FROM tbStoreSalePromotion WHERE Code = %s;', [QuotedStr(AGUID)]));
end;


end.





