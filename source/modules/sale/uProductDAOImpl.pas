unit uProductDAOImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DB,
  cm_messager, cm_DB,
  uDAO, uSaleDAO, uProductPO;

type
  { TProductDAO }

  TProductDAO = class(TPOSDAO, IProductDAO)
  private
    function GetObject(SQL: string): TProduct;
    function GetObjects(SQL: string): TProductList;
    function SetProperty(ADataSet: TDataSet; Instance: TProduct): boolean;
  public
    function GetByGID(AGID: integer): TProduct;
    function GetByGDCode(const AGDCode: string): TProduct;
    function GetByBarCode(const ABarCode: string): TProduct;
    function FindBySortCode(const ASortCode: string): TProductList;
    function FindByInputCode(const AInputCode: string): TProductList;
  end;

implementation

{ TProductDAO }

function TProductDAO.SetProperty(ADataSet: TDataSet; Instance: TProduct): boolean;
begin
  Result := False;
  try
    //Messager.Info('SetProperty: ...begin...');
    Instance.GID := ADataSet.FieldByName('GID').AsInteger;
    Instance.ProductCode := ADataSet.FieldByName('ProductCode').AsString;
    Instance.ShortCode := ADataSet.FieldByName('ShortCode').AsString;
    Instance.ProductName := ADataSet.FieldByName('ProductName').AsString;
    Instance.ShortName := ADataSet.FieldByName('ShortName').AsString;
    Instance.SortCode := ADataSet.FieldByName('SortCode').AsString;
    Instance.SortName := ADataSet.FieldByName('SortName').AsString;
    Instance.BarCode := ADataSet.FieldByName('BarCode').AsString;
    Instance.SPEC := ADataSet.FieldByName('SPEC').AsString;
    Instance.Unit_ := ADataSet.FieldByName('Unit').AsString;
    Instance.RetailPrice := ADataSet.FieldByName('RetailPrice').AsCurrency;
    Instance.Remark := ADataSet.FieldByName('Remark').AsString;
    Instance.Status := ADataSet.FieldByName('Status').AsString;
    Instance.PrcType := ADataSet.FieldByName('PrcType').AsInteger;
    Instance.ProductID := ADataSet.FieldByName('ProductID').AsInteger;
    Instance.MemberPrice := ADataSet.FieldByName('MemberPrice').AsCurrency;
    Instance.IsBigPack := ADataSet.FieldByName('IsBigPack').AsBoolean;
    Instance.IsDisp := ADataSet.FieldByName('IsDisp').AsInteger;
    Instance.IsLtd := ADataSet.FieldByName('IsLtd').AsInteger;
    Instance.SPProductID := ADataSet.FieldByName('SPProductID').AsString;
    Instance.RelativeQty := ADataSet.FieldByName('RelativeQty').AsInteger;
    Instance.Py := ADataSet.FieldByName('Py').AsString;
    Instance.UpdateState := ADataSet.FieldByName('UpdateState').AsInteger;
    //Messager.Info('SetProperty: ...end...');
  except
    on e: Exception do
      Messager.Error('SetProperty: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TProductDAO.GetObject(SQL: string): TProduct;
var
  ds: TDataSet;
  Instance: TProduct;
begin
  Result := nil;
  try
    ds := GetDBHelper.Query(SQL);
    if Assigned(ds) then
    begin
      if not ds.IsEmpty then
      begin
        Instance := TProduct.Create();
        SetProperty(ds, Instance);
        Result := Instance;
      end;
      ds.Free;
    end;
  except
    on e: Exception do
      Messager.Error('GetObject: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TProductDAO.GetObjects(SQL: string): TProductList;
var
  Instance: TProduct;
  ds: TDataSet;
begin
  Result := TProductList.Create;
  try
    ds := GetDBHelper.Query(SQL);
    if assigned(ds) then
    begin
      while not ds.EOF do
      begin
        Instance := TProduct.Create();
        SetProperty(ds, Instance);
        Result.Add(Instance);
        ds.Next;
      end;
      ds.Free;
    end;
  except
    on e: Exception do
      Messager.Error('GetObjects: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TProductDAO.GetByGID(AGID: integer): TProduct;
begin
  Result := GetObject(Format('SELECT * FROM tbShopProduct WHERE GID = %d COLLATE NOCASE;', [AGID]));
end;

function TProductDAO.GetByGDCode(const AGDCode: string): TProduct;
begin
  Result := GetObject(Format('SELECT * FROM tbShopProduct WHERE GDCode = %s COLLATE NOCASE;', [QuotedStr(AGDCode)]));
end;

function TProductDAO.GetByBarCode(const ABarCode: string): TProduct;
begin
  Result := GetObject(Format('SELECT * FROM tbShopProduct WHERE BarCode = %s COLLATE NOCASE;', [QuotedStr(ABarCode)]));
end;

function TProductDAO.FindBySortCode(const ASortCode: string): TProductList;
begin
  Result := GetObjects(Format('SELECT * FROM tbShopProduct WHERE SortCode = %s COLLATE NOCASE;', [QuotedStr(ASortCode)]));
end;

function TProductDAO.FindByInputCode(const AInputCode: string): TProductList;
const
  SQLFmt = 'SELECT * FROM tbShopProduct WHERE IFNULL(ProductCode,'''') LIKE %s or IFNULL(ProductName,'''') LIKE %s or IFNULL(barcode,'''') LIKE %s LIMIT 10 COLLATE NOCASE;';
var
  qryStr: string;
begin
  qryStr := QuotedStr('%' + AInputCode + '%');
  Result := GetObjects(Format(SQLFmt, [qryStr, qryStr, qryStr]));
end;

end.






