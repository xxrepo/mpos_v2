{

 Defining data transfer object

 **********************************************************************}

unit uSaleDTO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  cm_sysutils, cm_generics;

type

  { TShowItem }//显示项目对象

  TShowItem = class(TPersistent)
  private
    FUUID: string;
    FBarCode: string;
    FName: string;
    FUnit: string;
    FPrice: currency;
    FQuantity: currency;
    FRemark: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload;
    constructor Create(const AUUID: string); overload;
    function Equals(Obj: TObject): boolean; override;
    property UUID: string read FUUID;
    property BarCode: string read FBarCode write FBarCode;
    property Name: string read FName write FName;
    property Unit_: string read FUnit write FUnit;
    property Price: currency read FPrice write FPrice;
    property Quantity: currency read FQuantity write FQuantity;
    property Remark: string read FRemark write FRemark;
  public
    function GetAmount: currency;
  end;

  TShowItemList = class(TGFPHashObjectList<TShowItem>)
  public
    function SumAmount: currency;
  end;


  { TSaleQueryShowItem }

  TSaleQueryShowItem = class(TPersistent)
  private
    FSettlementTime: TDatetime;
    FUploadTime: TDatetime;
    FUUID: string;
    FOrderNo: string;
    FType: byte;
    FQuantity: currency;
    FSettlementAmount: currency;
    FRemark: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload;
    constructor Create(const AUUID: string); overload;
    function Equals(Obj: TObject): boolean; override;
    property UUID: string read FUUID;
    property OrderNo: string read FOrderNo write FOrderNo;
    property Type_: byte read FType write FType;
    property Quantity: currency read FQuantity write FQuantity;
    property SettlementAmount: currency read FSettlementAmount write FSettlementAmount;
    property SettlementTime: TDatetime read FSettlementTime write FSettlementTime;
    property UploadTime: TDatetime read FUploadTime write FUploadTime;
    property Remark: string read FRemark write FRemark;
  public
    function GetAmount: currency;
  end;

  { TSaleQueryShowItemList }

  TSaleQueryShowItemList = class(TGFPHashObjectList<TSaleQueryShowItem>)
  public
    function SumAmount: currency;
  end;

implementation

{ TSaleQueryShowItemList }

function TSaleQueryShowItemList.SumAmount: currency;
begin

end;

{ TSaleQueryShowItem }

procedure TSaleQueryShowItem.AssignTo(Dest: TPersistent);
begin
  if (Dest.UnitName + '.' + Dest.ClassName) = (Self.UnitName + '.TSaleQueryShowItem') then //考虑不同库中的情况
  begin
    TSaleQueryShowItem(Dest).FUUID := Self.FUUID;
    TSaleQueryShowItem(Dest).FOrderNo := Self.FOrderNo;
    TSaleQueryShowItem(Dest).Type_ := Self.Type_;
    TSaleQueryShowItem(Dest).FQuantity := Self.FQuantity;
    TSaleQueryShowItem(Dest).FSettlementAmount := Self.FSettlementAmount;
    TSaleQueryShowItem(Dest).FSettlementTime := Self.FSettlementTime;
    TSaleQueryShowItem(Dest).FRemark := Self.FRemark;
  end;
end;

constructor TSaleQueryShowItem.Create;
begin

end;

constructor TSaleQueryShowItem.Create(const AUUID: string);
begin

end;

function TSaleQueryShowItem.Equals(Obj: TObject): boolean;
begin
  Result := inherited Equals(Obj);
end;

function TSaleQueryShowItem.GetAmount: currency;
begin

end;

{ TShowItem }

procedure TShowItem.AssignTo(Dest: TPersistent);
begin
  if (Dest.UnitName + '.' + Dest.ClassName) = (Self.UnitName + '.TShowItem') then //考虑不同库中的情况
  begin
    TShowItem(Dest).FUUID := Self.FUUID;
    TShowItem(Dest).FBarCode := Self.FBarCode;
    TShowItem(Dest).FName := Self.FName;
    TShowItem(Dest).FUnit := Self.FUnit;
    TShowItem(Dest).FPrice := Self.FPrice;
    TShowItem(Dest).FQuantity := Self.FQuantity;
    TShowItem(Dest).FRemark := Self.FRemark;
  end;
end;

constructor TShowItem.Create;
begin
  FUUID := CreateGUIDStr;
end;

constructor TShowItem.Create(const AUUID: string);
begin
  FUUID := AUUID;
end;

function TShowItem.Equals(Obj: TObject): boolean;
begin
  Result := False;
  if (Obj.UnitName + '.' + Obj.ClassName) = (Self.UnitName + '.TShowItem') then
    Result := TShowItem(Obj).UUID = Self.UUID;
end;

function TShowItem.GetAmount: currency;
begin
  Result := FPrice * FQuantity;
end;

{ TShowItemList }

function TShowItemList.SumAmount: currency;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Self.Count - 1 do
    Result := Result + Items[i].GetAmount;
end;

end.

