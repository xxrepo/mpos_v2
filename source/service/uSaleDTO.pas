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

  { TShowItem } //显示项目对象

  TShowItem = class(TPersistent)
  private
    FUUID: string;
    FBarCode: string;
    FName: string;
    FUnit: string;
    FPrice: Currency;
    FQuantity: Currency;
    FRemark: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload;
    constructor Create(const AUUID: string); overload;
    function Equals(Obj: TObject): Boolean; override;
    property UUID: string read FUUID;
    property BarCode: string read FBarCode write FBarCode;
    property Name: string read FName write FName;
    property Unit_: string read FUnit write FUnit;
    property Price: Currency read FPrice write FPrice;
    property Quantity: Currency read FQuantity write FQuantity;
    property Remark: string read FRemark write FRemark;
  public
    function GetAmount: Currency;
  end;

  TShowItemList = class(TGFPHashObjectList<TShowItem>)
  public
    function SumAmount: Currency;
  end;

implementation

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

function TShowItem.Equals(Obj: TObject): Boolean;
begin
  Result := False;
  if (Obj.UnitName + '.' + Obj.ClassName) = (Self.UnitName + '.TShowItem') then
    Result := TShowItem(Obj).UUID = Self.UUID;
end;

function TShowItem.GetAmount: Currency;
begin
  Result := FPrice * FQuantity;
end;

{ TShowItemList }

function TShowItemList.SumAmount: Currency;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Self.Count - 1 do
    Result := Result + Items[i].GetAmount;
end;

end.

