{

 Defining business object

 **********************************************************************}

unit uSaleBO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  cm_sysutils, cm_generics;

type

  { TSaleCommodity }

  TSaleCommodity = class(TPersistent)
  private
    FUUID: string;
    FGID: Integer;
    FBarCode: string;
    FName: string;
    FUnit: string;
    FSpecification: string;
    FPrice: Currency;
    FSettlementPrice: Currency;
    FQuantity: Currency;
    FRemark: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    constructor Create; overload;
    constructor Create(const AUUID: string); overload;
    function Equals(Obj: TObject): Boolean; override;
    property UUID: string read FUUID;
    property GID: Integer read FGID write FGID;
    property BarCode: string read FBarCode write FBarCode;
    property Name: string read FName write FName;
    property Unit_: string read FUnit write FUnit;
    property Specification: string read FSpecification write FSpecification;
    property Price: Currency read FPrice write FPrice;   //商品标价
    property SettlementPrice: Currency read FSettlementPrice write FSettlementPrice; //实际结算价
    property Quantity: Currency read FQuantity write FQuantity;
    property Remark: string read FRemark write FRemark;
  public
    function GetAmount: Currency; //总价
    function GetSettlementAmount: Currency; //结算总价
  end;

  { TSaleCommodityList }

  TSaleCommodityList = class(TGFPHashObjectList<TSaleCommodity>)
  public
    function SumAmount: Currency; //总额
    function SumSettlementAmount: Currency; //结算总额
  end;

  { TSaleBill }

  TSaleBill = class
  protected
    FUUID: string;
    FType: Byte; //单据类型0: 销售单, 1: 冲退单, 2: 提货单; 3 凭码兑奖; 11:外卖订单; 99:临时挂单
    FCommodityList: TSaleCommodityList;
  public
    constructor Create;
    destructor Destroy; override;
    property UUID: string read FUUID;
    function SumSettlementAmount: Currency; //结算总额
    property Type_: Byte read FType write FType;
  public //operate
    function AddCommodity(ASampleCommodity: TSaleCommodity): Integer; virtual; //考虑跨库，传入 ACommodity 需自行销毁
    function RemoveCommodity(const ACommodityUUID: string): Integer; virtual;
    function UpdateCommodity(ASampleCommodity: TSaleCommodity): Boolean; virtual;
    procedure NotifyUpdate(const ACommodityUUID: string); virtual; abstract; //改变某一 commodity 的属性时调用
    procedure Clear; virtual;
  public //find
    function ExistCommodity(const ACommodityUUID: string): Boolean;
    function FindCommodity(const ACommodityUUID: string): TSaleCommodity;
    function CommodityCount: Integer;
    function GetCommodity(AIndex: Integer): TSaleCommodity;
  end;


implementation

{TSaleCommodity}

procedure TSaleCommodity.AssignTo(Dest: TPersistent);
begin
  if (Dest.UnitName + '.' + Dest.ClassName) = (Self.UnitName + '.TSaleCommodity') then //考虑不同库中的情况
    begin
      //Do not operate UUID
      TSaleCommodity(Dest).FGID := Self.FGID;
      TSaleCommodity(Dest).FBarCode := Self.FBarCode;
      TSaleCommodity(Dest).FName := Self.FName;
      TSaleCommodity(Dest).FUnit := Self.FUnit;
      TSaleCommodity(Dest).FSpecification := Self.FSpecification;
      TSaleCommodity(Dest).FPrice := Self.FPrice;
      TSaleCommodity(Dest).FQuantity := Self.FQuantity;
      TSaleCommodity(Dest).FSettlementPrice := Self.FSettlementPrice;
      TSaleCommodity(Dest).FRemark := Self.FRemark;
    end;
end;

constructor TSaleCommodity.Create;
begin
  FUUID := CreateGUIDStr;
end;

constructor TSaleCommodity.Create(const AUUID: string);
begin
  FUUID := AUUID;
end;

function TSaleCommodity.Equals(Obj: TObject): Boolean;
begin
  Result := False;
  if (Obj.UnitName + '.' + Obj.ClassName) = (Self.UnitName + '.TSaleCommodity') then
    Result := TSaleCommodity(Obj).UUID = Self.UUID;
end;

function TSaleCommodity.GetAmount: Currency;
begin
  Result := Price * Quantity;
end;

function TSaleCommodity.GetSettlementAmount: Currency;
begin
  Result := SettlementPrice * Quantity;
end;

{ TSaleCommodityList }

function TSaleCommodityList.SumAmount: Currency;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Self.Count - 1 do
    Result := Result + Items[i].GetAmount;
end;

function TSaleCommodityList.SumSettlementAmount: Currency;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Self.Count - 1 do
    Result := Result + Items[i].GetSettlementAmount;
end;

{ TSaleBill }

constructor TSaleBill.Create;
begin
  FUUID := CreateGUIDStr;
  FType := 0;
  FCommodityList := TSaleCommodityList.Create(True);
end;

destructor TSaleBill.Destroy;
begin
  FCommodityList.Free;
  inherited Destroy;
end;

function TSaleBill.SumSettlementAmount: Currency;
begin
  Result := FCommodityList.SumSettlementAmount;
end;

function TSaleBill.AddCommodity(ASampleCommodity: TSaleCommodity): Integer;
var
  c: TSaleCommodity;
begin
  Result := -1;
  c := TSaleCommodity.Create(ASampleCommodity.UUID);
  c.Assign(ASampleCommodity);
  Result := FCommodityList.Add(c.UUID, c);
end;

function TSaleBill.RemoveCommodity(const ACommodityUUID: string): Integer;
var
  c: TSaleCommodity;
begin
  Result := -1;
  c := FCommodityList.Find(ACommodityUUID);
  if Assigned(c) then
    Result :=  FCommodityList.Remove(c);
end;

function TSaleBill.UpdateCommodity(ASampleCommodity: TSaleCommodity): Boolean;
var
  c: TSaleCommodity;
begin
  Result := False;
  c := FCommodityList.Find(ASampleCommodity.UUID);
  if Assigned(c) then
    begin
      c.Assign(ASampleCommodity);
      Result :=  True;
    end;
end;

procedure TSaleBill.Clear;
begin
  FCommodityList.Clear;
end;

function TSaleBill.ExistCommodity(const ACommodityUUID: string): Boolean;
begin
  Result := FCommodityList.FindIndexOf(ACommodityUUID) >= 0;
end;

function TSaleBill.FindCommodity(const ACommodityUUID: string): TSaleCommodity;
begin
  Result := TSaleCommodity(FCommodityList.Find(ACommodityUUID));
end;

function TSaleBill.CommodityCount: Integer;
begin
  Result := FCommodityList.Count;
end;

function TSaleBill.GetCommodity(AIndex: Integer): TSaleCommodity;
begin
  Result := nil;
  if AIndex < FCommodityList.Count then
    Result := FCommodityList[AIndex];
end;




end.


