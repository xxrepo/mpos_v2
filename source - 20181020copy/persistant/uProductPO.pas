unit uProductPO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TSimpleTbShopProduct = class(TPersistent)
  private
    FBarCode: string;
    FGID: Integer;
    FIsBigPack: Boolean;
    FIsDisp: Word;
    FMemberPrice: string;
    FPrcType: Word;
    FProductCode: string;
    FProductID: Integer;
    FProductName: string;
    FPy: string;
    FRelativeQty: Integer;
    FRemark: string;
    FRetailPrice: Currency;
    //FShortCode: string;
    FShortName: string;
    FSortCode: string;
    //FSortName: string;
    FSPEC: string;
    FSPProductID: string;
    FStatus: string;
    FUnit: string;
    FUpdateState: Integer;
  published
    property GID: Integer read FGID write FGID;
    property ProductCode: string read FProductCode write FProductCode;
    property ProductName: string read FProductName write FProductName;
    property ShortName: string read FShortName write FShortName;
    property SortCode: string read FSortCode write FSortCode;
    property BarCode: string read FBarCode write FBarCode;
    property SPEC: string read FSPEC write FSPEC;
    property Unit_: string read FUnit write FUnit;
    property RetailPrice: Currency read FRetailPrice write FRetailPrice;
    property Remark: string read FRemark write FRemark;
    property Status: string read FStatus write FStatus;
    property PrcType: Word read FPrcType write FPrcType;
    property ProductID: Integer read FProductID write FProductID;
    property MemberPrice: string read FMemberPrice write FMemberPrice;
    property IsBigPack: Boolean read FIsBigPack write FIsBigPack;
    property IsDisp: Word read FIsDisp write FIsDisp;
    property SPProductID: string read FSPProductID write FSPProductID;
    property RelativeQty: Integer read FRelativeQty write FRelativeQty;
    property Py: string read FPy write FPy;
    property UpdateState: Integer read FUpdateState write FUpdateState;
  end;

  { TTbShopProduct }

  TTbShopProduct = class(TPersistent)
  private
    FBarCode: string;
    FGID: Integer;
    FIsBigPack: Boolean;
    FIsDisp: Word;
    FMemberPrice: string;
    FPrcType: Word;
    FProductCode: string;
    FProductID: Integer;
    FProductName: string;
    FPy: string;
    FRelativeQty: Integer;
    FRemark: string;
    FRetailPrice: Currency;
    FShortCode: string;
    FShortName: string;
    FSortCode: string;
    FSortName: string;
    FSPEC: string;
    FSPProductID: string;
    FStatus: string;
    FUnit: string;
    FUpdateState: Integer;
  published
    property GID: Integer read FGID write FGID;
    property ProductCode: string read FProductCode write FProductCode;
    property ShortCode: string read FShortCode write FShortCode;
    property ProductName: string read FProductName write FProductName;
    property ShortName: string read FShortName write FShortName;
    property SortCode: string read FSortCode write FSortCode;
    property SortName: string read FSortName write FSortName;
    property BarCode: string read FBarCode write FBarCode;
    property SPEC: string read FSPEC write FSPEC;
    property Unit_: string read FUnit write FUnit;
    property RetailPrice: Currency read FRetailPrice write FRetailPrice;
    property Remark: string read FRemark write FRemark;
    property Status: string read FStatus write FStatus;
    property PrcType: Word read FPrcType write FPrcType;
    property ProductID: Integer read FProductID write FProductID;
    property MemberPrice: string read FMemberPrice write FMemberPrice;
    property IsBigPack: Boolean read FIsBigPack write FIsBigPack;
    property IsDisp: Word read FIsDisp write FIsDisp;
    property SPProductID: string read FSPProductID write FSPProductID;
    property RelativeQty: Integer read FRelativeQty write FRelativeQty;
    property Py: string read FPy write FPy;
    property UpdateState: Integer read FUpdateState write FUpdateState;
  end;

implementation

end.

