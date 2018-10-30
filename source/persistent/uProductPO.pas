unit uProductPO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  TSimpleTbShopProduct = class(TPersistent)
  private
    FBarCode: string;
    FGID: integer;
    FIsBigPack: boolean;
    FIsDisp: word;
    FMemberPrice: string;
    FPrcType: word;
    FProductCode: string;
    FProductID: integer;
    FProductName: string;
    FPy: string;
    FRelativeQty: integer;
    FRemark: string;
    FRetailPrice: currency;
    //FShortCode: string;
    FShortName: string;
    FSortCode: string;
    //FSortName: string;
    FSPEC: string;
    FSPProductID: string;
    FStatus: string;
    FUnit: string;
    FUpdateState: integer;
  published
    property GID: integer read FGID write FGID;
    property ProductCode: string read FProductCode write FProductCode;
    property ProductName: string read FProductName write FProductName;
    property ShortName: string read FShortName write FShortName;
    property SortCode: string read FSortCode write FSortCode;
    property BarCode: string read FBarCode write FBarCode;
    property SPEC: string read FSPEC write FSPEC;
    property Unit_: string read FUnit write FUnit;
    property RetailPrice: currency read FRetailPrice write FRetailPrice;
    property Remark: string read FRemark write FRemark;
    property Status: string read FStatus write FStatus;
    property PrcType: word read FPrcType write FPrcType;
    property ProductID: integer read FProductID write FProductID;
    property MemberPrice: string read FMemberPrice write FMemberPrice;
    property IsBigPack: boolean read FIsBigPack write FIsBigPack;
    property IsDisp: word read FIsDisp write FIsDisp;
    property SPProductID: string read FSPProductID write FSPProductID;
    property RelativeQty: integer read FRelativeQty write FRelativeQty;
    property Py: string read FPy write FPy;
    property UpdateState: integer read FUpdateState write FUpdateState;
  end;

  { TProduct }

  TProduct = class(TPersistent)
  private
    FBarCode: string;
    FGID: integer;
    FIsBigPack: boolean;
    FIsDisp: word;
    FIsLtd: word;
    FMemberPrice: currency;
    FPrcType: word;
    FProductCode: string;
    FProductID: integer;
    FProductName: string;
    FPy: string;
    FRelativeQty: integer;
    FRemark: string;
    FRetailPrice: currency;
    FShortCode: string;
    FShortName: string;
    FSortCode: string;
    FSortName: string;
    FSPEC: string;
    FSPProductID: string;
    FStatus: string;
    FUnit: string;
    FUpdateState: integer;
  published
    property GID: integer read FGID write FGID;
    property ProductCode: string read FProductCode write FProductCode;
    property ShortCode: string read FShortCode write FShortCode;
    property ProductName: string read FProductName write FProductName;
    property ShortName: string read FShortName write FShortName;
    property SortCode: string read FSortCode write FSortCode;
    property SortName: string read FSortName write FSortName;
    property BarCode: string read FBarCode write FBarCode;
    property SPEC: string read FSPEC write FSPEC;
    property Unit_: string read FUnit write FUnit;
    property RetailPrice: currency read FRetailPrice write FRetailPrice;
    property Remark: string read FRemark write FRemark;
    property Status: string read FStatus write FStatus;
    property PrcType: word read FPrcType write FPrcType;
    property ProductID: integer read FProductID write FProductID;
    property MemberPrice: currency read FMemberPrice write FMemberPrice;
    property IsBigPack: boolean read FIsBigPack write FIsBigPack;
    property IsDisp: word read FIsDisp write FIsDisp;
    property IsLtd: word read FIsLtd write FIsLtd;
    property SPProductID: string read FSPProductID write FSPProductID;
    property RelativeQty: integer read FRelativeQty write FRelativeQty;
    property Py: string read FPy write FPy;
    property UpdateState: integer read FUpdateState write FUpdateState;
  end;

  TProductList = class(TObjectList<TProduct>);

implementation

end.

