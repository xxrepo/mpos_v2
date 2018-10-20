{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_ParameterUtils

    This is not a complete unit, for testing

    20181005  NOTE 1: ADD 子参数重名处理（主要应对 DOM 相同类型子节点，在应用时不建设较多的使用同名参数，这可能遇到意想不到的状况）。

 **********************************************************************}

unit cm_ParameterUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, Contnrs, DB, syncobjs,
  cm_interfaces, cm_sysutils, cm_messager, cm_DOM,
  cm_parameter;

type

  { TCMParameterBaseData }

  TCMParameterBaseData = class(TCMBase, ICMParameterBaseData)
  protected
    FIsNull: Boolean;
  public
    constructor Create;
  public
    procedure Clear; virtual;
    function DataType: TParameterDataType; virtual;
    function IsNull: Boolean; virtual;
    function AsBoolean: Boolean; virtual;
    function AsDateTime: TDateTime; virtual;
    function AsCurrency: Currency; virtual;
    function AsFloat: Double; virtual;
    function AsInteger: Integer; virtual;
    function AsLargeInt: Int64; virtual;
    function AsString: string; virtual;
  end;

  { TCMParameterData }

  TCMParameterData = class(TCMParameterBaseData, ICMParameterData)
  public
    function AsObject: TObject; virtual;
    function AsInterface: IUnknown; virtual;
    function AsPointer: Pointer; virtual;
  end;

  { TCMObjectParameterData }

  TCMObjectParameterData = class(TCMParameterData)
  private
    FObj: TObject;
  public
    constructor Create(const AValue: TObject); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsString: string; override;
    function AsObject: TObject; override;
    function AsInterface: IUnknown; override;
    function AsPointer: Pointer; override;
  end;

  { TCMInterfaceParameterData }

  TCMInterfaceParameterData = class(TCMParameterData)
  private
    FIntf: IUnknown;
  public
    constructor Create(const AValue: IUnknown); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsString: string; override;
    function AsInterface: IUnknown; override;
    function AsPointer: Pointer; override;
  end;

  { TCMPointerParameterData }

  TCMPointerParameterData = class(TCMParameterData)
  private
    FPointer: Pointer;
  public
    constructor Create(const AValue: Pointer); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsPointer: Pointer; override;
  end;

  { TCMBooleanParameterData }

  TCMBooleanParameterData = class(TCMParameterData)
  private
    FBool: Boolean;
  public
    constructor Create(const AValue: Boolean); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsBoolean: Boolean; override;
    function AsFloat: Double; override;
    function AsInteger: Integer; override;
    function AsLargeInt: Int64; override;
    function AsString: string; override;
  end;

  { TCMDateTimeParameterData }

  TCMDateTimeParameterData = class(TCMParameterData)
  private
    FDateTime: TDateTime;
  public
    constructor Create(const AValue: TDateTime); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsDateTime: TDateTime; override;
    function AsFloat: Double; override;
    function AsString: string; override;
  end;

  { TCMNumberParameterData }

  TCMNumberParameterData = class(TCMParameterData)
  public
    function DataType: TParameterDataType; override;
    function AsBoolean: Boolean; override;
  end;

  { TCMCurrencyParameterData }

  TCMCurrencyParameterData = class(TCMNumberParameterData)
  private
    FCurrency: Currency;
  public
    constructor Create(const AValue: Currency); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsCurrency: Currency; override;
    function AsFloat: Double; override;
    function AsInteger: Integer; override;
    function AsLargeInt: Int64; override;
    function AsString: string; override;
  end;

  { TCMFloatParameterData }

  TCMFloatParameterData = class(TCMNumberParameterData)
  private
    FDouble: Double;
  public
    constructor Create(const AValue: Double); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsDateTime: TDateTime; override;
    function AsCurrency: Currency; override;
    function AsFloat: Double; override;
    function AsInteger: Integer; override;
    function AsLargeInt: Int64; override;
    function AsString: string; override;
  end;

  { TCMIntegerParameterData }

  TCMIntegerParameterData = class(TCMNumberParameterData)
  private
    FInt: Integer;
  public
    constructor Create(const AValue: Integer); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsCurrency: Currency; override;
    function AsFloat: Double; override;
    function AsInteger: Integer; override;
    function AsLargeInt: Int64; override;
    function AsString: string; override;
  end;

  { TCMLargeIntParameterData }

  TCMLargeIntParameterData = class(TCMNumberParameterData)
  private
    FLargeInt: Int64;
  public
    constructor Create(const AValue: Int64); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsCurrency: Currency; override;
    function AsFloat: Double; override;
    function AsInteger: Integer; override;
    function AsLargeInt: Int64; override;
    function AsString: string; override;
  end;

  { TCMStringParameterData }

  TCMStringParameterData = class(TCMParameterData)
  private
    FStr: string;
  public
    constructor Create(const AValue: string); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsBoolean: Boolean; override;
    function AsDateTime: TDateTime; override;
    function AsCurrency: Currency; override;
    function AsFloat: Double; override;
    function AsInteger: Integer; override;
    function AsLargeInt: Int64; override;
    function AsString: string; override;
  end;

  { TCMParameterCell }  //参数移除时可能还有引用者的存在,所以不能直接释放

  TCMParameterCell = class
  private
    FId: Integer;
    FParentId: Integer;
    FLevel: Integer;
    FName: string;
    FClue: string;
    FData: ICMParameterData;
    FChildren: TFPHashObjectList;
    FDataSync: TSynchroObject;
  public
    constructor Create(AId, AParentId, ALevel: Integer; const AName, AClue: string);
    destructor Destroy; override;
    property Id: Integer read FId;
    property ParentId: Integer read FParentId;
    property Level: Integer read FLevel;
    property Name: string read FName;
    property Clue: string read FClue;
    property Data: ICMParameterData read FData write FData;
    property Children: TFPHashObjectList read FChildren;
  protected
    procedure AddChildCell(ACell: TCMParameterCell);
    procedure DataLock;
    procedure DataUnlock;
  end;

  { IParameterSet }

  ICMParameterSet = interface(ICMParameterLoader)
    ['{41EBCDD3-175F-4905-B06A-E581973F6B4C}']
    function AddCell(ACell: TCMParameterCell): Boolean;
    function RemoveCell(ACell: TCMParameterCell): Boolean;
    function GetCell(const AClue: string): TCMParameterCell;
    function GetParameter(const AClue: string): ICMParameter;
    procedure Lock;
    procedure Unlock;
  end;

  { TCMParameterSet }

  TCMParameterSet = class(TCMBase, ICMParameterSet, ICMParameterLoader)
  private
    FClueList: TFPHashObjectList;
    FSyncObj: TSynchroObject;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function AddCell(ACell: TCMParameterCell): Boolean;
    function RemoveCell(ACell: TCMParameterCell): Boolean;
    function GetCell(const AClue: string): TCMParameterCell;
    function GetParameter(const AClue: string): ICMParameter;
    procedure Lock;
    procedure Unlock;
  public
    function LoadParameters(ABase: ICMParameter; ADataSet: TDataSet): Integer;  overload;
    function LoadParameters(ABase: ICMParameter; ANode: TCMDOMNode): Integer;  overload;
  end;

  { TCMParameter }

  TCMParameter = class(TCMBase, ICMParameter)
  private
    FId: Integer;
    FParentId: Integer;
    FLevel: Integer;
    FName: string;
    FClue: string;
    FParameterSet: ICMParameterSet;
    function GetSelfCell: TCMParameterCell;
    procedure RemoveChildCell(ACell: TCMParameterCell);
    procedure AddChildCell(ACell: TCMParameterCell);
  protected
    constructor Create(AParameterSet: ICMParameterSet; ACell: TCMParameterCell);
    function ReData(AData: ICMParameterData): ICMParameter; virtual;
  public
    constructor CreateNew(AParent: TCMParameter; const AName: string; AData: ICMParameterData); virtual;
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Boolean);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: TDateTime);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Currency);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Double);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Integer);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Int64);
    constructor Create(AParent: TCMParameter; const AName, AValue: string);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: TObject);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: IUnknown);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Pointer);
    destructor Destroy; override;
    property ParameterSet: ICMParameterSet read FParameterSet;
  public
    function Id: Integer;
    function ParentId: Integer;
    function Level: Integer;
    function Name: string;
    function Clue: string;
    function ItemCount: Integer;
    function ItemIndex(const AName: string): Integer;
    function GetItem(AIndex: Integer): ICMParameter;
    procedure RemoveItem(const AName: string);
    procedure RemoveItems;
    function Get(const AParameterName: string): ICMParameter;
  public //ICMParameterData
    procedure Clear;
    function DataType: TParameterDataType;
    function IsNull: Boolean;
    function AsBoolean: Boolean;
    function AsDateTime: TDateTime;
    function AsCurrency: Currency;
    function AsFloat: Double;
    function AsInteger: Integer;
    function AsLargeInt: Int64;
    function AsString: string;
    function AsObject: TObject;
    function AsInterface: IUnknown;
    function AsPointer: Pointer;
  public
    function AddBoolean(const AName: string; AValue: Boolean): ICMParameter;
    function AddDateTime(const AName: string; AValue: TDateTime): ICMParameter;
    function AddCurrency(const AName: string; AValue: Currency): ICMParameter;
    function AddFloat(const AName: string; AValue: Double): ICMParameter;
    function AddInteger(const AName: string; AValue: Integer): ICMParameter;
    function AddLargeInt(const AName: string; AValue: Int64): ICMParameter;
    function AddString(const AName, AValue: string): ICMParameter;
    function AddObject(const AName: string; AValue: TObject): ICMParameter;
    function AddInterface(const AName: string; AValue: IUnknown): ICMParameter;
    function AddPointer(const AName: string; AValue: Pointer): ICMParameter;
  public
    function ReBoolean(AValue: Boolean): ICMParameter;
    function ReDateTime(AValue: TDateTime): ICMParameter;
    function ReCurrency(AValue: Currency): ICMParameter;
    function ReFloat(AValue: Double): ICMParameter;
    function ReInteger(AValue: Integer): ICMParameter;
    function ReLargeInt(AValue: Int64): ICMParameter;
    function ReString(const AValue: string): ICMParameter;
    function ReObject(AValue: TObject): ICMParameter;
    function ReInterface(AValue: IUnknown): ICMParameter;
    function RePointer(AValue: Pointer): ICMParameter;
  end;

  TCMParameterDataCell = class
  private
    FData: ICMParameterData;
  public
    property Data: ICMParameterData read FData write FData;
  end;

  { TCMParameterDataList }

  TCMParameterDataList = class(TCMBase, ICMParameterDataList, ICMConstantParameterDataList)
  private
    FDataList: TFPHashObjectList;
    FSyncObj: TSynchroObject;
  public
    constructor Create;
    destructor Destroy; override;
  public //ICMConstantParameterDataList
    function Count: Integer;
    function Get(AIndex: Integer): ICMParameterData; overload;
    function Get(const AName: string): ICMParameterData; overload;
    function GetName(AIndex: Integer): string;
    function GetNames: TStrings;
  public //ICMParameterDataList
    procedure Remove(AIndex: Integer); overload;
    procedure Remove(const AName: string); overload;
    procedure Clear;
    function SetData(const AName: string; AData: ICMParameterData): ICMParameterData;
    function SetBoolean(const AName: string; AValue: Boolean): ICMParameterData;
    function SetDateTime(const AName: string; AValue: TDateTime): ICMParameterData;
    function SetCurrency(const AName: string; AValue: Currency): ICMParameterData;
    function SetFloat(const AName: string; AValue: Double): ICMParameterData;
    function SetInteger(const AName: string; AValue: Integer): ICMParameterData;
    function SetLargeInt(const AName: string; AValue: Int64): ICMParameterData;
    function SetString(const AName, AValue: string): ICMParameterData;
    function SetObject(const AName: string; AValue: TObject): ICMParameterData;
    function SetInterface(const AName: string; AValue: IUnknown): ICMParameterData;
    function SetPointer(const AName: string; AValue: Pointer): ICMParameterData;
  end;

ResourceString
  SInvalidName = '"%s" is not a valid parameter name';
  SInvalidID   = '"%d" is not a valid parameter ID';
  SInvalidVar = '"%s" is not assigned value';
  SInvalidBaseParameter = 'Base parameters of clue "%s" does not exist';


const
  AutoParameterIdStart: Integer = 80000000;

var
  ParameterFormatSettings: TFormatSettings;

implementation

{ TCMParameterBaseData }

constructor TCMParameterBaseData.Create;
begin
  Self.Clear;
end;

procedure TCMParameterBaseData.Clear;
begin
  FIsNull := True;
end;

function TCMParameterBaseData.DataType: TParameterDataType;
begin
  Result := pdtUnknown;
end;

function TCMParameterBaseData.IsNull: Boolean;
begin
  Result := FIsNull;
end;

function TCMParameterBaseData.AsBoolean: Boolean;
begin
  Result := False;
end;

function TCMParameterBaseData.AsDateTime: TDateTime;
begin
  Result := MinDateTime;
end;

function TCMParameterBaseData.AsCurrency: Currency;
begin
  Result := MinCurrency;
end;

function TCMParameterBaseData.AsFloat: Double;
begin
  Result := 0;
end;

function TCMParameterBaseData.AsInteger: Integer;
begin
  Result := 0;
end;

function TCMParameterBaseData.AsLargeInt: Int64;
begin
  Result := 0;
end;

function TCMParameterBaseData.AsString: string;
begin
  Result := '';
end;

{ TCMParameterData }

function TCMParameterData.AsObject: TObject;
begin
  Result := nil;
end;

function TCMParameterData.AsInterface: IUnknown;
begin
  Result := nil;
end;

function TCMParameterData.AsPointer: Pointer;
begin
  Result := nil;
end;

{ TCMObjectParameterData }

constructor TCMObjectParameterData.Create(const AValue: TObject);
begin
  FObj := AValue;
  FIsNull := FObj = nil;
end;

procedure TCMObjectParameterData.Clear;
begin
  FObj := nil;
  FIsNull := True;
end;

function TCMObjectParameterData.DataType: TParameterDataType;
begin
  Result := pdtObject;
end;

function TCMObjectParameterData.AsString: string;
begin
  if Assigned(FObj) then
    Result := FObj.ToString
  else
    Result := inherited AsString;
end;

function TCMObjectParameterData.AsObject: TObject;
begin
  Result := FObj;
end;

function TCMObjectParameterData.AsInterface: IUnknown;
begin
  Supports(FObj, IUnknown, Result);
end;

function TCMObjectParameterData.AsPointer: Pointer;
begin
  Result := FObj;
end;

{ TCMInterfaceParameterData }

constructor TCMInterfaceParameterData.Create(const AValue: IUnknown);
begin
  FIntf := AValue;
  FIsNull := FIntf = nil;
end;

procedure TCMInterfaceParameterData.Clear;
begin
  FIntf := nil;
  FIsNull := True;
end;

function TCMInterfaceParameterData.DataType: TParameterDataType;
begin
  Result := pdtInterface;
end;

function TCMInterfaceParameterData.AsString: string;
begin
  if Assigned(FIntf) then
    Result := GUIDToString(FIntf)
  else
    Result := inherited AsString;
end;

function TCMInterfaceParameterData.AsInterface: IUnknown;
begin
  Result := FIntf;
end;

function TCMInterfaceParameterData.AsPointer: Pointer;
begin
  Result := FIntf;
end;

{ TCMPointerParameterData }

constructor TCMPointerParameterData.Create(const AValue: Pointer);
begin
  FPointer := AValue;
  FIsNull := FPointer = nil;
end;

procedure TCMPointerParameterData.Clear;
begin
  FPointer := nil;
  FIsNull := True;
end;

function TCMPointerParameterData.DataType: TParameterDataType;
begin
  Result := pdtPointer;
end;

function TCMPointerParameterData.AsPointer: Pointer;
begin
  Result := FPointer;
end;

{ TCMBooleanParameterData }

constructor TCMBooleanParameterData.Create(const AValue: Boolean);
begin
  FBool := AValue;
  FIsNull := False;
end;

procedure TCMBooleanParameterData.Clear;
begin
  FBool := False;
  FIsNull := True;
end;

function TCMBooleanParameterData.DataType: TParameterDataType;
begin
  Result := pdtBoolean;
end;

function TCMBooleanParameterData.AsBoolean: Boolean;
begin
  Result := FBool;
end;

function TCMBooleanParameterData.AsFloat: Double;
begin
  Result := Ord(AsBoolean);
end;

function TCMBooleanParameterData.AsInteger: Integer;
begin
  Result := Ord(AsBoolean);
end;

function TCMBooleanParameterData.AsLargeInt: Int64;
begin
  Result := Ord(AsBoolean);
end;

function TCMBooleanParameterData.AsString: string;
begin
  Result := BoolToStr(AsBoolean, True);
end;

{ TCMDateTimeParameterData }

constructor TCMDateTimeParameterData.Create(const AValue: TDateTime);
begin
  FDateTime := AValue;
  FIsNull := False;
end;

procedure TCMDateTimeParameterData.Clear;
begin
  FDateTime := MinDateTime;
  FIsNull := True;
end;

function TCMDateTimeParameterData.DataType: TParameterDataType;
begin
  Result := pdtDateTime;
end;

function TCMDateTimeParameterData.AsDateTime: TDateTime;
begin
  Result := FDateTime;
end;

function TCMDateTimeParameterData.AsFloat: Double;
begin
  Result := AsDateTime;
end;

function TCMDateTimeParameterData.AsString: string;
begin
  Result := DateTimeToStr(AsDateTime, ParameterFormatSettings);
end;

{ TCMNumberParameterData }

function TCMNumberParameterData.DataType: TParameterDataType;
begin
  Result := pdtNumber;
end;

function TCMNumberParameterData.AsBoolean: Boolean;
begin
  Result := AsInteger <> 0;
end;

{ TCMCurrencyParameterData }

constructor TCMCurrencyParameterData.Create(const AValue: Currency);
begin
  FCurrency := AValue;
  FIsNull := False;
end;

procedure TCMCurrencyParameterData.Clear;
begin
  FCurrency := MinCurrency;
  FIsNull := True;
end;

function TCMCurrencyParameterData.DataType: TParameterDataType;
begin
  Result := pdtCurrency;
end;

function TCMCurrencyParameterData.AsCurrency: Currency;
begin
  Result := FCurrency;
end;

function TCMCurrencyParameterData.AsFloat: Double;
begin
  Result := AsCurrency;
end;

function TCMCurrencyParameterData.AsInteger: Integer;
begin
  Result := Round(AsCurrency);
end;

function TCMCurrencyParameterData.AsLargeInt: Int64;
begin
  Result := Round(AsCurrency);
end;

function TCMCurrencyParameterData.AsString: string;
begin
  Result := CurrToStr(AsCurrency, ParameterFormatSettings);
end;

{ TCMFloatParameterData }

constructor TCMFloatParameterData.Create(const AValue: Double);
begin
  FDouble := AValue;
  FIsNull := False;
end;

procedure TCMFloatParameterData.Clear;
begin
  FDouble := 0;
  FIsNull := True;
end;

function TCMFloatParameterData.DataType: TParameterDataType;
begin
  Result := pdtFloat;
end;

function TCMFloatParameterData.AsDateTime: TDateTime;
begin
  Result := AsFloat;
end;

function TCMFloatParameterData.AsCurrency: Currency;
begin
  Result := AsFloat;
end;

function TCMFloatParameterData.AsFloat: Double;
begin
  Result := FDouble;
end;

function TCMFloatParameterData.AsInteger: Integer;
begin
  Result := Round(AsFloat);
end;

function TCMFloatParameterData.AsLargeInt: Int64;
begin
  Result := Round(AsFloat);
end;

function TCMFloatParameterData.AsString: string;
begin
  Result := FloatToStr(AsFloat);
end;

{ TCMIntegerParameterData }

constructor TCMIntegerParameterData.Create(const AValue: Integer);
begin
  FInt := AValue;
  FIsNull := False;
end;

procedure TCMIntegerParameterData.Clear;
begin
  FInt := 0;
  FIsNull := True;
end;

function TCMIntegerParameterData.DataType: TParameterDataType;
begin
  Result := pdtInteger;
end;

function TCMIntegerParameterData.AsCurrency: Currency;
begin
  Result := AsInteger;
end;

function TCMIntegerParameterData.AsFloat: Double;
begin
  Result := AsInteger;
end;

function TCMIntegerParameterData.AsInteger: Integer;
begin
  Result := FInt;
end;

function TCMIntegerParameterData.AsLargeInt: Int64;
begin
  Result := AsInteger;
end;

function TCMIntegerParameterData.AsString: string;
begin
  Result := IntToStr(AsInteger);
end;

{ TCMLargeIntParameterData }

constructor TCMLargeIntParameterData.Create(const AValue: Int64);
begin
  FLargeInt := AValue;
  FIsNull := False;
end;

procedure TCMLargeIntParameterData.Clear;
begin
  FLargeInt := 0;
  FIsNull := True;
end;

function TCMLargeIntParameterData.DataType: TParameterDataType;
begin
  Result := pdtLargeInt;
end;

function TCMLargeIntParameterData.AsCurrency: Currency;
begin
  Result := inherited AsCurrency;
  if (AsLargeInt < MaxCurrency) and (AsLargeInt > MinCurrency) then
    Result := AsLargeInt;
end;

function TCMLargeIntParameterData.AsFloat: Double;
begin
  Result := AsLargeInt;
end;

function TCMLargeIntParameterData.AsInteger: Integer;
begin
  Result := AsLargeInt;
end;

function TCMLargeIntParameterData.AsLargeInt: Int64;
begin
  Result := FLargeInt;
end;

function TCMLargeIntParameterData.AsString: string;
begin
  Result := IntToStr(AsLargeInt);
end;

{ TCMStringParameterData }

constructor TCMStringParameterData.Create(const AValue: string);
begin
  FStr := AValue;
  FIsNull := False;
end;

procedure TCMStringParameterData.Clear;
begin
  FStr := '';
  FIsNull := True;
end;

function TCMStringParameterData.DataType: TParameterDataType;
begin
  Result := pdtString;
end;

function TCMStringParameterData.AsBoolean: Boolean;
begin
  Result := inherited AsBoolean;
  TryStrToBool(AsString, Result);
end;

function TCMStringParameterData.AsDateTime: TDateTime;
begin
  Result := inherited AsDateTime;
  TryStrToDateTime(AsString, Result, ParameterFormatSettings);
end;

function TCMStringParameterData.AsCurrency: Currency;
begin
  Result := inherited AsCurrency;
  TryStrToCurr(AsString, Result, ParameterFormatSettings);
end;

function TCMStringParameterData.AsFloat: Double;
begin
  Result := inherited AsFloat;
  TryStrToFloat(AsString, Result, ParameterFormatSettings);
end;

function TCMStringParameterData.AsInteger: Integer;
begin
  Result := inherited AsInteger;
  TryStrToInt(AsString, Result);
end;

function TCMStringParameterData.AsLargeInt: Int64;
begin
  Result := inherited AsLargeInt;
  TryStrToInt64(AsString, Result);
end;

function TCMStringParameterData.AsString: string;
begin
  Result := FStr;
end;

{ TCMParameterCell }

constructor TCMParameterCell.Create(AId, AParentId, ALevel: Integer; const AName, AClue: string);
begin
  FId := AId;
  FParentId := AParentId;
  FLevel := ALevel;
  FName := AName;
  FClue := AClue;
  FData := nil;
  FChildren := TFPHashObjectList.Create(True);
  FDataSync := TSynchroObject.Create;
end;

destructor TCMParameterCell.Destroy;
begin
  FChildren.Free;
  FData := nil;
  FDataSync.Free;
  inherited Destroy;
end;

procedure TCMParameterCell.AddChildCell(ACell: TCMParameterCell);
var
  i: Integer;
begin
  ACell.FLevel := Self.Level + 1;
  ACell.FClue := Format('%s.%s', [Clue, ACell.Name]);
  i := FChildren.FindIndexOf(ACell.Clue);
  while i >= 0 do //重名时
    begin
      ACell.FClue := Format('%s.%s$%d', [Clue, ACell.Name, i+2]);
      i := FChildren.FindIndexOf(ACell.Clue);
    end;
  FChildren.Add(ACell.Clue, ACell);
end;

procedure TCMParameterCell.DataLock;
begin
  FDataSync.Acquire;
end;

procedure TCMParameterCell.DataUnlock;
begin
  FDataSync.Release;
end;

{ TCMParameterSet }

constructor TCMParameterSet.Create;
begin
  FClueList := TFPHashObjectList.Create(False);
  FSyncObj := TSynchroObject.Create;
end;

destructor TCMParameterSet.Destroy;
begin
  FClueList.Free;
  FSyncObj.Free;
  inherited Destroy;
end;

function TCMParameterSet.AddCell(ACell: TCMParameterCell): Boolean;
begin
  Result := FClueList.Add(ACell.Clue, ACell) >= 0;
end;

function TCMParameterSet.RemoveCell(ACell: TCMParameterCell): Boolean;
begin
  Result := FClueList.Remove(ACell) >= 0;
end;

function TCMParameterSet.GetCell(const AClue: string): TCMParameterCell;
begin
  Result := TCMParameterCell(FClueList.Find(AClue));
end;

function TCMParameterSet.GetParameter(const AClue: string): ICMParameter;
begin
  Self.Lock;
  try
    Result := TCMParameter.Create(Self, Self.GetCell(AClue));
  finally
    Self.Unlock;
  end;
end;

procedure TCMParameterSet.Lock;
begin
  FSyncObj.Acquire;
end;

procedure TCMParameterSet.Unlock;
begin
  FSyncObj.Release;
end;

function TCMParameterSet.LoadParameters(ABase: ICMParameter; ADataSet: TDataSet): Integer;
var
  exist: Boolean;
  minParentId: Integer;
  id, parentId: Integer;
  name, value: string;
  cell, baseCell: TCMParameterCell;
  recList: TFPHashObjectList;
  rootList: TFPHashObjectList;
  j: Integer;
  procedure AdjustInsert(parentCell, subCell: TCMParameterCell);
  var i: Integer;
  begin
    if Assigned(parentCell) then
      parentCell.AddChildCell(subCell);
    Self.AddCell(subCell);
    Result := Result + 1;
    for i:=0 to recList.Count-1 do
      begin
        cell := TCMParameterCell(recList[i]);
        if cell.ParentId = subCell.Id then
          begin
            cell.FLevel := subCell.Level + 1;
            cell.FClue := Format('%s.%s', [subCell.Clue, cell.Name]);
            AdjustInsert(subCell, cell);
          end;
      end;
  end;
begin
  Result := 0;
  if Assigned(ADataSet) and (not ADataSet.IsEmpty) then
    begin
      Self.Lock;
      try
        exist := Assigned(ABase);
        baseCell := nil;
        if exist then
          begin
            baseCell := Self.GetCell(ABase.Clue);
            if not Assigned(baseCell) then
              begin
                raise EParameterError.CreateFmt(SInvalidBaseParameter, [ABase.Clue]);
                Exit;
              end;
          end;
        //找出最小 parentId
        minParentId := AutoParameterIdStart;
        ADataSet.First;
        while not ADataSet.EOF do
          begin
            parentId := ADataSet.FieldByName('parentId').AsInteger;
            if parentId < minParentId then
              minParentId := parentId;
            ADataSet.Next;
          end;
        //记录
        recList := TFPHashObjectList.Create(False);
        rootList := TFPHashObjectList.Create(False);
        ADataSet.First;
        while not ADataSet.EOF do
          begin
            id := ADataSet.FieldByName('id').AsInteger;
            parentId := ADataSet.FieldByName('parentId').AsInteger;
            name := ADataSet.FieldByName('name').AsString;
            value := ADataSet.FieldByName('value').AsString;
            //
            if parentId = minParentId then
              begin
                if exist then
                  cell := TCMParameterCell.Create(id, baseCell.Id, baseCell.Level + 1, name, Format('%s.%s', [baseCell.Clue, name]))
                else
                  cell := TCMParameterCell.Create(id, parentId, 1, name, name);
                rootList.Add(cell.Clue, cell);
              end
            else
              cell := TCMParameterCell.Create(id, parentId, 1, name, name);
            cell.Data := TCMStringParameterData.Create(value);
            //
            recList.Add(IntToStr(cell.Id), cell);
            ADataSet.Next;
          end;
        //加入
        for j:=0 to rootList.Count-1 do
          AdjustInsert(baseCell, TCMParameterCell(rootList[j]));
        rootList.Free;
        recList.Free;
      finally
        Self.Unlock;
      end;
    end;
end;

function TCMParameterSet.LoadParameters(ABase: ICMParameter; ANode: TCMDOMNode): Integer;
var
  name, value: string;
  rootCell: TCMParameterCell;
  rootParam: TCMParameter;
  procedure addChildren(pParam: TCMParameter; node: TCMDOMNode);
  var i: Integer; param: TCMParameter;
  begin
    for i:=0 to node.ChildCount-1 do
      begin
        if node.ChildNodes[i].AttributeExists('name') then
          param := TCMParameter.Create(pParam, node.ChildNodes[i].GetAttribute('name'), node.ChildNodes[i].Text)
        else
          param := TCMParameter.Create(pParam, node.ChildNodes[i].Name, node.ChildNodes[i].Text);
        Result := Result + 1;
        addChildren(param, node.ChildNodes[i]);
      end;
  end;
begin
  Result := 0;
  if Assigned(ANode) then
    begin
      if Assigned(ABase) and (FClueList.FindIndexOf(ABase.Clue) < 0) then
        begin
          raise EParameterError.CreateFmt(SInvalidBaseParameter, [ABase.Clue]);
          Exit;
        end;
      if ANode.AttributeExists('name') then
        name := ANode.GetAttribute('name')
      else
        name := ANode.Name;
      value := ANode.Text;
      //
      Self.Lock;
      try
        if Assigned(ABase) then
          rootCell:= TCMParameterCell.Create(-1, ABase.Id, ABase.Level + 1, name, Format('%s.%s', [ABase.Clue, name]))
        else
          rootCell:= TCMParameterCell.Create(-1, -1, 1, name, name);
        rootCell.FId := AutoParameterIdStart + Abs(rootCell.GetHashCode);
        rootCell.Data := TCMStringParameterData.Create(value);
        Self.AddCell(rootCell);
        rootParam := TCMParameter.Create(Self, rootCell);
        Result := Result + 1;
        addChildren(rootParam, ANode);
      finally
        Self.Unlock;
      end;
    end;
end;

{ TCMParameter }

constructor TCMParameter.Create(AParameterSet: ICMParameterSet; ACell: TCMParameterCell);
const
  varName1: string = 'AParameterSet';
  varName2: string = 'ACell.Data';
begin
  if not Assigned(AParameterSet) then
    raise EParameterError.CreateFmt(SInvalidVar, [varName1]);
  FParameterSet := AParameterSet;
  //
  if Assigned(ACell) then
    begin
      FId := ACell.Id;
      FParentId := ACell.ParentId;
      FLevel := ACell.Level;
      FName := ACell.Name;
      FClue := ACell.Clue;
      if not Assigned(ACell.Data) then
        raise EParameterError.CreateFmt(SInvalidVar, [varName2]);
    end
  else
    begin
      FId := -1;
      FParentId := -1;
      FLevel := 1;
      FName := '';
      FClue := '';
    end;
end;

constructor TCMParameter.CreateNew(AParent: TCMParameter; const AName: string; AData: ICMParameterData);
var
  exist: Boolean;
  cell: TCMParameterCell;
begin
  if (AName='') or not IsValidIdent(AName) then
    raise EParameterError.CreateFmt(SInvalidName, [AName]);
  FId := -1;
  FParentId := -1;
  FLevel := 1;
  FName := AName;
  FClue := AName;
  exist := Assigned(AParent);
  if exist then
    begin
      FParentId := AParent.Id;
      FLevel := AParent.Level + 1;
      FClue := Format('%s.%s', [AParent.Clue, AName]);
      FParameterSet := AParent.FParameterSet;
    end
  else
    FParameterSet := TCMParameterSet.Create;
  cell := TCMParameterCell.Create(FId, FParentId, FLevel, FName, FClue);
  FId := AutoParameterIdStart + Abs(cell.GetHashCode);
  cell.FId := FId;
  cell.Data := AData;
  //放入集合记录
  FParameterSet.Lock;
  try
    if exist then
      begin
        AParent.AddChildCell(cell);
        if FClue <> cell.Clue then //当重名时 clue 可能已变更
          FClue := cell.Clue;
      end;
    FParameterSet.AddCell(cell);
  finally
    FParameterSet.Unlock;
  end;
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Boolean);
begin
  Self.CreateNew(AParent, AName, TCMBooleanParameterData.Create(AValue));
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: TDateTime);
begin
  Self.CreateNew(AParent, AName, TCMDateTimeParameterData.Create(AValue));
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Currency);
begin
  Self.CreateNew(AParent, AName, TCMCurrencyParameterData.Create(AValue));
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Double);
begin
  Self.CreateNew(AParent, AName, TCMFloatParameterData.Create(AValue));
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Integer);
begin
  Self.CreateNew(AParent, AName, TCMIntegerParameterData.Create(AValue));
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Int64);
begin
  Self.CreateNew(AParent, AName, TCMLargeIntParameterData.Create(AValue));
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName, AValue: string);
begin
  Self.CreateNew(AParent, AName, TCMStringParameterData.Create(AValue));
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: TObject);
begin
  Self.CreateNew(AParent, AName, TCMObjectParameterData.Create(AValue));
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: IUnknown);
begin
  Self.CreateNew(AParent, AName, TCMInterfaceParameterData.Create(AValue));
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Pointer);
begin
  Self.CreateNew(AParent, AName, TCMPointerParameterData.Create(AValue));
end;

destructor TCMParameter.Destroy;
begin
  if ParentId = -1 then
    RemoveItems;
  FParameterSet := nil;
  inherited Destroy;
end;

//private
function TCMParameter.GetSelfCell: TCMParameterCell;
begin
  Result := FParameterSet.GetCell(FClue);
end;

//private
procedure TCMParameter.RemoveChildCell(ACell: TCMParameterCell);
var
  i: Integer;
  tempCell: TCMParameterCell;
begin
  if Assigned(ACell) then
    begin
      for i:=0 to ACell.Children.Count-1 do
        begin
          tempCell := TCMParameterCell(ACell.Children[i]);
          FParameterSet.RemoveCell(tempCell);
          RemoveChildCell(tempCell);
        end;
      ACell.Children.Clear;
    end;
end;

//private
procedure TCMParameter.AddChildCell(ACell: TCMParameterCell);
var
  selfCell: TCMParameterCell;
begin
  selfCell := GetSelfCell;
  if Assigned(selfCell) then
    selfCell.AddChildCell(ACell);
end;

function TCMParameter.Id: Integer;
begin
  Result := FId;
end;

function TCMParameter.ParentId: Integer;
begin
  Result := FParentId;
end;

function TCMParameter.Level: Integer;
begin
  Result := FLevel;
end;

function TCMParameter.Name: string;
begin
  Result := FName;
end;

function TCMParameter.Clue: string;
begin
  Result := FClue;
end;

function TCMParameter.ItemCount: Integer;
var
  cell: TCMParameterCell;
begin
  Result := 0;
  FParameterSet.Lock;
  try
    cell := GetSelfCell;
    if Assigned(cell) then
      Result := cell.Children.Count;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.ItemIndex(const AName: string): Integer;
var
  selfCell: TCMParameterCell;
begin
  Result := -1;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      Result := selfCell.Children.FindIndexOf(Format('%s.%s', [FClue, AName]));
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.GetItem(AIndex: Integer): ICMParameter;
var
  selfCell, subCell: TCMParameterCell;
begin
  Result := nil;
  subCell := nil;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        if AIndex < selfCell.Children.Count then
          subCell := TCMParameterCell(selfCell.Children[AIndex]);
      end;
    Result := TCMParameter.Create(FParameterSet, subCell);
  finally
    FParameterSet.Unlock;
  end;
end;

procedure TCMParameter.RemoveItem(const AName: string);
var
  selfCell, tempCell: TCMParameterCell;
begin
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        tempCell := TCMParameterCell(selfCell.Children.Find(Format('%s.%s', [FClue, AName])));
        RemoveChildCell(tempCell);
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

procedure TCMParameter.RemoveItems;
begin
  FParameterSet.Lock;
  try
    RemoveChildCell(GetSelfCell);
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.Get(const AParameterName: string): ICMParameter;
begin
  Result := FParameterSet.GetParameter(Format('%s.%s', [FClue, AParameterName]));
end;

procedure TCMParameter.Clear;
var
  selfCell: TCMParameterCell;
begin
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          selfCell.Data.Clear;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.DataType: TParameterDataType;
var
  selfCell: TCMParameterCell;
begin
  Result := pdtUnknown;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.DataType;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.IsNull: Boolean;
var
  selfCell: TCMParameterCell;
begin
  Result := True;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.IsNull;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AsBoolean: Boolean;
var
  selfCell: TCMParameterCell;
begin
  Result := False;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.AsBoolean;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AsDateTime: TDateTime;
var
  selfCell: TCMParameterCell;
begin
  Result := MinDateTime;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.AsDateTime;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AsCurrency: Currency;
var
  selfCell: TCMParameterCell;
begin
  Result := MinCurrency;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.AsCurrency;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AsFloat: Double;
var
  selfCell: TCMParameterCell;
begin
  Result := 0;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.AsFloat;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AsInteger: Integer;
var
  selfCell: TCMParameterCell;
begin
  Result := 0;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.AsInteger;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AsLargeInt: Int64;
var
  selfCell: TCMParameterCell;
begin
  Result := 0;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.AsLargeInt;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AsString: string;
var
  selfCell: TCMParameterCell;
begin
  Result := '';
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.AsString;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AsObject: TObject;
var
  selfCell: TCMParameterCell;
begin
  Result := nil;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.AsObject;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AsInterface: IUnknown;
var
  selfCell: TCMParameterCell;
begin
  Result := nil;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.AsInterface;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AsPointer: Pointer;
var
  selfCell: TCMParameterCell;
begin
  Result := nil;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          Result := selfCell.Data.AsPointer;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.AddBoolean(const AName: string; AValue: Boolean): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.AddDateTime(const AName: string; AValue: TDateTime): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.AddCurrency(const AName: string; AValue: Currency): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.AddFloat(const AName: string; AValue: Double): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.AddInteger(const AName: string; AValue: Integer): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.AddLargeInt(const AName: string; AValue: Int64): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.AddString(const AName, AValue: string): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.AddObject(const AName: string; AValue: TObject): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.AddInterface(const AName: string; AValue: IUnknown): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.AddPointer(const AName: string; AValue: Pointer): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

//protected
function TCMParameter.ReData(AData: ICMParameterData): ICMParameter;
var
  selfCell: TCMParameterCell;
begin
  Result := Self;
  FParameterSet.Lock;
  try
    selfCell := GetSelfCell;
    if Assigned(selfCell) then
      begin
        selfCell.DataLock;
        try
          selfCell.Data := nil;
          selfCell.Data := AData;
        finally
          selfCell.DataUnlock;
        end;
      end;
  finally
    FParameterSet.Unlock;
  end;
end;

function TCMParameter.ReBoolean(AValue: Boolean): ICMParameter;
begin
  Result := Self.ReData(TCMBooleanParameterData.Create(AValue));
end;

function TCMParameter.ReDateTime(AValue: TDateTime): ICMParameter;
begin
  Result := Self.ReData(TCMDateTimeParameterData.Create(AValue));
end;

function TCMParameter.ReCurrency(AValue: Currency): ICMParameter;
begin
  Result := Self.ReData(TCMCurrencyParameterData.Create(AValue));
end;

function TCMParameter.ReFloat(AValue: Double): ICMParameter;
begin
  Result := Self.ReData(TCMFloatParameterData.Create(AValue));
end;

function TCMParameter.ReInteger(AValue: Integer): ICMParameter;
begin
  Result := Self.ReData(TCMIntegerParameterData.Create(AValue));
end;

function TCMParameter.ReLargeInt(AValue: Int64): ICMParameter;
begin
  Result := Self.ReData(TCMLargeIntParameterData.Create(AValue));
end;

function TCMParameter.ReString(const AValue: string): ICMParameter;
begin
  Result := Self.ReData(TCMStringParameterData.Create(AValue));
end;

function TCMParameter.ReObject(AValue: TObject): ICMParameter;
begin
  Result := Self.ReData(TCMObjectParameterData.Create(AValue));
end;

function TCMParameter.ReInterface(AValue: IUnknown): ICMParameter;
begin
  Result := Self.ReData(TCMInterfaceParameterData.Create(AValue));
end;

function TCMParameter.RePointer(AValue: Pointer): ICMParameter;
begin
  Result := Self.ReData(TCMPointerParameterData.Create(AValue));
end;

{ TCMParameterDataList }

constructor TCMParameterDataList.Create;
begin
  FDataList := TFPHashObjectList.Create;
  FSyncObj := TSynchroObject.Create;
end;

destructor TCMParameterDataList.Destroy;
begin
  FDataList.Free;
  FSyncObj.Free;
  inherited Destroy;
end;

function TCMParameterDataList.Count: Integer;
begin
  Result := FDataList.Count;
end;

function TCMParameterDataList.Get(AIndex: Integer): ICMParameterData;
var
  cell: TCMParameterDataCell;
begin
  Result := nil;
  FSyncObj.Acquire;
  try
    if AIndex < FDataList.Count then
      cell := TCMParameterDataCell(FDataList[AIndex])
    else
      cell := nil;
    if Assigned(cell) then
      Result := cell.Data
    else
      Result := TCMParameterData.Create;
  finally
    FSyncObj.Release;
  end;
end;

function TCMParameterDataList.Get(const AName: string): ICMParameterData;
var
  cell: TCMParameterDataCell;
begin
  Result := nil;
  FSyncObj.Acquire;
  try
    cell := TCMParameterDataCell(FDataList.Find(AName));
    if Assigned(cell) then
      Result := cell.Data
    else
      Result := TCMParameterData.Create;
  finally
    FSyncObj.Release;
  end;
end;

function TCMParameterDataList.GetName(AIndex: Integer): string;
begin
  Result := '';
  FSyncObj.Acquire;
  try
    if AIndex < FDataList.Count then
      Result := FDataList.NameOfIndex(AIndex);
  finally
    FSyncObj.Release;
  end;
end;

function TCMParameterDataList.GetNames: TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;
  FSyncObj.Acquire;
  try
    for i:=0 to FDataList.Count-1 do
      Result.Add(FDataList.NameOfIndex(i));
  finally
    FSyncObj.Release;
  end;
end;

procedure TCMParameterDataList.Remove(AIndex: Integer);
begin
  FSyncObj.Acquire;
  try
    if AIndex < FDataList.Count then
      FDataList.Delete(AIndex);
  finally
    FSyncObj.Release;
  end;
end;

procedure TCMParameterDataList.Remove(const AName: string);
begin
  FSyncObj.Acquire;
  try
    FDataList.Delete(FDataList.FindIndexOf(AName));
  finally
    FSyncObj.Release;
  end;
end;

procedure TCMParameterDataList.Clear;
begin
  FSyncObj.Acquire;
  try
    FDataList.Clear;
  finally
    FSyncObj.Release;
  end;
end;

function TCMParameterDataList.SetData(const AName: string; AData: ICMParameterData): ICMParameterData;
var
  cell: TCMParameterDataCell;
begin
  Result := nil;
  if (AName='') or not IsValidIdent(AName) then
    raise EParameterError.CreateFmt(SInvalidName, [AName]);
  Result := AData;
  FSyncObj.Acquire;
  try
    cell := TCMParameterDataCell(FDataList.Find(AName));
    if not Assigned(cell) then
      begin
        cell := TCMParameterDataCell.Create;
        FDataList.Add(AName, cell);
      end;
    cell.Data := nil;
    cell.Data := AData;
  finally
    FSyncObj.Release;
  end;
end;

function TCMParameterDataList.SetBoolean(const AName: string; AValue: Boolean): ICMParameterData;
begin
  Result := SetData(AName, TCMBooleanParameterData.Create(AValue));
end;

function TCMParameterDataList.SetDateTime(const AName: string; AValue: TDateTime): ICMParameterData;
begin
  Result := SetData(AName, TCMDateTimeParameterData.Create(AValue));
end;

function TCMParameterDataList.SetCurrency(const AName: string; AValue: Currency): ICMParameterData;
begin
  Result := SetData(AName, TCMCurrencyParameterData.Create(AValue));
end;

function TCMParameterDataList.SetFloat(const AName: string; AValue: Double): ICMParameterData;
begin
  Result := SetData(AName, TCMFloatParameterData.Create(AValue));
end;

function TCMParameterDataList.SetInteger(const AName: string; AValue: Integer): ICMParameterData;
begin
  Result := SetData(AName, TCMIntegerParameterData.Create(AValue));
end;

function TCMParameterDataList.SetLargeInt(const AName: string; AValue: Int64): ICMParameterData;
begin
  Result := SetData(AName, TCMLargeIntParameterData.Create(AValue));
end;

function TCMParameterDataList.SetString(const AName, AValue: string): ICMParameterData;
begin
  Result := SetData(AName, TCMStringParameterData.Create(AValue));
end;

function TCMParameterDataList.SetObject(const AName: string; AValue: TObject): ICMParameterData;
begin
  Result := SetData(AName, TCMObjectParameterData.Create(AValue));
end;

function TCMParameterDataList.SetInterface(const AName: string; AValue: IUnknown): ICMParameterData;
begin
  Result := SetData(AName, TCMInterfaceParameterData.Create(AValue));
end;

function TCMParameterDataList.SetPointer(const AName: string; AValue: Pointer): ICMParameterData;
begin
  Result := SetData(AName, TCMPointerParameterData.Create(AValue));
end;


initialization
  ParameterFormatSettings := CMFormatSettings;


end.

