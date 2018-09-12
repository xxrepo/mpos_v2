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
    procedure SetValue(const AValue: Variant); virtual;
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
    function AsPointer: Pointer; virtual;
    function AsObject: TObject; virtual;
  end;

  { TCMPointerParameterData }

  TCMPointerParameterData = class(TCMParameterData)
  private
    FPointer: Pointer;
  protected
    procedure SetValue(const AValue: Pointer); overload;
  public
    constructor Create(const AValue: Pointer); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsPointer: Pointer; override;
  end;

  { TCMObjectParameterData }

  TCMObjectParameterData = class(TCMParameterData)
  private
    FObj: TObject;
  protected
    procedure SetValue(const AValue: TObject); overload;
  public
    constructor Create(const AValue: TObject); overload;
    procedure Clear; override;
    function DataType: TParameterDataType; override;
    function AsString: string; override;
    function AsPointer: Pointer; override;
    function AsObject: TObject; override;
  end;

  { TCMBooleanParameterData }

  TCMBooleanParameterData = class(TCMParameterData)
  private
    FBool: Boolean;
  protected
    procedure SetValue(const AValue: Variant); override;
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
  protected
    procedure SetValue(const AValue: Variant); override;
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
  protected
    procedure SetValue(const AValue: Variant); override;
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
  protected
    procedure SetValue(const AValue: Variant); override;
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
  protected
    procedure SetValue(const AValue: Variant); override;
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
  protected
    procedure SetValue(const AValue: Variant); override;
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
  protected
    procedure SetValue(const AValue: Variant); override;
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

  { TCMParameterCell }

  TCMParameterCell = class
  private
    FId: Integer;
    FParentId: Integer;
    FLevel: Integer;
    FName: string;
    FData: ICMParameterData;
    FClue: string;
    FChildClues: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: Integer read FId write FId;
    property ParentId: Integer read FParentId write FParentId;
    property Level: Integer read FLevel write FLevel;
    property Name: string read FName write FName;
    property Data: ICMParameterData read FData write FData;
    property Clue: string read FClue write FClue;
    property ChildClues: TStrings read FChildClues;
  end;

  { ICMParameterSet }

  ICMParameterSet = interface(ICMBase)
    ['{8F0D6695-0637-4AA9-A9A2-B9F43B612598}']
    function CreateVariantData(const AValue: Variant): ICMParameterData;
    function CreatePointerData(const AValue: Pointer): ICMParameterData;
    function CreateObjectData(const AValue: TObject): ICMParameterData;
    //
    function AddParameter(AId, AParentId: Integer; const AName: string; const AValue: Variant): TCMParameterCell;
    function AddParameter(AId, AParentId: Integer; const AName: string; const AValue: Pointer): TCMParameterCell;
    function AddParameter(AId, AParentId: Integer; const AName: string; const AValue: TObject): TCMParameterCell;
    function AddParameter(ABaseCell: TCMParameterCell; const AName: string; const AValue: Variant): TCMParameterCell;
    function AddParameter(ABaseCell: TCMParameterCell; const AName: string; const AValue: Pointer): TCMParameterCell;
    function AddParameter(ABaseCell: TCMParameterCell; const AName: string; const AValue: TObject): TCMParameterCell;
    function GetParameter(AId: Integer): TCMParameterCell;
    function GetParameter(const AParameterName: string): TCMParameterCell;
  end;

  { TCMParameter }

  TCMParameter = class(TCMBase, ICMParameter)
  private
    FParameterSet: ICMParameterSet;
    FCell: TCMParameterCell;
    FSynchroObject: TSynchroObject;
    function AddVariant(const AName: string; AValue: Variant): ICMParameter;
    function ReVariant(AValue: Variant): ICMParameter;
  public
    constructor Create(AParameterSet: ICMParameterSet; const ACell: TCMParameterCell);
    destructor Destroy; override;
  public
    function Id: Integer;
    function ParentId: Integer;
    function Level: Integer;
    function Name: string;
    function Clue: string;
    function ItemCount: Integer;
    function GetItem(AIndex: Integer): ICMParameter;
    function Get(const AParameterName: string): ICMParameter; virtual;
    //
    function AddBoolean(const AName: string; AValue: Boolean): ICMParameter;
    function AddDateTime(const AName: string; AValue: TDateTime): ICMParameter;
    function AddCurrency(const AName: string; AValue: Currency): ICMParameter;
    function AddFloat(const AName: string; AValue: Double): ICMParameter;
    function AddInteger(const AName: string; AValue: Integer): ICMParameter;
    function AddLargeInt(const AName: string; AValue: Int64): ICMParameter;
    function AddString(const AName, AValue: string): ICMParameter;
    function AddPointer(const AName: string; AValue: Pointer): ICMParameter;
    function AddObject(const AName: string; AValue: TObject): ICMParameter;
    //
    procedure Clear;
    function DataType: TParameterDataType; virtual;
    function IsNull: Boolean; virtual;
    function AsBoolean: Boolean; virtual;
    function AsCurrency: Currency; virtual;
    function AsDateTime: TDateTime; virtual;
    function AsFloat: Double; virtual;
    function AsInteger: Integer; virtual;
    function AsLargeInt: Int64; virtual;
    function AsString: string; virtual;
    function AsPointer: Pointer; virtual;
    function AsObject: TObject; virtual;
    //
    function ReBoolean(AValue: Boolean): ICMParameter;
    function ReDateTime(AValue: TDateTime): ICMParameter;
    function ReCurrency(AValue: Currency): ICMParameter;
    function ReFloat(AValue: Double): ICMParameter;
    function ReInteger(AValue: Integer): ICMParameter;
    function ReLargeInt(AValue: Int64): ICMParameter;
    function ReString(const AValue: string): ICMParameter;
    function RePointer(AValue: Pointer): ICMParameter;
    function ReObject(AValue: TObject): ICMParameter;
  end;

  { TCMParameterSet }

  TCMParameterSet = class(TCMMessageable, ICMParameterSet, ICMParameterWidget)
  private
    FRecordList: TFPHashObjectList;
    FClueList: TFPHashObjectList;
    //ACell 应准备 id、Name、Data
    function Add(ABaseCell: TCMParameterCell; ACell: TCMParameterCell): Boolean;
    //准备 id、Name、Data
    function Add(AId, AParentId: Integer; const AName: string; AData: ICMParameterData): TCMParameterCell;
    function Add(ABaseCell: TCMParameterCell; const AName: string; AData: ICMParameterData): TCMParameterCell;
  public
    constructor Create;
    destructor Destroy; override;
  public  //ICMParameterSet
    function CreateVariantData(const AValue: Variant): ICMParameterData;
    function CreatePointerData(const AValue: Pointer): ICMParameterData;
    function CreateObjectData(const AValue: TObject): ICMParameterData;
    //
    function AddParameter(AId, AParentId: Integer; const AName: string; const AValue: Variant): TCMParameterCell;
    function AddParameter(AId, AParentId: Integer; const AName: string; const AValue: Pointer): TCMParameterCell;
    function AddParameter(AId, AParentId: Integer; const AName: string; const AValue: TObject): TCMParameterCell;
    //
    function AddParameter(ABaseCell: TCMParameterCell; const AName: string; const AValue: Variant): TCMParameterCell;
    function AddParameter(ABaseCell: TCMParameterCell; const AName: string; const AValue: Pointer): TCMParameterCell;
    function AddParameter(ABaseCell: TCMParameterCell; const AName: string; const AValue: TObject): TCMParameterCell;
    //
    function GetParameter(AId: Integer): TCMParameterCell;
    function GetParameter(const AParameterName: string): TCMParameterCell;
  public  //ICMParameterWidget
    function AddParameters(ADatum: ICMParameter; ADataSet: TDataSet): Integer;
    function AddParameters(ADatum: ICMParameter; ANode: TCMDOMNode): Integer;
  end;

ResourceString
  SInvalidName = '"%s" is not a valid parameter name';

var
  ParameterFormatSettings: TFormatSettings;

implementation

{ TCMParameterBaseData }

constructor TCMParameterBaseData.Create;
begin
  Self.Clear;
end;

procedure TCMParameterBaseData.SetValue(const AValue: Variant);
begin
  FIsNull := False;
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

function TCMParameterData.AsPointer: Pointer;
begin
  Result := nil;
end;

function TCMParameterData.AsObject: TObject;
begin
  Result := nil;
end;

{ TCMPointerParameterData }

procedure TCMPointerParameterData.SetValue(const AValue: Pointer);
begin
  FPointer := AValue;
  FIsNull := FPointer = nil;
end;

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

{ TCMObjectParameterData }

procedure TCMObjectParameterData.SetValue(const AValue: TObject);
begin
  FObj := AValue;
  FIsNull := FObj = nil;
end;

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

function TCMObjectParameterData.AsPointer: Pointer;
begin
  Result := FObj;
end;

function TCMObjectParameterData.AsObject: TObject;
begin
  Result := FObj;
end;

{ TCMBooleanParameterData }

procedure TCMBooleanParameterData.SetValue(const AValue: Variant);
begin
  FBool := AValue;
  FIsNull := False;
end;

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

procedure TCMDateTimeParameterData.SetValue(const AValue: Variant);
begin
  FDateTime := AValue;
  FIsNull := False;
end;

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

procedure TCMCurrencyParameterData.SetValue(const AValue: Variant);
begin
  FCurrency := AValue;
  FIsNull := False;
end;

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

procedure TCMFloatParameterData.SetValue(const AValue: Variant);
begin
  FDouble := AValue;
  FIsNull := False;
end;

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

procedure TCMIntegerParameterData.SetValue(const AValue: Variant);
begin
  FInt := AValue;
  FIsNull := False;
end;

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

procedure TCMLargeIntParameterData.SetValue(const AValue: Variant);
begin
  FLargeInt := AValue;
  FIsNull := False;
end;

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

procedure TCMStringParameterData.SetValue(const AValue: Variant);
begin
  FStr := AValue;
  FIsNull := False;
end;

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

constructor TCMParameterCell.Create;
begin
  FChildClues := TStringList.Create;
end;

destructor TCMParameterCell.Destroy;
begin
  FChildClues.Free;
  inherited Destroy;
end;

{TCMParameter}

constructor TCMParameter.Create(AParameterSet: ICMParameterSet; const ACell: TCMParameterCell);
begin
  FParameterSet := AParameterSet;
  FCell := ACell;
  FSynchroObject := TCriticalSection.Create;
end;

destructor TCMParameter.Destroy;
begin
  FParameterSet := nil;
  FCell := nil;
  FSynchroObject.Free;
  inherited Destroy;
end;

function TCMParameter.Id: Integer;
begin
  Result := -1;
  if Assigned(FCell) then
    Result := FCell.Id;
end;

function TCMParameter.ParentId: Integer;
begin
  Result := -1;
  if Assigned(FCell) then
    Result := FCell.ParentId;
end;

function TCMParameter.Level: Integer;
begin
  Result := -1;
  if Assigned(FCell) then
    Result := FCell.Level;
end;

function TCMParameter.Name: string;
begin
  Result := '';
  if Assigned(FCell) then
    Result := FCell.Name;
end;

function TCMParameter.Clue: string;
begin
  Result := '';
end;

function TCMParameter.ItemCount: Integer;
begin
  Result := 0;
  if Assigned(FCell) then
    Result := FCell.ChildClues.Count;
end;

function TCMParameter.GetItem(AIndex: Integer): ICMParameter;
var
  cell: TCMParameterCell;
begin
  if Assigned(FCell) then
    begin
      if (AIndex >= 0) and (AIndex < FCell.ChildClues.Count) then
        begin
          cell := FParameterSet.GetParameter(FCell.ChildClues[AIndex]);
          if Assigned(cell) then
            begin
              Result := TCMParameter.Create(FParameterSet, cell);
              Exit;
            end;
        end;
    end;
  Result := TCMParameter.Create(nil, nil);
end;

function TCMParameter.Get(const AParameterName: string): ICMParameter;
var
  cell: TCMParameterCell;
begin
  Result := nil;
  if Assigned(FParameterSet) then
    begin
      if Assigned(FCell) then
        cell := FParameterSet.GetParameter(FCell.Clue + '.' + AParameterName)
      else
        cell := FParameterSet.GetParameter(AParameterName);
      if Assigned(cell) then
        begin
          Result := TCMParameter.Create(FParameterSet, cell);
          Exit;
        end;
    end;
  Result := TCMParameter.Create(nil, nil);
end;

//private
function TCMParameter.AddVariant(const AName: string; AValue: Variant): ICMParameter;
begin
  if Assigned(FParameterSet) then
    Result := TCMParameter.Create(FParameterSet, FParameterSet.AddParameter(FCell, AName, AValue))
  else
    Result := TCMParameter.Create(nil, nil);
end;

function TCMParameter.AddBoolean(const AName: string; AValue: Boolean): ICMParameter;
begin
  Result := AddVariant(AName, AValue);
end;

function TCMParameter.AddDateTime(const AName: string; AValue: TDateTime): ICMParameter;
begin
  Result := AddVariant(AName, AValue);
end;

function TCMParameter.AddCurrency(const AName: string; AValue: Currency): ICMParameter;
begin
  Result := AddVariant(AName, AValue);
end;

function TCMParameter.AddFloat(const AName: string; AValue: Double): ICMParameter;
begin
  Result := AddVariant(AName, AValue);
end;

function TCMParameter.AddInteger(const AName: string; AValue: Integer): ICMParameter;
begin
  Result := AddVariant(AName, AValue);
end;

function TCMParameter.AddLargeInt(const AName: string; AValue: Int64): ICMParameter;
begin
  Result := AddVariant(AName, AValue);
end;

function TCMParameter.AddString(const AName, AValue: string): ICMParameter;
begin
  Result := AddVariant(AName, AValue);
end;

function TCMParameter.AddPointer(const AName: string; AValue: Pointer): ICMParameter;
begin
  if Assigned(FParameterSet) then
    Result := TCMParameter.Create(FParameterSet, FParameterSet.AddParameter(FCell, AName, AValue))
  else
    Result := TCMParameter.Create(nil, nil);
end;

function TCMParameter.AddObject(const AName: string; AValue: TObject): ICMParameter;
begin
  if Assigned(FParameterSet) then
    Result := TCMParameter.Create(FParameterSet, FParameterSet.AddParameter(FCell, AName, AValue))
  else
    Result := TCMParameter.Create(nil, nil);
end;

procedure TCMParameter.Clear;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        FCell.Data.Clear;
      finally
        FSynchroObject.Release;
      end;
    end;
end;

function TCMParameter.DataType: TParameterDataType;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.DataType;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := pdtUnknown;
end;

function TCMParameter.IsNull: Boolean;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.IsNull;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := True;
end;

function TCMParameter.AsBoolean: Boolean;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.AsBoolean;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := False;
end;

function TCMParameter.AsCurrency: Currency;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.AsCurrency;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := MinCurrency;
end;

function TCMParameter.AsDateTime: TDateTime;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.AsDateTime;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := MinDateTime;
end;

function TCMParameter.AsFloat: Double;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.AsFloat;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := 0;
end;

function TCMParameter.AsInteger: Integer;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.AsInteger;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := 0;
end;

function TCMParameter.AsLargeInt: Int64;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.AsLargeInt;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := 0;
end;

function TCMParameter.AsString: string;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.AsString;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := '';
end;

function TCMParameter.AsPointer: Pointer;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.AsPointer;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := nil;
end;

function TCMParameter.AsObject: TObject;
begin
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        Result := FCell.Data.AsObject;
      finally
        FSynchroObject.Release;
      end;
      Exit;
    end;
  Result := nil;
end;

//private
function TCMParameter.ReVariant(AValue: Variant): ICMParameter;
begin
  Result := Self;
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        FCell.Data := FParameterSet.CreateVariantData(AValue);
      finally
        FSynchroObject.Release;
      end;
    end;
end;

function TCMParameter.ReBoolean(AValue: Boolean): ICMParameter;
begin
  Result := ReVariant(AValue);
end;

function TCMParameter.ReDateTime(AValue: TDateTime): ICMParameter;
begin
  Result := ReVariant(AValue);
end;

function TCMParameter.ReCurrency(AValue: Currency): ICMParameter;
begin
  Result := ReVariant(AValue);
end;

function TCMParameter.ReFloat(AValue: Double): ICMParameter;
begin
  Result := ReVariant(AValue);
end;

function TCMParameter.ReInteger(AValue: Integer): ICMParameter;
begin
  Result := ReVariant(AValue);
end;

function TCMParameter.ReLargeInt(AValue: Int64): ICMParameter;
begin
  Result := ReVariant(AValue);
end;

function TCMParameter.ReString(const AValue: string): ICMParameter;
begin
  Result := ReVariant(AValue);
end;

function TCMParameter.RePointer(AValue: Pointer): ICMParameter;
begin
  Result := Self;
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        FCell.Data := FParameterSet.CreatePointerData(AValue);
      finally
        FSynchroObject.Release;
      end;
    end;
end;

function TCMParameter.ReObject(AValue: TObject): ICMParameter;
begin
  Result := Self;
  if Assigned(FCell) then
    begin
      FSynchroObject.Acquire;
      try
        FCell.Data := FParameterSet.CreateObjectData(AValue);
      finally
        FSynchroObject.Release;
      end;
    end;
end;

{ TCMParameterSet }

function TCMParameterSet.CreateVariantData(const AValue: Variant): ICMParameterData;
var
  unsupported: Boolean;
  re: TCMParameterData;
begin
  Result := nil;
  //TODO 36:varrecord
  unsupported := False;
  case VarType(AValue) of
  11: re := TCMBooleanParameterData.Create;
  7:  re := TCMDateTimeParameterData.Create;
  6:  re := TCMCurrencyParameterData.Create;
  5,14:  re := TCMFloatParameterData.Create;
  2,3,4,16,17,18,19,21:  re := TCMIntegerParameterData.Create;
  20: re := TCMLargeIntParameterData.Create;
  0,1,8,9,10,12,13,15,$48,$49,$101,$102,$fff,$2000,$4000: begin unsupported := True; re := TCMStringParameterData.Create; end;
  else re := TCMStringParameterData.Create;
  end;
  if unsupported then
    re.SetValue('')
  else
    re.SetValue(AValue);
  Result := re;
end;

function TCMParameterSet.CreatePointerData(const AValue: Pointer): ICMParameterData;
var
  re: TCMPointerParameterData;
begin
  Result := nil;
  re := TCMPointerParameterData.Create;
  re.SetValue(AValue);
  Result := re;
end;

function TCMParameterSet.CreateObjectData(const AValue: TObject): ICMParameterData;
var
  re: TCMObjectParameterData;
begin
  Result := nil;
  re := TCMObjectParameterData.Create;
  re.SetValue(AValue);
  Result := re;
end;

constructor TCMParameterSet.Create;
begin
  inherited Create;
  FRecordList := TFPHashObjectList.Create(True);
  FClueList := TFPHashObjectList.Create(False);
end;

destructor TCMParameterSet.Destroy;
begin
  FClueList.Free;
  FRecordList.Free;
  inherited Destroy;
end;

//ACell 应准备 id、Name、Data
function TCMParameterSet.Add(ABaseCell: TCMParameterCell; ACell: TCMParameterCell): Boolean;
begin
  Result := False;
  if Assigned(ABaseCell) then
    begin
      ACell.ParentId := ABaseCell.Id;
      ACell.Level := ABaseCell.Level + 1;
      ACell.Clue := ABaseCell.Clue + '.' + ACell.Name;
      ABaseCell.ChildClues.Add(ACell.Clue);
    end
  else
    begin
      ACell.ParentId := -1;
      ACell.Level := 1;
      ACell.Clue := ACell.Name;
    end;
  //
  if FRecordList.Add(IntToStr(ACell.Id), ACell) >= 0 then
    begin
      if FClueList.Add(ACell.Clue, ACell) >= 0 then
        Result := True;
    end
  else
    ACell.Free;
end;

function TCMParameterSet.Add(AId, AParentId: Integer; const AName: string; AData: ICMParameterData): TCMParameterCell;
var
  aCell, pCell: TCMParameterCell;
begin
  Result := nil;
  if (AName='') or not IsValidIdent(AName) then
    raise EParameterError.CreateFmt(SInvalidName, [AName]);
  if AId <= 0 then
    begin
      Messager.Error('增加参数失败. [01]', 'SelAdd() AId:%d 不能小于等于0.', [AId]);
      Exit;
    end;
  pCell := nil;
  if AParentId > 0 then
    begin
      pCell := TCMParameterCell(FRecordList.Find(IntToStr(AParentId)));
      if not Assigned(pCell) then
        begin
          Messager.Error('增加参数失败. [02]', 'SelAdd() ParentId:%d 不存在.', [AParentId]);
          Exit;
        end;
    end;
  aCell := TCMParameterCell.Create;
  aCell.Id := AId;
  aCell.Name := AName;
  aCell.Data := AData;
  //
  if Self.Add(pCell, ACell) then
    Result := aCell;
end;

function TCMParameterSet.AddParameter(AId, AParentId: Integer; const AName: string; const AValue: Variant): TCMParameterCell;
begin
  Result := Self.Add(AId, AParentId, AName, CreateVariantData(AValue));
end;

function TCMParameterSet.AddParameter(AId, AParentId: Integer; const AName: string; const AValue: Pointer): TCMParameterCell;
begin
  Result := Self.Add(AId, AParentId, AName, CreatePointerData(AValue));
end;

function TCMParameterSet.AddParameter(AId, AParentId: Integer; const AName: string; const AValue: TObject): TCMParameterCell;
begin
  Result := Self.Add(AId, AParentId, AName, CreateObjectData(AValue));
end;

function TCMParameterSet.Add(ABaseCell: TCMParameterCell; const AName: string; AData: ICMParameterData): TCMParameterCell;
var
  aCell: TCMParameterCell;
begin
  Result := nil;
  if (AName='') or not IsValidIdent(AName) then
    raise EParameterError.CreateFmt(SInvalidName, [AName]);
  aCell := TCMParameterCell.Create;
  aCell.Id := 80000000 + Abs(aCell.GetHashCode);
  aCell.Name := AName;
  aCell.Data := AData;
  //
  if Self.Add(ABaseCell, aCell) then
    Result := aCell;
end;

function TCMParameterSet.AddParameter(ABaseCell: TCMParameterCell; const AName: string; const AValue: Variant): TCMParameterCell;
begin
  Result := Self.Add(ABaseCell, AName, CreateVariantData(AValue));
end;

function TCMParameterSet.AddParameter(ABaseCell: TCMParameterCell; const AName: string; const AValue: Pointer): TCMParameterCell;
begin
  Result := Self.Add(ABaseCell, AName, CreatePointerData(AValue));
end;

function TCMParameterSet.AddParameter(ABaseCell: TCMParameterCell; const AName: string; const AValue: TObject): TCMParameterCell;
begin
  Result := Self.Add(ABaseCell, AName, CreateObjectData(AValue));
end;

function TCMParameterSet.GetParameter(AId: Integer): TCMParameterCell;
begin
  Result := TCMParameterCell(FRecordList.Find(IntToStr(AId)));
end;

function TCMParameterSet.GetParameter(const AParameterName: string): TCMParameterCell;
var
  nameList: TStringList;
  i: Integer;
begin
  Result := nil;
  nameList := TStringList.Create;
  try
    nameList.Delimiter := '.';
    nameList.DelimitedText := Trim(AParameterName);
    //检查名字有效性
    for i:=0 to nameList.Count-1 do
      begin
        if (nameList[i]='') or not IsValidIdent(nameList[i]) then
          raise EParameterError.CreateFmt(SInvalidName, [nameList[i]]);
      end;
    //
    Result := TCMParameterCell(FClueList.Find(AParameterName));
  finally
    nameList.Free;
  end;
end;

//约定参数 id 为增序, id 不大于 80'000'000. dataset 以 id 正排序
function TCMParameterSet.AddParameters(ADatum: ICMParameter; ADataSet: TDataSet): Integer;
var
  minParentId: Integer;
  //idRec: TFPHashList;
  id, parentId: Integer;
  name, value: string;
begin
  Result := 0;
  if Assigned(ADataSet) and (not ADataSet.IsEmpty) then
    begin
      if Assigned(ADatum) then
        begin
          //idRec := TFPHashList.Create;
          minParentId := 80000000;
          //打出最小 parentId
          ADataSet.First;
          while not ADataSet.EOF do
            begin
              //idRec.Add(ADataSet.FieldByName('id').AsString, nil);
              parentId := ADataSet.FieldByName('parentId').AsInteger;
              if parentId < minParentId then
                minParentId := parentId;
              ADataSet.Next;
            end;
          //加入
          ADataSet.First;
          while not ADataSet.EOF do
            begin
              id := ADataSet.FieldByName('id').AsInteger;
              parentId := ADataSet.FieldByName('parentId').AsInteger;
              name := ADataSet.FieldByName('name').AsString;
              value := ADataSet.FieldByName('value').AsString;
              if parentId = minParentId then
                Self.AddParameter(id, ADatum.Id, name, value)
              else
                Self.AddParameter(id, parentId, name, value);
              //
              Result := Result + 1;
              ADataSet.Next;
            end;
        end
      else
        begin
          ADataSet.First;
          while not ADataSet.EOF do
            begin
              id := ADataSet.FieldByName('id').AsInteger;
              parentId := ADataSet.FieldByName('parentId').AsInteger;
              name := ADataSet.FieldByName('name').AsString;
              value := ADataSet.FieldByName('value').AsString;
              Self.AddParameter(id, parentId, name, value);
              Result := Result + 1;
              ADataSet.Next;
            end;
        end;
    end;
end;

function TCMParameterSet.AddParameters(ADatum: ICMParameter; ANode: TCMDOMNode): Integer;
var
  name, value: string;
  cell: TCMParameterCell;
  procedure addChildren(pCell: TCMParameterCell; node: TCMDOMNode);
  var i: Integer; p2Cell: TCMParameterCell;
  begin
    for i:=0 to node.ChildCount-1 do
      begin
        if node.ChildNodes[i].AttributeExists('name') then
          p2Cell := Self.AddParameter(pCell, node.ChildNodes[i].GetAttribute('name'), node.ChildNodes[i].Text)
        else
          p2Cell := Self.AddParameter(pCell, node.ChildNodes[i].Name, node.ChildNodes[i].Text);
        Result := Result + 1;
        addChildren(p2Cell, node.ChildNodes[i]);
      end;
  end;
begin
  Result := 0;
  if Assigned(ANode) then
    begin
      if ANode.AttributeExists('name') then
        name := ANode.GetAttribute('name')
      else
        name := ANode.Name;
      value := ANode.Text;
      //
      cell := nil;
      if Assigned(ADatum) then
        begin
          cell := Self.GetParameter(ADatum.Id);
        end;
      cell := Self.AddParameter(cell, name, value);
      Result := Result + 1;
      addChildren(cell, ANode);
    end;
end;


initialization
  ParameterFormatSettings := CMFormatSettings;


end.

