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

  { TCMParameterRec }  //参数移除时可能还有引用者的存在,所以不能直接释放

  TCMParameterRec = class
  private
    FParam: ICMParameter;
    FChildClues: TStrings;
  public
    constructor Create(AParam: ICMParameter);
    destructor Destroy; override;
    property Param: ICMParameter read FParam;
    property ChildClues: TStrings read FChildClues;
  end;

  { TCMParameterSet }

  TCMParameterSet = class(TCMBase, ICMParameterLoader)
  private
    FRecordList: TFPHashObjectList;
    FClueList: TFPHashObjectList;
    procedure Remove(ARec: TCMParameterRec);
    function Find(const AClue: string): TCMParameterRec;
  public
    constructor Create;
    destructor Destroy; override;
    function RecordParameter(AParameter: ICMParameter): TCMParameterRec;
    procedure RemoveParameter(AParameter: ICMParameter);
    function GetParameter(const AClue: string): ICMParameter;
    function GetParameterRec(AParameterId: Integer): TCMParameterRec;
  public
    function AddParameters(ABase: ICMParameter; ADataSet: TDataSet): Integer;
    function AddParameters(ABase: ICMParameter; ANode: TCMDOMNode): Integer;
  end;

  { TCMParameter }

  TCMParameter = class(TCMBase, ICMParameter)
  private
    FParameterSet: TCMParameterSet;
    FDataSync: TSynchroObject;
  private
    FId: Integer;
    FParentId: Integer;
    FLevel: Integer;
    FName: string;
    FClue: string;
    FParameterData: ICMParameterData;
    function getParameterData: ICMParameterData;
    procedure setParent(AParent: TCMParameter);
  protected
    property ParameterData: ICMParameterData read getParameterData;
  public
    constructor Create(const AName: string); virtual;
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Boolean);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: TDateTime);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Currency);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Double);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Integer);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Int64);
    constructor Create(AParent: TCMParameter; const AName, AValue: string);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: Pointer);
    constructor Create(AParent: TCMParameter; const AName: string; AValue: TObject);
    destructor Destroy; override;
    procedure AfterConstruction; override;
  public
    function Id: Integer;
    function ParentId: Integer;
    function Level: Integer;
    function Name: string;
    function Clue: string;
    function ItemCount: Integer;
    function GetItem(AIndex: Integer): ICMParameter;
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
    function AsPointer: Pointer;
    function AsObject: TObject;
  public
    function AddBoolean(const AName: string; AValue: Boolean): ICMParameter;
    function AddDateTime(const AName: string; AValue: TDateTime): ICMParameter;
    function AddCurrency(const AName: string; AValue: Currency): ICMParameter;
    function AddFloat(const AName: string; AValue: Double): ICMParameter;
    function AddInteger(const AName: string; AValue: Integer): ICMParameter;
    function AddLargeInt(const AName: string; AValue: Int64): ICMParameter;
    function AddString(const AName, AValue: string): ICMParameter;
    function AddPointer(const AName: string; AValue: Pointer): ICMParameter;
    function AddObject(const AName: string; AValue: TObject): ICMParameter;
  public
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

  { TCMLoadParameter } //仅用于特殊加载

  TCMLoadParameter = class(TCMParameter)
  public
    constructor Create(AParameterSet: TCMParameterSet; AId, AParentId: Integer; const AName: string); overload;
  end;


ResourceString
  SInvalidName = '"%s" is not a valid parameter name';
  SInvalidID   = '"%d" is not a valid parameter ID';
  SInvalidBaseParameter = 'Benchmark parameters of ID "%d" does not exist';

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

{ TCMParameterRec }

constructor TCMParameterRec.Create(AParam: ICMParameter);
begin
  FParam := AParam;
  FChildClues := TStringList.Create;
end;

destructor TCMParameterRec.Destroy;
begin
  FParam.Clear;
  FParam := nil;
  FChildClues.Free;
  inherited Destroy;
end;

{ TCMParameterSet }

constructor TCMParameterSet.Create;
begin
  FRecordList := TFPHashObjectList.Create(True);
  FClueList := TFPHashObjectList.Create(False);
end;

destructor TCMParameterSet.Destroy;
begin
  FClueList.Free;
  FRecordList.Free;
  inherited Destroy;
end;

function TCMParameterSet.RecordParameter(AParameter: ICMParameter): TCMParameterRec;
begin
  Result := TCMParameterRec.Create(AParameter);
  FRecordList.Add(IntToStr(AParameter.Id), Result);
  FClueList.Add(AParameter.Clue, Result);
end;

procedure TCMParameterSet.RemoveParameter(AParameter: ICMParameter);
var
  i: Integer;
begin
  i := FClueList.FindIndexOf(IntToStr(AParameter.Id));
  if i >= 0 then
    FClueList.Delete(i);
  i := FClueList.FindIndexOf(AParameter.Clue);
  if i >= 0 then
    FRecordList.Delete(i);
end;

procedure TCMParameterSet.Remove(ARec: TCMParameterRec);
begin
  FClueList.Remove(ARec);
  FRecordList.Remove(ARec);
end;

function TCMParameterSet.Find(const AClue: string): TCMParameterRec;
begin
  Result := TCMParameterRec(FClueList.Find(AClue));
end;

function TCMParameterSet.GetParameter(const AClue: string): ICMParameter;
var
  rec: TCMParameterRec;
begin
  Result := nil;
  rec := Self.Find(AClue);
  if Assigned(rec) then
    Result := rec.Param;
end;

function TCMParameterSet.GetParameterRec(AParameterId: Integer): TCMParameterRec;
begin
  Result := TCMParameterRec(FRecordList.Find(IntToStr(AParameterId)));
end;

function TCMParameterSet.AddParameters(ABase: ICMParameter; ADataSet: TDataSet): Integer;
var
  minParentId: Integer;
  id, parentId: Integer;
  name, value: string;
  loadParam: TCMLoadParameter;
begin
  Result := 0;
  if Assigned(ADataSet) and (not ADataSet.IsEmpty) then
    begin
      if Assigned(ABase) and (FRecordList.FindIndexOf(IntToStr(ABase.Id)) < 0) then
        begin
          raise EParameterError.CreateFmt(SInvalidBaseParameter, [ABase.Id]);
          Exit;
        end;
      minParentId := AutoParameterIdStart;
      //找出最小 parentId
      ADataSet.First;
      while not ADataSet.EOF do
        begin
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
            begin
              if Assigned(ABase) then
                loadParam := TCMLoadParameter.Create(Self, id, ABase.Id, name)
              else
                loadParam := TCMLoadParameter.Create(Self, id, -1, name);
            end
          else
            loadParam := TCMLoadParameter.Create(Self, id, parentId, name);
          loadParam.FParameterData := TCMStringParameterData.Create(value);
          //
          Result := Result + 1;
          ADataSet.Next;
        end;
    end;
end;

function TCMParameterSet.AddParameters(ABase: ICMParameter; ANode: TCMDOMNode): Integer;
var
  name, value: string;
  rootParam: TCMLoadParameter;
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
      if ANode.AttributeExists('name') then
        name := ANode.GetAttribute('name')
      else
        name := ANode.Name;
      value := ANode.Text;
      //
      if Assigned(ABase) then
        rootParam:= TCMLoadParameter.Create(Self, AutoParameterIdStart + Abs(ANode.GetHashCode), ABase.Id, name)
      else
        rootParam:= TCMLoadParameter.Create(Self, AutoParameterIdStart + Abs(ANode.GetHashCode), -1, name);
      rootParam.FParameterData := TCMStringParameterData.Create(value);
      Result := Result + 1;
      addChildren(rootParam, ANode);
    end;
end;

{ TCMLoadParameter }

constructor TCMLoadParameter.Create(AParameterSet: TCMParameterSet; AId, AParentId: Integer; const AName: string);
var
  rec: TCMParameterRec;
begin
  if (AName='') or not IsValidIdent(AName) then
    raise EParameterError.CreateFmt(SInvalidName, [AName]);
  if AId < 0 then
    raise EParameterError.CreateFmt(SInvalidID, [AId]);
  FParameterSet := AParameterSet;
  FId := AId;
  FParentId := AParentId;
  FLevel := 1;
  FName := AName;
  FClue := AName;
  //
  rec := FParameterSet.GetParameterRec(FParentId);
  if Assigned(rec) then
    begin
      FLevel := rec.Param.Level + 1;
      FClue := Format('%s.%s', [rec.Param.Clue, FName]);
      rec.ChildClues.Add(FClue);
    end;
  FParameterData := nil;
  FDataSync := TSynchroObject.Create;
end;

{ TCMParameter }

constructor TCMParameter.Create(const AName: string);
begin
  if (AName='') or not IsValidIdent(AName) then
    raise EParameterError.CreateFmt(SInvalidName, [AName]);
  FId := AutoParameterIdStart + Abs(Self.GetHashCode);
  FParentId := -1;
  FLevel := 1;
  FName := AName;
  FClue := AName;
  FParameterData := nil;
  FParameterSet := nil;
  FDataSync := TSynchroObject.Create;
end;

procedure TCMParameter.setParent(AParent: TCMParameter);
var
  rec: TCMParameterRec;
begin
  if Assigned(AParent) then
    begin
      FParameterSet := AParent.FParameterSet;
      FParentId := AParent.Id;
      FLevel := AParent.Level + 1;
      FClue := Format('%s.%s', [AParent.Clue, FName]);
      rec := FParameterSet.GetParameterRec(AParent.Id);
      if Assigned(rec) then
        rec.ChildClues.Add(FClue);
    end;
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Boolean);
begin
  Self.Create(AName);
  setParent(AParent);
  FParameterData := TCMBooleanParameterData.Create(AValue);
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: TDateTime);
begin
  Self.Create(AName);
  setParent(AParent);
  FParameterData := TCMDateTimeParameterData.Create(AValue);
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Currency);
begin
  Self.Create(AName);
  setParent(AParent);
  FParameterData := TCMCurrencyParameterData.Create(AValue);
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Double);
begin
  Self.Create(AName);
  setParent(AParent);
  FParameterData := TCMFloatParameterData.Create(AValue);
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Integer);
begin
  Self.Create(AName);
  setParent(AParent);
  FParameterData := TCMIntegerParameterData.Create(AValue);
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Int64);
begin
  Self.Create(AName);
  setParent(AParent);
  FParameterData := TCMLargeIntParameterData.Create(AValue);
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName, AValue: string);
begin
  Self.Create(AName);
  setParent(AParent);
  FParameterData := TCMStringParameterData.Create(AValue);
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: Pointer);
begin
  Self.Create(AName);
  setParent(AParent);
  FParameterData := TCMPointerParameterData.Create(AValue);
end;

constructor TCMParameter.Create(AParent: TCMParameter; const AName: string; AValue: TObject);
begin
  Self.Create(AName);
  setParent(AParent);
  FParameterData := TCMObjectParameterData.Create(AValue);
end;

destructor TCMParameter.Destroy;
begin
  FParameterData := nil;
  if Assigned(FParameterSet) then
    begin
      FParameterSet.RemoveParameter(Self);
      if ParentId = -1 then
        FParameterSet.Free;
    end;
  inherited Destroy;
end;

procedure TCMParameter.AfterConstruction;
begin
  inherited AfterConstruction;
  if not Assigned(FParameterSet) then
    FParameterSet := TCMParameterSet.Create;
  FParameterSet.RecordParameter(Self);
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
  rec: TCMParameterRec;
begin
  Result := 0;
  if Assigned(FParameterSet) then
    begin
      rec := FParameterSet.GetParameterRec(Id);
      if Assigned(rec) then
        Result := rec.ChildClues.Count;
    end;
end;

function TCMParameter.GetItem(AIndex: Integer): ICMParameter;
var
  rec: TCMParameterRec;
begin
  Result := nil;
  if Assigned(FParameterSet) then
    begin
      rec := FParameterSet.GetParameterRec(Id);
      if Assigned(rec) then
        Result := FParameterSet.GetParameter(rec.ChildClues[AIndex]);
    end;
end;

procedure TCMParameter.RemoveItems;
var
  rec, cRec: TCMParameterRec;
  i: Integer;
begin
  rec := FParameterSet.GetParameterRec(Id);
  if Assigned(rec) then
    begin
      for i:=0 to rec.ChildClues.Count-1 do
        begin
          cRec := FParameterSet.Find(rec.ChildClues[i]);
          if Assigned(cRec) then
            FParameterSet.Remove(cRec);
        end;
      rec.ChildClues.Clear;
    end;
end;

function TCMParameter.Get(const AParameterName: string): ICMParameter;
begin
  Result := FParameterSet.GetParameter(AParameterName);
  if not Assigned(Result) then
    Result := TCMParameter.Create('nil');
end;

//private
function TCMParameter.getParameterData: ICMParameterData;
begin
  if not Assigned(FParameterData) then
    FParameterData := TCMParameterData.Create;
  Result := FParameterData;
end;

procedure TCMParameter.Clear;
begin
  FDataSync.Acquire;
  try
    ParameterData.Clear;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.DataType: TParameterDataType;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.DataType;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.IsNull: Boolean;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.IsNull;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.AsBoolean: Boolean;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.AsBoolean;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.AsDateTime: TDateTime;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.AsDateTime;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.AsCurrency: Currency;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.AsCurrency;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.AsFloat: Double;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.AsFloat;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.AsInteger: Integer;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.AsInteger;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.AsLargeInt: Int64;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.AsLargeInt;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.AsString: string;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.AsString;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.AsPointer: Pointer;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.AsPointer;
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.AsObject: TObject;
begin
  FDataSync.Acquire;
  try
    Result := ParameterData.AsObject;
  finally
    FDataSync.Release;
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

function TCMParameter.AddPointer(const AName: string; AValue: Pointer): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.AddObject(const AName: string; AValue: TObject): ICMParameter;
begin
  Result := TCMParameter.Create(Self, AName, AValue);
end;

function TCMParameter.ReBoolean(AValue: Boolean): ICMParameter;
begin
  Result := Self;
  FDataSync.Acquire;
  try
    if Assigned(FParameterData) then
      FParameterData := nil;
    FParameterData := TCMBooleanParameterData.Create(AValue);
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.ReDateTime(AValue: TDateTime): ICMParameter;
begin
  Result := Self;
  FDataSync.Acquire;
  try
    if Assigned(FParameterData) then
      FParameterData := nil;
    FParameterData := TCMDateTimeParameterData.Create(AValue);
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.ReCurrency(AValue: Currency): ICMParameter;
begin
  Result := Self;
  FDataSync.Acquire;
  try
    if Assigned(FParameterData) then
      FParameterData := nil;
    FParameterData := TCMCurrencyParameterData.Create(AValue);
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.ReFloat(AValue: Double): ICMParameter;
begin
  Result := Self;
  FDataSync.Acquire;
  try
    if Assigned(FParameterData) then
      FParameterData := nil;
    FParameterData := TCMFloatParameterData.Create(AValue);
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.ReInteger(AValue: Integer): ICMParameter;
begin
  Result := Self;
  FDataSync.Acquire;
  try
    if Assigned(FParameterData) then
      FParameterData := nil;
    FParameterData := TCMIntegerParameterData.Create(AValue);
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.ReLargeInt(AValue: Int64): ICMParameter;
begin
  Result := Self;
  FDataSync.Acquire;
  try
    if Assigned(FParameterData) then
      FParameterData := nil;
    FParameterData := TCMLargeIntParameterData.Create(AValue);
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.ReString(const AValue: string): ICMParameter;
begin
  Result := Self;
  FDataSync.Acquire;
  try
    if Assigned(FParameterData) then
      FParameterData := nil;
    FParameterData := TCMStringParameterData.Create(AValue);
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.RePointer(AValue: Pointer): ICMParameter;
begin
  Result := Self;
  FDataSync.Acquire;
  try
    if Assigned(FParameterData) then
      FParameterData := nil;
    FParameterData := TCMPointerParameterData.Create(AValue);
  finally
    FDataSync.Release;
  end;
end;

function TCMParameter.ReObject(AValue: TObject): ICMParameter;
begin
  Result := Self;
  FDataSync.Acquire;
  try
    if Assigned(FParameterData) then
      FParameterData := nil;
    FParameterData := TCMObjectParameterData.Create(AValue);
  finally
    FDataSync.Release;
  end;
end;


initialization
  ParameterFormatSettings := CMFormatSettings;


end.

