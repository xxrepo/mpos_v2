unit cm_TypeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, TypInfo,
  cm_interfaces, cm_messager,
  cm_type;

type

  { TCMLCLGenerator }

  TCMLCLGenerator = class(TCMBase, ICMLCLGenerator)
  private
    FClassList: TFPHashList;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterClass(const AClassName: string; AClass: TClass): Boolean;
  public
    function NewComponent(const AClassName: string; AOwner: TComponent): TComponent;
    function GetComponentClass(const AClassName: string): TComponentClass;
    function NewObject(const AClassName: string): TObject;
    function GetClass(const AClassName: string): TClass;
  end;

  { TCMLCLPropertyReaderWriter }

  TCMLCLPropertyReaderWriter = class(TCMMessageable, ICMLCLPropertyReaderWriter)
  public
    function  GetOrdProp(Instance: TObject; const PropName: string): Int64;
    procedure SetOrdProp(Instance: TObject; const PropName: string; Value: Int64);
    function  GetEnumProp(Instance: TObject; const PropName: string): string;
    procedure SetEnumProp(Instance: TObject; const PropName: string;const Value: string);
    function  GetSetProp(Instance: TObject; const PropName: string): string;
    procedure SetSetProp(Instance: TObject; const PropName: string; const Value: string);
    function  GetStrProp(Instance: TObject; const PropName: string): string;
    procedure SetStrProp(Instance: TObject; const PropName: string; const Value: AnsiString);
    function GetWideStrProp(Instance: TObject; const PropName: string): WideString;
    procedure SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString);
    //
    function GetUnicodeStrProp(Instance: TObject; const PropName: string): UnicodeString;
    procedure SetUnicodeStrProp(Instance: TObject; const PropName: string; const Value: UnicodeString);
    {$IFNDEF FPUNONE}
    function  GetFloatProp(Instance: TObject; const PropName: string): Extended;
    procedure SetFloatProp(Instance: TObject; const PropName: string; Value: Extended);
    {$ENDIF}
    function  GetObjectProp(Instance: TObject; const PropName: string): TObject;
    function  GetObjectProp(Instance: TObject; const PropName: string; MinClass: TClass): TObject;
    procedure SetObjectProp(Instance: TObject; const PropName: string; Value: TObject);
    function  GetObjectPropClass(Instance: TObject; const PropName: string): TClass;
    function  GetObjectPropClass(AClass: TClass; const PropName: string): TClass;
    //
    function  GetMethodProp(Instance: TObject; const PropName: string): TMethod;
    procedure SetMethodProp(Instance: TObject; const PropName: string; const Value: TMethod);
    //
    function  GetInt64Prop(Instance: TObject; const PropName: string): Int64;
    procedure SetInt64Prop(Instance: TObject; const PropName: string;  const Value: Int64);
    //
    function GetPropValue(Instance: TObject; const PropName: string): Variant;
    function GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;
    procedure SetPropValue(Instance: TObject; const PropName: string; const Value: Variant);
    function  GetVariantProp(Instance: TObject; const PropName: string): Variant;
    procedure SetVariantProp(Instance: TObject; const PropName: string; const Value: Variant);
    //
    function GetInterfaceProp(Instance: TObject; const PropName: string): IInterface;
    procedure SetInterfaceProp(Instance: TObject; const PropName: string; const Value: IInterface);
    //
    function GetRawInterfaceProp(Instance: TObject; const PropName: string): Pointer;
    procedure SetRawInterfaceProp(Instance: TObject; const PropName: string; const Value: Pointer);
  end;

implementation

{ TCMLCLGenerator }

constructor TCMLCLGenerator.Create;
begin
  inherited Create;
  FClassList := TFPHashList.Create;
end;

destructor TCMLCLGenerator.Destroy;
begin
  inherited Destroy;
end;

function TCMLCLGenerator.RegisterClass(const AClassName: string; AClass: TClass): Boolean;
begin
  Result := FClassList.Add(AClassName, AClass) >= 0;
end;

function TCMLCLGenerator.NewComponent(const AClassName: string; AOwner: TComponent): TComponent;
var
  pclass: TPersistentClass;
begin
  Result := nil;
  pclass := TPersistentClass(FClassList.Find(AClassName));
  if Assigned(pclass) then
    if TPersistentClass(pclass).InheritsFrom(TComponentClass.ClassType) then
      Result := TComponentClass(pclass).Create(AOwner);
end;

function TCMLCLGenerator.GetComponentClass(const AClassName: string): TComponentClass;
var
  pclass: TPersistentClass;
begin
  Result := nil;
  pclass := TPersistentClass(FClassList.Find(AClassName));
  if Assigned(pclass) then
    if TPersistentClass(pclass).InheritsFrom(TComponentClass.ClassType) then
      Result := TComponentClass(pclass);
end;

function TCMLCLGenerator.NewObject(const AClassName: string): TObject;
var
  pclass: TClass;
begin
  Result := nil;
  pclass := TClass(FClassList.Find(AClassName));
  if Assigned(pclass) then
    Result := TObject(pclass).Create;
end;

function TCMLCLGenerator.GetClass(const AClassName: string): TClass;
var
  pclass: TClass;
begin
  Result := nil;
  pclass := TClass(FClassList.Find(AClassName));
  if Assigned(pclass) then
    Result := TClass(pclass);
end;

{ TCMLCLPropertyReaderWriter }

function TCMLCLPropertyReaderWriter.GetOrdProp(Instance: TObject; const PropName: string): Int64;
begin
  Messager.Debug('GetOrdProp()...');
  Result := TypInfo.GetOrdProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetOrdProp(Instance: TObject; const PropName: string; Value: Int64);
begin
  Messager.Debug('SetOrdProp()...');
  TypInfo.SetOrdProp(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetEnumProp(Instance: TObject; const PropName: string): string;
begin
  Messager.Debug('GetEnumProp()...');
  Result := TypInfo.GetEnumProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetEnumProp(Instance: TObject; const PropName: string; const Value: string);
begin
  Messager.Debug('SetEnumProp()...');
  TypInfo.SetEnumProp(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetSetProp(Instance: TObject; const PropName: string): string;
begin
  Messager.Debug('GetSetProp()...');
  Result := TypInfo.GetSetProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetSetProp(Instance: TObject; const PropName: string; const Value: string);
begin
  Messager.Debug('SetSetProp()...');
  TypInfo.SetSetProp(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetStrProp(Instance: TObject; const PropName: string): string;
begin
  Messager.Debug('GetStrProp()...');
  Result := TypInfo.GetStrProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetStrProp(Instance: TObject; const PropName: string; const Value: AnsiString);
begin
  Messager.Debug('SetStrProp()...');
  TypInfo.SetStrProp(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetWideStrProp(Instance: TObject; const PropName: string): WideString;
begin
  Result := TypInfo.GetWideStrProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString);
begin
  TypInfo.SetWideStrProp(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetUnicodeStrProp(Instance: TObject; const PropName: string): UnicodeString;
begin
  Result := TypInfo.GetUnicodeStrProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetUnicodeStrProp(Instance: TObject; const PropName: string; const Value: UnicodeString);
begin
  TypInfo.SetUnicodeStrProp(Instance, PropName, Value);
end;

{$IFNDEF FPUNONE}

function TCMLCLPropertyReaderWriter.GetFloatProp(Instance: TObject; const PropName: string): Extended;
begin
  Result := TypInfo.GetFloatProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetFloatProp(Instance: TObject; const PropName: string; Value: Extended);
begin
  TypInfo.SetFloatProp(Instance, PropName, Value);
end;

{$ENDIF}

function TCMLCLPropertyReaderWriter.GetObjectProp(Instance: TObject; const PropName: string): TObject;
begin
  Result := TypInfo.GetObjectProp(Instance, PropName);
end;

function TCMLCLPropertyReaderWriter.GetObjectProp(Instance: TObject; const PropName: string; MinClass: TClass): TObject;
begin
  Result := TypInfo.GetObjectProp(Instance, PropName, MinClass);
end;

procedure TCMLCLPropertyReaderWriter.SetObjectProp(Instance: TObject; const PropName: string; Value: TObject);
begin
  TypInfo.SetObjectProp(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetObjectPropClass(Instance: TObject; const PropName: string): TClass;
begin
  Result := TypInfo.GetObjectPropClass(Instance, PropName);
end;

function TCMLCLPropertyReaderWriter.GetObjectPropClass(AClass: TClass; const PropName: string): TClass;
begin
  Result := TypInfo.GetObjectPropClass(AClass, PropName);
end;

function TCMLCLPropertyReaderWriter.GetMethodProp(Instance: TObject; const PropName: string): TMethod;
begin
  Result := TypInfo.GetMethodProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetMethodProp(Instance: TObject; const PropName: string; const Value: TMethod);
begin
  Messager.Debug('SetMethodProp()...');
  TypInfo.SetMethodProp(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetInt64Prop(Instance: TObject; const PropName: string): Int64;
begin
  Result := TypInfo.GetInt64Prop(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetInt64Prop(Instance: TObject; const PropName: string; const Value: Int64);
begin
  TypInfo.SetInt64Prop(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetPropValue(Instance: TObject; const PropName: string): Variant;
begin
  Result := TypInfo.GetPropValue(Instance, PropName);
end;

function TCMLCLPropertyReaderWriter.GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;
begin
  Result := TypInfo.GetPropValue(Instance, PropName, PreferStrings);
end;

procedure TCMLCLPropertyReaderWriter.SetPropValue(Instance: TObject; const PropName: string; const Value: Variant);
begin
  TypInfo.SetPropValue(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetVariantProp(Instance: TObject; const PropName: string): Variant;
begin
  Result := TypInfo.GetVariantProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetVariantProp(Instance: TObject; const PropName: string; const Value: Variant);
begin
  TypInfo.SetVariantProp(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetInterfaceProp(Instance: TObject; const PropName: string): IInterface;
begin
  Result := TypInfo.GetInterfaceProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetInterfaceProp(Instance: TObject; const PropName: string; const Value: IInterface);
begin
  TypInfo.SetInterfaceProp(Instance, PropName, Value);
end;

function TCMLCLPropertyReaderWriter.GetRawInterfaceProp(Instance: TObject; const PropName: string): Pointer;
begin
  Result := TypInfo.GetRawInterfaceProp(Instance, PropName);
end;

procedure TCMLCLPropertyReaderWriter.SetRawInterfaceProp(Instance: TObject; const PropName: string; const Value: Pointer);
begin
  TypInfo.SetRawInterfaceProp(Instance, PropName, Value);
end;

end.

