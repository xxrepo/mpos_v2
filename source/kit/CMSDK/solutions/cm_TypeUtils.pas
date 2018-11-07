unit cm_TypeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, TypInfo,
  cm_interfaces, cm_messager,
  cm_type;

type

  { TCMObjectGenerator }

  TCMObjectGenerator = class(TCMBase, ICMObjectGenerator)
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

  { TCMObjectPropertyReaderWriter }

  TCMObjectPropertyReaderWriter = class(TCMMessageable, ICMObjectPropertyReaderWriter)
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

{ TCMObjectGenerator }

constructor TCMObjectGenerator.Create;
begin
  inherited Create;
  FClassList := TFPHashList.Create;
end;

destructor TCMObjectGenerator.Destroy;
begin
  inherited Destroy;
end;

function TCMObjectGenerator.RegisterClass(const AClassName: string; AClass: TClass): Boolean;
begin
  Result := FClassList.Add(AClassName, AClass) >= 0;
end;

function TCMObjectGenerator.NewComponent(const AClassName: string; AOwner: TComponent): TComponent;
var
  pclass: TPersistentClass;
begin
  Result := nil;
  pclass := TPersistentClass(FClassList.Find(AClassName));
  if Assigned(pclass) then
    if TPersistentClass(pclass).InheritsFrom(TComponentClass.ClassType) then
      Result := TComponentClass(pclass).Create(AOwner);
end;

function TCMObjectGenerator.GetComponentClass(const AClassName: string): TComponentClass;
var
  pclass: TPersistentClass;
begin
  Result := nil;
  pclass := TPersistentClass(FClassList.Find(AClassName));
  if Assigned(pclass) then
    if TPersistentClass(pclass).InheritsFrom(TComponentClass.ClassType) then
      Result := TComponentClass(pclass);
end;

function TCMObjectGenerator.NewObject(const AClassName: string): TObject;
var
  pclass: TClass;
begin
  Result := nil;
  pclass := TClass(FClassList.Find(AClassName));
  if Assigned(pclass) then
    Result := TObject(pclass).Create;
end;

function TCMObjectGenerator.GetClass(const AClassName: string): TClass;
var
  pclass: TClass;
begin
  Result := nil;
  pclass := TClass(FClassList.Find(AClassName));
  if Assigned(pclass) then
    Result := TClass(pclass);
end;

{ TCMObjectPropertyReaderWriter }

function TCMObjectPropertyReaderWriter.GetOrdProp(Instance: TObject; const PropName: string): Int64;
begin
  Messager.Debug('GetOrdProp()...');
  Result := TypInfo.GetOrdProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetOrdProp(Instance: TObject; const PropName: string; Value: Int64);
begin
  Messager.Debug('SetOrdProp()...');
  TypInfo.SetOrdProp(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetEnumProp(Instance: TObject; const PropName: string): string;
begin
  Messager.Debug('GetEnumProp()...');
  Result := TypInfo.GetEnumProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetEnumProp(Instance: TObject; const PropName: string; const Value: string);
begin
  Messager.Debug('SetEnumProp()...');
  TypInfo.SetEnumProp(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetSetProp(Instance: TObject; const PropName: string): string;
begin
  Messager.Debug('GetSetProp()...');
  Result := TypInfo.GetSetProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetSetProp(Instance: TObject; const PropName: string; const Value: string);
begin
  Messager.Debug('SetSetProp()...');
  TypInfo.SetSetProp(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetStrProp(Instance: TObject; const PropName: string): string;
begin
  Messager.Debug('GetStrProp()...');
  Result := TypInfo.GetStrProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetStrProp(Instance: TObject; const PropName: string; const Value: AnsiString);
begin
  Messager.Debug('SetStrProp()...');
  TypInfo.SetStrProp(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetWideStrProp(Instance: TObject; const PropName: string): WideString;
begin
  Result := TypInfo.GetWideStrProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString);
begin
  TypInfo.SetWideStrProp(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetUnicodeStrProp(Instance: TObject; const PropName: string): UnicodeString;
begin
  Result := TypInfo.GetUnicodeStrProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetUnicodeStrProp(Instance: TObject; const PropName: string; const Value: UnicodeString);
begin
  TypInfo.SetUnicodeStrProp(Instance, PropName, Value);
end;

{$IFNDEF FPUNONE}

function TCMObjectPropertyReaderWriter.GetFloatProp(Instance: TObject; const PropName: string): Extended;
begin
  Result := TypInfo.GetFloatProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetFloatProp(Instance: TObject; const PropName: string; Value: Extended);
begin
  TypInfo.SetFloatProp(Instance, PropName, Value);
end;

{$ENDIF}

function TCMObjectPropertyReaderWriter.GetObjectProp(Instance: TObject; const PropName: string): TObject;
begin
  Result := TypInfo.GetObjectProp(Instance, PropName);
end;

function TCMObjectPropertyReaderWriter.GetObjectProp(Instance: TObject; const PropName: string; MinClass: TClass): TObject;
begin
  Result := TypInfo.GetObjectProp(Instance, PropName, MinClass);
end;

procedure TCMObjectPropertyReaderWriter.SetObjectProp(Instance: TObject; const PropName: string; Value: TObject);
begin
  TypInfo.SetObjectProp(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetObjectPropClass(Instance: TObject; const PropName: string): TClass;
begin
  Result := TypInfo.GetObjectPropClass(Instance, PropName);
end;

function TCMObjectPropertyReaderWriter.GetObjectPropClass(AClass: TClass; const PropName: string): TClass;
begin
  Result := TypInfo.GetObjectPropClass(AClass, PropName);
end;

function TCMObjectPropertyReaderWriter.GetMethodProp(Instance: TObject; const PropName: string): TMethod;
begin
  Result := TypInfo.GetMethodProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetMethodProp(Instance: TObject; const PropName: string; const Value: TMethod);
begin
  Messager.Debug('SetMethodProp()...');
  TypInfo.SetMethodProp(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetInt64Prop(Instance: TObject; const PropName: string): Int64;
begin
  Result := TypInfo.GetInt64Prop(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetInt64Prop(Instance: TObject; const PropName: string; const Value: Int64);
begin
  TypInfo.SetInt64Prop(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetPropValue(Instance: TObject; const PropName: string): Variant;
begin
  Result := TypInfo.GetPropValue(Instance, PropName);
end;

function TCMObjectPropertyReaderWriter.GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;
begin
  Result := TypInfo.GetPropValue(Instance, PropName, PreferStrings);
end;

procedure TCMObjectPropertyReaderWriter.SetPropValue(Instance: TObject; const PropName: string; const Value: Variant);
begin
  TypInfo.SetPropValue(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetVariantProp(Instance: TObject; const PropName: string): Variant;
begin
  Result := TypInfo.GetVariantProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetVariantProp(Instance: TObject; const PropName: string; const Value: Variant);
begin
  TypInfo.SetVariantProp(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetInterfaceProp(Instance: TObject; const PropName: string): IInterface;
begin
  Result := TypInfo.GetInterfaceProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetInterfaceProp(Instance: TObject; const PropName: string; const Value: IInterface);
begin
  TypInfo.SetInterfaceProp(Instance, PropName, Value);
end;

function TCMObjectPropertyReaderWriter.GetRawInterfaceProp(Instance: TObject; const PropName: string): Pointer;
begin
  Result := TypInfo.GetRawInterfaceProp(Instance, PropName);
end;

procedure TCMObjectPropertyReaderWriter.SetRawInterfaceProp(Instance: TObject; const PropName: string; const Value: Pointer);
begin
  TypInfo.SetRawInterfaceProp(Instance, PropName, Value);
end;

end.

