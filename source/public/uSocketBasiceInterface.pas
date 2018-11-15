unit uSocketBasiceInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpjson,
  cm_messager,
  cm_parameter,cm_ParameterUtils;

Type
  IBasic=Interface //--ICMMessageable
  ['{96530D3F-46C0-4D86-B247-03ABA143639E}']
  end;

  IBasicParams=Interface(ICMParameterDataList) //ICMParameterData)  //IBasic)
  ['{82959EDB-4574-4921-AD28-62CA36D3FB16}']
    //Procedure AddParam(AName:String;AValue:String); overload;
    //Procedure AddParam(AName:String;AValue:Integer); overload;
    //Procedure AddParam(AName:String;AValue:Boolean); overload;
    //Procedure AddParam(AName:String;AValue:Double); overload;
    //Procedure AddParam(AName:String;AValue:Pointer); overload;
    //Function IndexofParam(AName:String):Integer;
    //Function GetParam(AName:String;DefaultValue:String):String; overload;
    //Function GetParam(AName:String;DefaultValue:Integer):Integer; overload;
    //Function GetParam(AName:String;DefaultValue:Boolean):Boolean; overload;
    //Function GetParam(AName:String;DefaultValue:Double):Double; overload;
    //Function GetParam(AName:String;DefaultValue:Pointer):Pointer; overload;
    //Function DeleteParam(AName:String):Integer;
    //Procedure Clear();
    //Function EnumParam():String;
    Procedure CloneTo(Dest:IBasicParams);
  end;

  TBasic=Class(TCMMessageable,IBasic)

  end;

  { TBasicParam }

  TBasicParam=Class(TCMParameterDataList,IBasicParams)
  Protected
    //FParamJson:TJsonObject;
  Public
    Constructor Create();
    Destructor Destroy; override;

    //Procedure AddParam(AName:String;AValue:String); virtual; overload;
    //Procedure AddParam(AName:String;AValue:Integer); virtual; overload;
    //Procedure AddParam(AName:String;AValue:Boolean); virtual; overload;
    //Procedure AddParam(AName:String;AValue:Double);virtual;  overload;
    //Procedure AddParam(AName:String;AValue:Pointer); virtual; overload;
    //
    //Function IndexofParam(AName:String):Integer;virtual;
    //
    //Function GetParam(AName:String;DefaultValue:String):String; virtual; overload;
    //Function GetParam(AName:String;DefaultValue:Integer):Integer;virtual;  overload;
    //Function GetParam(AName:String;DefaultValue:Boolean):Boolean;virtual;  overload;
    //Function GetParam(AName:String;DefaultValue:Double):Double;virtual;  overload;
    //Function GetParam(AName:String;DefaultValue:Pointer):Pointer;virtual;  overload;

    //Function DeleteParam(AName:String):Integer;virtual;
    //Procedure Clear();virtual;
    //Function EnumParam():String;virtual;
    Procedure CloneTo(Dest:IBasicParams);virtual;
  end;




implementation

{ TBasicParam }

constructor TBasicParam.Create();
begin
  Inherited;
  //FParamJson:= TJsonObject.Create();
end;

destructor TBasicParam.Destroy;
begin
  //FParamJson.Free;
  inherited Destroy;
end;
{
procedure TBasicParam.AddParam(AName: String; AValue: String);
Var
  index:integer;
begin
  index:=IndexofParam(AName);
  if index>=0 then
    FParamJson.Delete(index);
  FParamJson.Add(AName,AValue);
end;

procedure TBasicParam.AddParam(AName: String; AValue: Integer);
Var
  index:integer;
begin
  index:=IndexofParam(AName);
  if index>=0 then
    FParamJson.Delete(index);
  FParamJson.Add(AName,AValue);
end;

procedure TBasicParam.AddParam(AName: String; AValue: Boolean);
Var
  index:integer;
begin
  index:=IndexofParam(AName);
  if index>=0 then
    FParamJson.Delete(index);
  FParamJson.Add(AName,AValue);
end;

procedure TBasicParam.AddParam(AName: String; AValue: Double);
Var
  index:integer;
begin
  index:=IndexofParam(AName);
  if index>=0 then
    FParamJson.Delete(index);
  FParamJson.Add(AName,AValue);
end;

procedure TBasicParam.AddParam(AName: String; AValue: Pointer);
Var
  index:integer;
begin
  index:=IndexofParam(AName);
  if index>=0 then
    FParamJson.Delete(index);
  FParamJson.Add(AName,DWord(AValue));
end;

function TBasicParam.IndexofParam(AName: String): Integer;
begin
  Result:= FParamJson.IndexOfName(AName);
end;

function TBasicParam.GetParam(AName: String; DefaultValue: String): String;
begin
  Result:=FParamJson.Get(AName,DefaultValue);
end;

function TBasicParam.GetParam(AName: String; DefaultValue: Integer): Integer;
begin
  Result:=FParamJson.Get(AName,DefaultValue);
end;

function TBasicParam.GetParam(AName: String; DefaultValue: Boolean): Boolean;
begin
  Result:=FParamJson.Get(AName,DefaultValue);
end;

function TBasicParam.GetParam(AName: String; DefaultValue: Double): Double;
begin
  Result:=FParamJson.Get(AName,DefaultValue);
end;

function TBasicParam.GetParam(AName: String; DefaultValue: Pointer): Pointer;
Var
  RetValue:Integer;
begin
  Result:=DefaultValue;
  RetValue:=GetParam(AName,integer(0));
  if RetValue<=0 Then exit;
  Result:= Pointer(RetValue);
end;

function TBasicParam.DeleteParam(AName: String): Integer;
Var
  Index:Integer;
begin
  Index:= IndexofParam(AName);
  Result:=Index;
  if index>=0 then
    FParamJson.Delete(index);
end;

procedure TBasicParam.Clear();
begin
  FParamJson.Clear;
end;
 }
{
 function TBasicParam.EnumParam(): String;
Var
  i:integer;
  ItemData: ICMParameterData;
begin
  For i:= 1 to Self.Count do
  Begin
    ItemData:=self.Get(i-1);
    //--TParameterDataType = (pdtUnknown, pdtNumber, pdtBoolean, pdtCurrency, pdtDateTime, pdtFloat, pdtInteger, pdtLargeInt, pdtString, pdtObject, pdtInterface, pdtPointer);
    //if ItemData.DataType;

  end;
  Result:=FParamJson.AsJSON;
end;
}
procedure TBasicParam.CloneTo(Dest: IBasicParams);
Var
  i:integer;
  ItemData: ICMParameterData;
begin
  For i:= 1 to Self.Count do
  Begin
    ItemData:=self.Get(i-1);
    Dest.SetData(self.GetName(i-1),ItemData);
  end;
end;

end.

