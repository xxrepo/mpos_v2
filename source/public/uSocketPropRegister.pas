unit uSocketPropRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,typinfo;

Type
  TObjectPropType=(OBJ_ITEM,OBJ_CLASS,OBJ_ARRAY);

  TObjPropInfo=Record
    PropType:TObjectPropType;
    Name:String;
    PropInfo:PTypeInfo;
    ItemPropInfo:PTypeInfo;
  end;
  PObjPropInfo=^TObjPropInfo;


  { IObjectPropInterface }

  IObjectPropInterface=Interface
    ['{96F7A230-3522-4521-8A62-636DE2853D7E}']
    Procedure AddSpaceName(SpaceName:String);
    Function SoapSpaceName:String;

    Procedure AddProp(ObjPropType:TObjectPropType; Name:String; PropInfo:PTypeInfo; ItemPropInfo:PTypeInfo);
    Procedure AddItemProp(Name:String; PropInfo:PTypeInfo);
    Procedure AddClassProp(Name:String; PropInfo:PTypeInfo);
    Procedure AddArrayProp(Name:String; PropInfo:PTypeInfo;ItemPropInfo:PTypeInfo);

    Function IndexofPropInfo(PropInfo:PTypeInfo):Integer;
    Function GetItem(Index:Integer):Pointer;

    Function NewObjectPropInterface(CopyValue:Boolean=False ):IObjectPropInterface;
  end;

  { TObjectPropRegistry }

  TObjectPropRegistry=Class(TInterfacedObject,IObjectPropInterface)
  Protected
    FSoapSpaceName:String;
    FList: TList;
    Procedure DeleteFromIndex(FromIndex,ToIndex:Integer);  virtual;
  Public
    Constructor Create();
    Destructor Destroy; override;

    Procedure AddSpaceName(SpaceName:String);
    Function SoapSpaceName:String;
    Procedure AddProp(ObjPropType:TObjectPropType; Name:String; PropInfo:PTypeInfo; ItemPropInfo:PTypeInfo);   virtual;
    Procedure AddItemProp(Name:String; PropInfo:PTypeInfo); virtual;
    Procedure AddClassProp(Name:String; PropInfo:PTypeInfo); virtual;
    Procedure AddArrayProp(Name:String; PropInfo:PTypeInfo;ItemPropInfo:PTypeInfo); virtual;

    Function IndexofPropInfo(PropInfo:PTypeInfo):Integer; virtual;
    Function GetItem(Index:Integer):Pointer; virtual;

    Function NewObjectPropInterface(CopyValue:Boolean=False):IObjectPropInterface;virtual;
  end;


Var
  DefaultObjectTypeRegisty : IObjectPropInterface;
implementation

{ TObjectPropRegistry }

procedure TObjectPropRegistry.DeleteFromIndex(FromIndex, ToIndex: Integer);
Var
  i:integer;
  p:Pointer;
begin
  if ToIndex=-1 Then ToIndex:=FList.Count;
  for i:= ToIndex-1 downto FromIndex do
  Begin
    p:=FList.Items[i];
    Dispose(PObjPropInfo(P));
    p:=nil;
    FList.Delete(i);
  end;

end;

constructor TObjectPropRegistry.Create();
begin
  Inherited;
  FList:= TList.Create;
  FSoapSpaceName:='';
end;

destructor TObjectPropRegistry.Destroy;
begin
  DeleteFromIndex(0,-1);
  FList.Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TObjectPropRegistry.AddSpaceName(SpaceName: String);
begin
  if SpaceName<>FSoapSpaceName Then
    FSoapSpaceName:= SpaceName;
end;

function TObjectPropRegistry.SoapSpaceName: String;
begin
  Result:=FSoapSpaceName;
end;

procedure TObjectPropRegistry.AddProp(ObjPropType: TObjectPropType;
  Name: String; PropInfo: PTypeInfo; ItemPropInfo: PTypeInfo);
Var
  P:PObjPropInfo;
begin
  New(P);
  P^.PropType:=ObjPropType;
  P^.Name:=Name;
  P^.PropInfo:=PropInfo;
  P^.ItemPropInfo:=ItemPropInfo;
  FList.Add(P);
end;

procedure TObjectPropRegistry.AddItemProp(Name: String; PropInfo: PTypeInfo);
begin
  AddProp(OBJ_ITEM,Name,PropInfo,Nil);
end;

procedure TObjectPropRegistry.AddClassProp(Name: String; PropInfo: PTypeInfo);
begin
  AddProp(OBJ_CLASS,Name,PropInfo,Nil);
end;

procedure TObjectPropRegistry.AddArrayProp(Name: String; PropInfo: PTypeInfo;
  ItemPropInfo: PTypeInfo);
begin
  AddProp(OBJ_ARRAY,Name,PropInfo,ItemPropInfo);
end;

function TObjectPropRegistry.IndexofPropInfo(PropInfo: PTypeInfo): Integer;
Var
  i:integer;
  P:PObjPropInfo;
begin
  result:=-1;
  for i:= FList.Count downto 1 do
  Begin
    P:=PObjPropInfo(FList.Items[i-1]);
    if P^.PropInfo= PropInfo Then
    Begin
      Result:=i-1;
      break;
    end;

  end;

end;

function TObjectPropRegistry.GetItem(Index: Integer): Pointer;
begin
  Result:=nil;
  if (Index<FList.Count ) and (index>=0) then
    Result:= FList.Items[index];
end;

function TObjectPropRegistry.NewObjectPropInterface(CopyValue: Boolean
  ): IObjectPropInterface;
Var
  vObject:TObjectPropRegistry;
  P:PObjPropInfo;
  i:integer;
begin
  vObject:= TObjectPropRegistry.Create();
  if CopyValue Then
  Begin
    for i:= 1 to FList.Count do
    Begin
      New (P);
      Move(FList.Items[i-1],P,Sizeof(TObjectPropType));
      vObject.FList.Add(P);
    end;
    vObject.AddSpaceName(FSoapSpaceName);
  end;
  Result:= vObject;
end;

Initialization
  DefaultObjectTypeRegisty := TObjectPropRegistry.Create();

end.

