{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_generics

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_generics;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type

  { TGFPHashList
    // 接口不增加引用计数
  }

  TGFPHashList<T> = class(TFPHashList)
  protected
    function Get(Index: Integer): T;
    procedure Put(Index: Integer; AValue: T);
  public
    function Add(const AName: shortstring; Item: T): Integer;
    function Expand: TGFPHashList<T>;
    function Extract(Item: T): T;
    function IndexOf(Item: T): Integer;
    function Find(const AName: shortstring): T;
    function FindWithHash(const AName: shortstring; AHash: LongWord): T;
    function Remove(Item: T): Integer;
    property Items[Index: Integer]: T read Get write Put; default;
  end;

  { TGFPHashObjectList }

  TGFPHashObjectList<T: class> = class(TFPHashObjectList)
  protected
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; AValue: T);
  public
    function Add(const AName: shortstring; Item: T): Integer;
    function Expand: TGFPHashObjectList<T>;
    function Extract(Item: T): T;
    function IndexOf(Item: T): Integer;
    function Find(const AName: shortstring): T;
    function FindWithHash(const AName: shortstring; AHash: LongWord): T;
    function Remove(Item: T): Integer;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

  { TGInterfaceListEnumerator }

  TGInterfaceListEnumerator<T: IUnknown> = class
  private
    FList: TInterfaceList;
    FPosition: Integer;
  public
    constructor Create(AList: TInterfaceList);
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  { TGInterfaceList }

  TGInterfaceList<T: IUnknown> = class(TInterfaceList)
  private
    function Get(Index: Integer): T;
    procedure Put(Index: Integer; AValue: T);
  public
    function First: T;
    function GetEnumerator: TGInterfaceListEnumerator<T>;
    function IndexOf(Item : T) : Integer;
    function Add(Item: T) : Integer;
    procedure Insert(Index: Integer; Item : T);
    function Last: T;
    function Remove(Item: T): Integer;
    function Expand: TGInterfaceList<T>;
    property Items[Index: Integer]: T read Get write Put; default;
  end;

  { TGFPHashInterfaceList }

  TGFPHashInterfaceList<T: IUnknown> = class(TFPHashList)
  protected
    FRecList: TFPList;
    function Get(Index: Integer): T;
    procedure Put(Index: Integer; AValue: T);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName: shortstring; Item: T): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    function Expand: TGFPHashInterfaceList<T>;
    function Extract(Item: T): T;
    function IndexOf(Item: T): Integer;
    function Find(const AName: shortstring): T;
    function FindWithHash(const AName: shortstring; AHash: LongWord): T;
    function Remove(Item: T): Integer;
    property Items[Index: Integer]: T read Get write Put; default;
  end;

implementation

{ TGFPHashList }

function TGFPHashList<T>.Get(Index: Integer): T;
begin
  Result := T(inherited Get(Index));
end;

procedure TGFPHashList<T>.Put(Index: Integer; AValue: T);
begin
  inherited Put(Index, AValue);
end;

function TGFPHashList<T>.Add(const AName: shortstring; Item: T): Integer;
begin
  Result := inherited Add(AName, Item);
end;

function TGFPHashList<T>.Expand: TGFPHashList<T>;
begin
  Result := TGFPHashList<T>(inherited Expand);
end;

function TGFPHashList<T>.Extract(Item: T): T;
begin
  Result := T(inherited Extract(Item));
end;

function TGFPHashList<T>.IndexOf(Item: T): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TGFPHashList<T>.Find(const AName: shortstring): T;
begin
  Result := T(inherited Find(AName));
end;

function TGFPHashList<T>.FindWithHash(const AName: shortstring; AHash: LongWord): T;
begin
  Result := T(FindWithHash(AName, AHash));
end;

function TGFPHashList<T>.Remove(Item: T): Integer;
begin
  Result := inherited Remove(Item);
end;

{ TGFPHashObjectList }

function TGFPHashObjectList<T>.GetItem(Index: Integer): T;
begin
  Result := T(inherited GetItem(Index));
end;

procedure TGFPHashObjectList<T>.SetItem(Index: Integer; AValue: T);
begin
  inherited SetItem(Index, AValue);
end;

function TGFPHashObjectList<T>.Add(const AName: shortstring; Item: T): Integer;
begin
  Result := inherited Add(AName, Item);
end;

function TGFPHashObjectList<T>.Expand: TGFPHashObjectList<T>;
begin
  Result := TGFPHashObjectList<T>(inherited Expand);
end;

function TGFPHashObjectList<T>.Extract(Item: T): T;
begin
  Result := T(inherited Extract(Item));
end;

function TGFPHashObjectList<T>.IndexOf(Item: T): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TGFPHashObjectList<T>.Find(const AName: shortstring): T;
begin
  Result := T(inherited Find(AName));
end;

function TGFPHashObjectList<T>.FindWithHash(const AName: shortstring; AHash: LongWord): T;
begin
  Result := T(FindWithHash(AName, AHash));
end;

function TGFPHashObjectList<T>.Remove(Item: T): Integer;
begin
  Result := inherited Remove(Item);
end;

{ TGInterfaceList }

function TGInterfaceList<T>.Get(Index: Integer): T;
begin
  Result := T(inherited Get(Index));
end;

procedure TGInterfaceList<T>.Put(Index: Integer; AValue: T);
begin
  inherited Put(Index, AValue)
end;

function TGInterfaceList<T>.First: T;
begin
  Result := T(inherited First);
end;

function TGInterfaceList<T>.GetEnumerator: TGInterfaceListEnumerator<T>;
begin
  Result := TGInterfaceListEnumerator<T>.Create(Self);
end;

function TGInterfaceList<T>.IndexOf(Item: T): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TGInterfaceList<T>.Add(Item: T): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TGInterfaceList<T>.Insert(Index: Integer; Item: T);
begin
  inherited Insert(Index, Item);
end;

function TGInterfaceList<T>.Last: T;
begin
  Result := T(inherited Last);
end;

function TGInterfaceList<T>.Remove(Item: T): Integer;
begin
  Result := inherited Remove(Item);
end;

function TGInterfaceList<T>.Expand: TGInterfaceList<T>;
begin
  Result := TGInterfaceList<T>(inherited Expand)
end;

{ TGInterfaceListEnumerator }

constructor TGInterfaceListEnumerator<T>.Create(AList: TInterfaceList);
begin
  inherited create;
  FList := AList;
  FPosition := -1;
end;

function TGInterfaceListEnumerator<T>.GetCurrent: T;
begin
  Result := T(FList[FPosition]);
end;

function TGInterfaceListEnumerator<T>.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TGFPHashInterfaceList }

function TGFPHashInterfaceList<T>.Get(Index: Integer): T;
begin
  Result := T(inherited Get(Index));
end;

procedure TGFPHashInterfaceList<T>.Put(Index: Integer; AValue: T);
var
  i: Integer;
  intf: T;
begin
  intf := Self.Items[Index];
  if (Index < FRecList.Count) and (intf = IUnknown(FRecList.List^[Index])) then
    IUnknown(FRecList.List^[Index]) := AValue
  else
    for i:=0 to FRecList.Count-1 do
      begin
        if intf = IUnknown(FRecList.List^[i]) then
          IUnknown(FRecList.List^[i]):=AValue;
      end;
  inherited Put(Index, Pointer(AValue));
end;

constructor TGFPHashInterfaceList<T>.Create;
begin
  inherited Create;
  FRecList := TFPList.Create;
end;

destructor TGFPHashInterfaceList<T>.Destroy;
begin
  inherited Destroy;
  FRecList.Free;
end;

function TGFPHashInterfaceList<T>.Add(const AName: shortstring; Item: T): Integer;
var
  i: Integer;
begin
  Result := inherited Add(AName, Pointer(Item));
  i := FRecList.Add(nil);
  IUnknown(FRecList.List^[i]) := Item;
end;

procedure TGFPHashInterfaceList<T>.Clear;
var
  i: Integer;
begin
  for i:=0 to FRecList.Count-1 do
    IUnknown(FRecList.List^[i]) := nil;
  FRecList.Clear;
  inherited Clear;
end;

procedure TGFPHashInterfaceList<T>.Delete(Index: Integer);
var
  i: Integer;
  intf: T;
begin
  if (Index >= 0) and (Index < Self.Count) then
    begin
      intf := Items[Index];
      if (Index < FRecList.Count) and (intf = IUnknown(FRecList.List^[Index])) then
        begin
          IUnknown(FRecList.List^[Index]) := nil;
          FRecList.Delete(Index);
        end
      else
        for i:=0 to FRecList.Count-1 do
          begin
            if intf = IUnknown(FRecList.List^[i]) then
              begin
                IUnknown(FRecList.List^[i]):=nil;
                FRecList.Delete(i);
              end;
          end;
    end;
  inherited Delete(Index);
end;

function TGFPHashInterfaceList<T>.Expand: TGFPHashInterfaceList<T>;
begin
  Result := TGFPHashInterfaceList<T>(inherited Expand);
end;

function TGFPHashInterfaceList<T>.Extract(Item: T): T;
begin
  Result := T(inherited Extract(Pointer(Item)));
end;

function TGFPHashInterfaceList<T>.IndexOf(Item: T): Integer;
begin
  Result := inherited IndexOf(Pointer(Item));
end;

function TGFPHashInterfaceList<T>.Find(const AName: shortstring): T;
begin
  Result := T(inherited Find(AName));
end;

function TGFPHashInterfaceList<T>.FindWithHash(const AName: shortstring; AHash: LongWord): T;
begin
  Result := T(FindWithHash(AName, AHash));
end;

function TGFPHashInterfaceList<T>.Remove(Item: T): Integer;
var
  i: Integer;
begin
  Result := inherited Remove(Pointer(Item));
  i := FRecList.IndexOf(Pointer(Item));
  if i>=0 then
    begin
      IUnknown(FRecList.List^[i]) := nil;
      FRecList.Delete(i);
    end;
end;


end.

