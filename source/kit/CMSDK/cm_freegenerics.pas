{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_freegenerics

    This is not a complete unit, for testing

    基于 free pascal 的泛型单元
    尽管有提供 fgl 单元，但其有太多的 inline 警告
    所以这里哪起炉灶

 **********************************************************************}

unit cm_freegenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_classes;


type

  { TFGFPListEnumerator }

  generic TFGFPListEnumerator<T> = class
  private
    FList: TFPList;
    FPosition: Integer;
  public
    constructor Create(AList: TFPList);
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  { TFGFPList
    // 当放入接口时不增加引用计数
  }

  generic TFGFPList<T> = class(TFPList)
  protected
    function Get(Index: Integer): T;
    procedure Put(Index: Integer; AValue: T);
  public
    type
      TFGFPListEnumeratorSpec = specialize TFGFPListEnumerator<T>;
    Procedure AddList(AList: specialize TFGFPList<T>);
    function Add(Item: T): Integer;
    function Expand: specialize TFGFPList<T>;
    function Extract(Item: T): T;
    function First: T;
    function GetEnumerator: TFGFPListEnumeratorSpec;
    function IndexOf(Item: T): Integer;
    procedure Insert(Index: Integer; Item: t);
    function Last: t;
    function Remove(Item: t): Integer;
    procedure Pack;
    property Items[Index: Integer]: T read Get write Put; default;
  end;

  { TFGInterfaceListEnumerator }

  generic TFGInterfaceListEnumerator<T: IUnknown> = class
  private
    FList: TCMInterfaceList;
    FPosition: Integer;
  public
    constructor Create(AList: TCMInterfaceList);
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  { TFGInterfaceList }

  generic TFGInterfaceList<T: IUnknown> = class(TCMInterfaceList)
  protected
    function Get(Index: Integer): T;
    procedure Put(Index: Integer; AValue: T);
  public
    type
      TFGInterfaceListEnumeratorSpec = specialize TFGInterfaceListEnumerator<T>;
    function First: T;
    function GetEnumerator: TFGInterfaceListEnumeratorSpec;
    function IndexOf(Item : T) : Integer;
    function Add(Item: T) : Integer;
    procedure Insert(Index: Integer; Item : T);
    function Last: T;
    function Remove(Item: T): Integer;
    function Expand: specialize TFGInterfaceList<T>;
    property Items[Index: Integer]: T read Get write Put; default;
  public
    function Clone: specialize TFGInterfaceList<T>;
  end;


implementation

{ TFGFPList }

function TFGFPList.Get(Index: Integer): T;
begin
  Result := T(inherited Get(Index));
end;

procedure TFGFPList.Put(Index: Integer; AValue: T);
begin
  inherited Put(Index, AValue);
end;

procedure TFGFPList.AddList(AList: specialize TFGFPList<T>);
begin
  inherited AddList(AList);
end;

function TFGFPList.Add(Item: T): Integer;
begin
  Result := inherited Add(Item);
end;

function TFGFPList.Expand: specialize TFGFPList<T>;
begin
  Result := specialize TFGFPList<T>(inherited Expand);
end;

function TFGFPList.Extract(Item: T): T;
begin
  Result := T(inherited Extract(Item));
end;

function TFGFPList.First: T;
begin
  Result := T(inherited First);
end;

function TFGFPList.GetEnumerator: TFGFPListEnumeratorSpec;
begin
  Result := TFGFPListEnumeratorSpec.Create(Self);
end;

function TFGFPList.IndexOf(Item: T): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TFGFPList.Insert(Index: Integer; Item: t);
begin
  inherited Insert(Index, Item);
end;

function TFGFPList.Last: t;
begin
  Result := T(inherited Last);
end;

function TFGFPList.Remove(Item: t): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TFGFPList.Pack;
begin
  inherited Pack;
end;

{ TFGFPListEnumerator }

constructor TFGFPListEnumerator.Create(AList: TFPList);
begin
  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TFGFPListEnumerator.GetCurrent: T;
begin
  Result := T(FList[FPosition]);
end;

function TFGFPListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TFGInterfaceList }

function TFGInterfaceList.Get(Index: Integer): T;
begin
  Result := T(inherited Get(Index));
end;

procedure TFGInterfaceList.Put(Index: Integer; AValue: T);
begin
  inherited Put(Index, AValue);
end;

function TFGInterfaceList.First: T;
begin
  Result := T(inherited First);
end;

function TFGInterfaceList.GetEnumerator: TFGInterfaceListEnumeratorSpec;
begin
  Result := TFGInterfaceListEnumeratorSpec.Create(Self);
end;

function TFGInterfaceList.IndexOf(Item: T): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TFGInterfaceList.Add(Item: T): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TFGInterfaceList.Insert(Index: Integer; Item: T);
begin
  inherited Insert(Index, Item);
end;

function TFGInterfaceList.Last: T;
begin
  Result := T(inherited Last);
end;

function TFGInterfaceList.Remove(Item: T): Integer;
begin
  Result := inherited Remove(Item);
end;

function TFGInterfaceList.Expand: specialize TFGInterfaceList<T>;
begin
  Result := specialize TFGInterfaceList<T>(inherited Expand);
end;

function TFGInterfaceList.Clone: specialize TFGInterfaceList<T>;
begin
  Result := specialize TFGInterfaceList<T>(inherited Clone);
end;

{ TFGInterfaceListEnumerator }

constructor TFGInterfaceListEnumerator.Create(AList: TCMInterfaceList);
begin
  inherited Create;
  FList:=AList;
  FPosition:=-1;
end;

function TFGInterfaceListEnumerator.GetCurrent: T;
begin
  Result := T(FList[FPosition]);
end;

function TFGInterfaceListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

end.

