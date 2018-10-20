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
  Classes, SysUtils, Contnrs,
  cm_interfaces;

type

  { TCMHashObjectList }

  TCMHashObjectList<T: class> = class
  private
    FObjectList: TFPHashObjectList;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    procedure SetCapacity(AValue: Integer);
    procedure SetCount(AValue: Integer);
    procedure SetItem(Index: Integer; AValue: T);
  private
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(AValue: Boolean);
  public
    constructor Create(FreeObjects: Boolean=True);
    destructor Destroy; override;
    procedure Clear;
    function Add(const AName: ShortString; AObject: T): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
    function HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Delete(Index: Integer);
    function Remove(AObject: T): Integer; inline;
    function IndexOf(AObject: T): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Find(const s: shortstring): T; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindIndexOf(const s: shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindWithHash(const AName: shortstring; AHash:LongWord): T;
    function Rename(const AOldName, ANewName: shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    //
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  { TInterfaceItem }

  TInterfaceItem<T: IUnknown> = class
  private
    FInft: T;
  public
    constructor Create(AInft: T);
    destructor Destroy; override;
    property Inft: T read FInft;
  end;

  { TCMHashInterfaceList
    // 因考虑到接口自动生命周期与其实现难度的问题，用 TInterfaceItem<T: IUnknown> 对接口进行托管处理，
    如此则存放量大时诸如：Remove(AInft: T)、IndexOf(AObject: T) 效率将下降。
  }

  TCMHashInterfaceList<T: IUnknown> = class
  private
    FObjectList: TFPHashObjectList;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    procedure SetCapacity(AValue: Integer);
    procedure SetCount(AValue: Integer);
    procedure SetItem(Index: Integer; AValue: T);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const AName: ShortString; AInft: T): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
    function HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Delete(Index: Integer);
    function Remove(AInft: T): Integer; inline;
    function IndexOf(AInft: T): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Find(const s: shortstring): T; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindIndexOf(const s: shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindWithHash(const AName: shortstring; AHash:LongWord): T;
    function Rename(const AOldName, ANewName: shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

  IIterator<E: IUnknown> = interface(ICMBase)
    ['{4729D3B9-0279-4CE0-905A-C99ADC8E34B9}']
    function HasNext: Boolean; //如果仍有元素可以迭代，则返回 true。
    function Next: E; //返回迭代的下一个元素。
  end;

  { TCMMapEntry }

  TCMMapEntry<K: ICMBase; V: IUnknown> = class
  private
    FK: K;
    FV: V;
  public
    constructor Create(AKey: K; AValue: V);
    destructor Destroy; override;
    property Key: K read FK;
    property Value: V read FV;
  end;

  { TCMHashInterfaceMap }

  TCMHashInterfaceMap<K: ICMBase; V: IUnknown> = class
  private
    FList: TFPHashObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function ContainsKey(AKey: K): Boolean;
    function Get(AKey: K): V;
    function IsEmpty: Boolean;
    function Put(AKey: K; AValue: V): V;  //返回以前与 key 关联的值，如果没有针对 key 的映射关系，则返回 nil。
    function Remove(AKey: K): V; //返回以前与 key 关联的值。
    function Size: Integer;
    //TODO 迭代
  end;


  { TCMInterfaceListEnumerator
    // 此语法上不支持泛型定义前置，只能先如此
  }
  TCMInterfaceListEnumerator<T: IUnknown> = class abstract
  public
    function GetCurrent: T; virtual; abstract;
    function MoveNext: Boolean; virtual; abstract;
    property Current: T read GetCurrent;
  end;

  { TCMInterfaceList
    // copy and update by TInterfaceList
    // TODO 应直接实现为好
  }
  TCMInterfaceList<T: IUnknown> = class
  private
    FList : TInterfaceList;
  protected type
    TCMInterfaceListEnumeratorImpl = class(TCMInterfaceListEnumerator<T>)
    private
      FList: TCMInterfaceList<T>;
      FPosition: Integer;
    public
      constructor Create(AList: TCMInterfaceList<T>);
      function GetCurrent: T; override;
      function MoveNext: Boolean; override;
    end;
  protected
    function Get(i: Integer) : T;
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure Put(i: Integer; item: T);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear;
    procedure Delete(index: Integer);
    procedure Exchange(index1, index2: Integer);
    function First: T;
    function GetEnumerator: TCMInterfaceListEnumerator<T>;
    function IndexOf(item: T): Integer;
    function Add(item: T): Integer;
    procedure Insert(i: Integer; item: T);
    function Last: T;
    function Remove(item: T): Integer;
    procedure Lock;
    procedure Unlock;
    //
    function Expand: TCMInterfaceList<T>;
    //
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer] : T read Get write Put; default;
  end;

implementation

{ TCMHashObjectList }

function TCMHashObjectList<T>.GetCapacity: Integer;
begin
  Result := FObjectList.Capacity;
end;

function TCMHashObjectList<T>.GetCount: Integer;
begin
  Result := FObjectList.Count;
end;

function TCMHashObjectList<T>.GetItem(Index: Integer): T;
begin
  Result := T(FObjectList[Index]);
end;

procedure TCMHashObjectList<T>.SetCapacity(AValue: Integer);
begin
  FObjectList.Capacity := AValue;
end;

procedure TCMHashObjectList<T>.SetCount(AValue: Integer);
begin
  FObjectList.Count := AValue;
end;

procedure TCMHashObjectList<T>.SetItem(Index: Integer; AValue: T);
begin
  FObjectList[Index] := AValue;
end;

function TCMHashObjectList<T>.GetOwnsObjects: Boolean;
begin
  Result := FObjectList.OwnsObjects;
end;

procedure TCMHashObjectList<T>.SetOwnsObjects(AValue: Boolean);
begin
  FObjectList.OwnsObjects := AValue;
end;

constructor TCMHashObjectList<T>.Create(FreeObjects: Boolean);
begin
  FObjectList := TFPHashObjectList.Create(FreeObjects);
end;

destructor TCMHashObjectList<T>.Destroy;
begin
  FObjectList.Free;
  inherited Destroy;
end;

procedure TCMHashObjectList<T>.Clear;
begin
  FObjectList.Clear;
end;

function TCMHashObjectList<T>.Add(const AName: ShortString; AObject: T): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.Add(AName, AObject);
end;

function TCMHashObjectList<T>.NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.NameOfIndex(Index);
end;

function TCMHashObjectList<T>.HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.HashOfIndex(Index);
end;

procedure TCMHashObjectList<T>.Delete(Index: Integer);
begin
  FObjectList.Delete(Index);
end;

function TCMHashObjectList<T>.Remove(AObject: T): Integer;
begin
  Result := FObjectList.Remove(AObject);
end;

function TCMHashObjectList<T>.IndexOf(AObject: T): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.IndexOf(AObject);
end;

function TCMHashObjectList<T>.Find(const s: shortstring): T; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := T(FObjectList.Find(s));
end;

function TCMHashObjectList<T>.FindIndexOf(const s: shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.FindIndexOf(s);
end;

function TCMHashObjectList<T>.FindWithHash(const AName: shortstring; AHash: LongWord): T;
begin
  Result := T(FObjectList.FindWithHash(AName, AHash));
end;

function TCMHashObjectList<T>.Rename(const AOldName, ANewName: shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.Rename(AOldName, ANewName);
end;

{ TInterfaceItem }

constructor TInterfaceItem<T>.Create(AInft: T);
begin
  FInft := AInft;
end;

destructor TInterfaceItem<T>.Destroy;
begin
  IUnknown(FInft) := nil;
  inherited Destroy;
end;

{ TCMHashInterfaceList }

function TCMHashInterfaceList<T>.GetCapacity: Integer;
begin
  Result := FObjectList.Capacity;
end;

function TCMHashInterfaceList<T>.GetCount: Integer;
begin
  Result := FObjectList.Count;
end;

function TCMHashInterfaceList<T>.GetItem(Index: Integer): T;
begin
  Result := TInterfaceItem<T>(FObjectList[Index]).Inft;
end;

procedure TCMHashInterfaceList<T>.SetCapacity(AValue: Integer);
begin
  FObjectList.Capacity := AValue;
end;

procedure TCMHashInterfaceList<T>.SetCount(AValue: Integer);
begin
  FObjectList.Count := AValue;
end;

procedure TCMHashInterfaceList<T>.SetItem(Index: Integer; AValue: T);
begin
  TInterfaceItem<T>(FObjectList[Index]).FInft := AValue;
end;

constructor TCMHashInterfaceList<T>.Create;
begin
  FObjectList := TFPHashObjectList.Create(True);
end;

destructor TCMHashInterfaceList<T>.Destroy;
begin
  FObjectList.Free;
  inherited Destroy;
end;

procedure TCMHashInterfaceList<T>.Clear;
begin
  FObjectList.Clear;
end;

function TCMHashInterfaceList<T>.Add(const AName: ShortString; AInft: T): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.Add(AName, TInterfaceItem<T>.Create(AInft));
end;

function TCMHashInterfaceList<T>.NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.NameOfIndex(Index);
end;

function TCMHashInterfaceList<T>.HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.HashOfIndex(Index);
end;

procedure TCMHashInterfaceList<T>.Delete(Index: Integer);
begin
  FObjectList.Delete(Index);
end;

function TCMHashInterfaceList<T>.Remove(AInft: T): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := Self.IndexOf(AInft);
  if i >= 0 then
    begin
      FObjectList.Delete(i);
      Result := i;
    end;
end;

function TCMHashInterfaceList<T>.IndexOf(AInft: T): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
var
  i: Integer;
begin
  Result := -1;
  for i:=0 to FObjectList.Count-1 do
    begin
      if TInterfaceItem<T>(FObjectList[i]).Inft = AInft then
        begin
          Result := i;
          Exit;
        end;
    end;
end;

function TCMHashInterfaceList<T>.Find(const s: shortstring): T; {$ifdef CCLASSESINLINE}inline;{$endif}
var
  item: TInterfaceItem<T>;
begin
  Result := T(nil);
  item := TInterfaceItem<T>(FObjectList.Find(s));
  if Assigned(item) then
    Result := item.Inft;
end;

function TCMHashInterfaceList<T>.FindIndexOf(const s: shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.FindIndexOf(s);
end;

function TCMHashInterfaceList<T>.FindWithHash(const AName: shortstring; AHash: LongWord): T;
var
  item: TInterfaceItem<T>;
begin
  Result := T(nil);
  item := TInterfaceItem<T>(FObjectList.FindWithHash(AName, AHash));
  if Assigned(item) then
    Result := item.Inft;
end;

function TCMHashInterfaceList<T>.Rename(const AOldName, ANewName: shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FObjectList.Rename(AOldName, ANewName);
end;

{ TCMMapEntry }

constructor TCMMapEntry<K, V>.Create(AKey: K; AValue: V);
begin
  FK := AKey;
  FV := AValue;
end;

destructor TCMMapEntry<K, V>.Destroy;
begin
  FK := K(nil);
  FV := V(nil);
  inherited Destroy;
end;

{ TCMHashInterfaceMap }

constructor TCMHashInterfaceMap<K, V>.Create;
begin
  FList := TFPHashObjectList.Create(True);
end;

destructor TCMHashInterfaceMap<K, V>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TCMHashInterfaceMap<K, V>.Clear;
begin
  FList.Clear;
end;

function TCMHashInterfaceMap<K, V>.ContainsKey(AKey: K): Boolean;
begin
  Result := FList.FindIndexOf(IntToStr(ICMBase(AKey).GetHashCode)) >= 0;
end;

function TCMHashInterfaceMap<K, V>.Get(AKey: K): V;
var
  me: TCMMapEntry<K, V>;
begin
  Result := V(nil);
  me := TCMMapEntry<K, V>(FList.Find(IntToStr(ICMBase(AKey).GetHashCode)));
  if Assigned(me) then
    Result := me.Value;
end;

function TCMHashInterfaceMap<K, V>.IsEmpty: Boolean;
begin
  Result := FList.Count <= 0;
end;

function TCMHashInterfaceMap<K, V>.Put(AKey: K; AValue: V): V;
var
  me: TCMMapEntry<K, V>;
begin
  Result := V(nil);
  me := TCMMapEntry<K, V>.Create(AKey, AValue);
  if FList.Add(IntToStr(ICMBase(AKey).GetHashCode), me) >= 0 then
    Result := AValue;
end;

function TCMHashInterfaceMap<K, V>.Remove(AKey: K): V;
var
  me: TCMMapEntry<K, V>;
begin
  Result := V(nil);
  me := TCMMapEntry<K, V>(FList.Find(IntToStr(ICMBase(AKey).GetHashCode)));
  if Assigned(me) then
    begin
      Result := me.Value;
      if FList.Remove(me) >= 0 then
        Result := V(nil);
    end;
end;

function TCMHashInterfaceMap<V, K>.Size: Integer;
begin
  Result := FList.Count;
end;

{ TCMInterfaceList }

function TCMInterfaceList<T>.Get(i: Integer): T;
begin
  Result := T(FList[i]);
end;

function TCMInterfaceList<T>.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TCMInterfaceList<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TCMInterfaceList<T>.Put(i: Integer; item: T);
begin
  FList[i] := item;
end;

procedure TCMInterfaceList<T>.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity := NewCapacity;
end;

procedure TCMInterfaceList<T>.SetCount(NewCount: Integer);
begin
  FList.Count := NewCount;
end;

constructor TCMInterfaceList<T>.Create;
begin
  FList := TInterfaceList.Create;
end;

destructor TCMInterfaceList<T>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TCMInterfaceList<T>.Clear;
begin
  FList.Clear;
end;

procedure TCMInterfaceList<T>.Delete(index: Integer);
begin
  FList.Delete(index);
end;

procedure TCMInterfaceList<T>.Exchange(index1, index2: Integer);
begin
  FList.Exchange(index1, index2);
end;

function TCMInterfaceList<T>.First: T;
begin
  Result := T(FList.First);
end;

function TCMInterfaceList<T>.GetEnumerator: TCMInterfaceListEnumerator<T>;
begin
  Result := TCMInterfaceListEnumeratorImpl.Create(Self);
end;

function TCMInterfaceList<T>.IndexOf(item: T): Integer;
begin
  Result := FList.IndexOf(item);
end;

function TCMInterfaceList<T>.Add(item: T): Integer;
begin
  Result := FList.Add(item);
end;

procedure TCMInterfaceList<T>.Insert(i: Integer; item: T);
begin
  FList.Insert(i, item);
end;

function TCMInterfaceList<T>.Last: T;
begin
  Result := T(FList.Last);
end;

function TCMInterfaceList<T>.Remove(item: T): Integer;
begin
  Result := FList.Remove(item);
end;

procedure TCMInterfaceList<T>.Lock;
begin
  FList.Lock;
end;

procedure TCMInterfaceList<T>.Unlock;
begin
  FList.Unlock;
end;

function TCMInterfaceList<T>.Expand: TCMInterfaceList<T>;
begin
  FList.Expand;
  Result := Self;
end;

{ TCMInterfaceList.TCMInterfaceListEnumeratorImpl }

constructor TCMInterfaceList<T>.TCMInterfaceListEnumeratorImpl.Create(AList: TCMInterfaceList<T>);
begin
  inherited create;
  FList := AList;
  FPosition:=-1;
end;

function TCMInterfaceList<T>.TCMInterfaceListEnumeratorImpl.GetCurrent: T;
begin
  Result := FList[FPosition];
end;

function TCMInterfaceList<T>.TCMInterfaceListEnumeratorImpl.MoveNext: Boolean;
begin
  Result := False;
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;



end.

