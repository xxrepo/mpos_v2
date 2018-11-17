{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_classes

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TStringArray = array of string;

  TEventTypeLevel = (etlAll, etlCustom, etlDebug, etlInfo, etlWarning, etlError, etlOff);

  { TCMThreadList
    // 原 TThreadList List 无法扩展
  }

  TCMThreadList = class
  protected
    FList: TList;
    FDuplicates: TDuplicates;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function  LockList: TList;
    procedure Remove(Item: Pointer);
    procedure UnlockList;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  public
    function Clone: TCMThreadList;
  end;

  TCMInterfaceList = class;

  { TCMInterfaceListEnumerator }

  TCMInterfaceListEnumerator = class
  protected
    FList: TCMInterfaceList;
    FPosition: Integer;
  public
    constructor Create(AList: TCMInterfaceList);
    function GetCurrent: IUnknown;
    function MoveNext: Boolean;
    property Current: IUnknown read GetCurrent;
  end;

  { TCMInterfaceList
    // 原 TInterfaceList 在迭代和克隆方面不安全，无法正常继承使用。
  }
  TCMInterfaceList = class(TInterfacedObject, IInterfaceList)
  protected
    FList : TCMThreadList;
    function Get(i : Integer) : IUnknown;
    function GetCapacity : Integer;
    function GetCount : Integer;
    procedure Put(i : Integer;item : IUnknown);
    procedure SetCapacity(NewCapacity : Integer);
    procedure SetCount(NewCount : Integer);
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Clear;
    procedure Delete(index : Integer);
    procedure Exchange(index1,index2 : Integer);
    function First : IUnknown;
    function IndexOf(item : IUnknown) : Integer;
    function Add(item : IUnknown) : Integer;
    procedure Insert(i : Integer;item : IUnknown);
    function Last : IUnknown;
    function Remove(item : IUnknown): Integer;
    procedure Lock;
    procedure Unlock;
    property Capacity : Integer read GetCapacity write SetCapacity;
    property Count : Integer read GetCount write SetCount;
    property Items[Index : Integer] : IUnknown read Get write Put;default;
  public
    function GetEnumerator: TCMInterfaceListEnumerator;
    function Expand : TCMInterfaceList;
    function Clone: TCMInterfaceList;
  end;

  TCMInterfaceListClass = class of TCMInterfaceList;

implementation

{ TCMThreadList }

constructor TCMThreadList.Create;
begin
  inherited Create;
  FDuplicates := dupIgnore;
  {$ifdef FPC_HAS_FEATURE_THREADING}
  InitCriticalSection(FLock);
  {$endif}
  FList := TList.Create;
end;

destructor TCMThreadList.Destroy;
begin
  LockList;
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    {$ifdef FPC_HAS_FEATURE_THREADING}
    DoneCriticalSection(FLock);
    {$endif}
  end;
end;

procedure TCMThreadList.Add(Item: Pointer);
begin
  LockList;
  try
    if (Duplicates=dupAccept) or
      // make sure it's not already in the list
      (FList.IndexOf(Item)=-1) then
       FList.Add(Item)
     else if (Duplicates=dupError) then
       //FList.Error('Duplicates not allowed in this list ($0%x)', PtrUInt(Item));
       FList.Error('Duplicates not allowed in this list.', 0);
  finally
    UnlockList;
  end;
end;

procedure TCMThreadList.Clear;
begin
  Locklist;
  try
    FList.Clear;
  finally
    UnLockList;
  end;
end;

function TCMThreadList.LockList: TList;
begin
  Result := FList;
  {$ifdef FPC_HAS_FEATURE_THREADING}
  System.EnterCriticalSection(FLock);
  {$endif}
end;

procedure TCMThreadList.Remove(Item: Pointer);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TCMThreadList.UnlockList;
begin
  {$ifdef FPC_HAS_FEATURE_THREADING}
  System.LeaveCriticalSection(FLock);
  {$endif}
end;

function TCMThreadList.Clone: TCMThreadList;
var
  i: Integer;
begin
  Result := TCMThreadList.Create;
  Self.LockList;
  try
    for i:=0 to FList.Count-1 do
      Result.Add(FList[i]);
  finally
    Self.UnlockList;
  end;
end;

{ TCMInterfaceList }

function TCMInterfaceList.Get(i: Integer): IUnknown;
begin
  FList.Locklist;
  try
    if (i<0) or (i>=FList.FList.Count) then
      FList.FList.Error('List index (%d) out of bounds', i);
    Result  :=  IUnknown(FList.FList.List^[i]);
  finally
    FList.UnlockList;
  end;
end;

function TCMInterfaceList.GetCapacity: Integer;
begin
  FList.Locklist;
  try
    Result := FList.FList.Capacity;
  finally
    FList.UnlockList;
  end;
end;

function TCMInterfaceList.GetCount: Integer;
begin
  begin
    FList.Locklist;
    try
      Result := FList.FList.Count;
    finally
      FList.UnlockList;
    end;
  end;
end;

procedure TCMInterfaceList.Put(i: Integer; item: IUnknown);
begin
  FList.Locklist;
  try
    if (i<0) or (i>=FList.FList.Count) then
      FList.FList.Error('List index (%d) out of bounds',i);
    IUnknown(FList.FList.List^[i]) := item;
  finally
    FList.UnlockList;
  end;
end;

procedure TCMInterfaceList.SetCapacity(NewCapacity: Integer);
begin
  FList.Locklist;
  try
    FList.FList.Capacity := NewCapacity;
  finally
    FList.UnlockList;
  end;
end;

procedure TCMInterfaceList.SetCount(NewCount: Integer);
begin
  FList.Locklist;
  try
    FList.FList.Count := NewCount;
  finally
    FList.UnlockList;
  end;
end;

constructor TCMInterfaceList.Create;
begin
  inherited create;
  FList := TCMThreadList.Create;
end;

destructor TCMInterfaceList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TCMInterfaceList.Clear;
var
  i : SizeInt;
begin
  FList.Locklist;
  try
    for i := 0 to FList.FList.Count-1 do
      IUnknown(FList.FList.List^[i]) := nil;
    FList.Clear;
  finally
    FList.UnlockList;
  end;
end;

procedure TCMInterfaceList.Delete(index: Integer);
begin
  FList.Locklist;
  try
    if (index<0) or (index>=FList.FList.Count) then
      FList.FList.Error('List index (%d) out of bounds',index);
    IUnknown(FList.FList.List^[index]) := nil;
    FList.FList.Delete(index);
  finally
    FList.UnlockList;
  end;
end;

procedure TCMInterfaceList.Exchange(index1, index2: Integer);
begin
  FList.Locklist;
  try
    FList.FList.Exchange(index1,index2);
  finally
    FList.UnlockList;
  end;
end;

function TCMInterfaceList.First: IUnknown;
begin
  Result := Get(0);
end;

function TCMInterfaceList.IndexOf(item: IUnknown): Integer;
begin
  FList.Locklist;
  try
    Result := FList.FList.IndexOf(Pointer(Item));
  finally
    FList.UnlockList;
  end;
end;

function TCMInterfaceList.Add(item: IUnknown): Integer;
begin
  FList.Locklist;
  try
    Result := FList.FList.Add(nil);
    IUnknown(FList.FList.List^[Result]) := item;
  finally
    FList.UnlockList;
  end;
end;

procedure TCMInterfaceList.Insert(i: Integer; item: IUnknown);
begin
  FList.Locklist;
  try
    FList.FList.Insert(i,nil);
    IUnknown(FList.FList.List^[i]) := item;
  finally
    FList.UnlockList;
  end;
end;

function TCMInterfaceList.Last: IUnknown;
begin
  Result := Get(Count-1);
end;

function TCMInterfaceList.Remove(item: IUnknown): Integer;
begin
  FList.Locklist;
  try
    Result := FList.FList.IndexOf(item);
    if Result>=0 then
      begin
        IUnknown(FList.FList.List^[Result]) := nil;
        FList.FList.Delete(Result);
      end;
  finally
    FList.UnlockList;
  end;
end;

procedure TCMInterfaceList.Lock;
begin
  FList.Locklist;
end;

procedure TCMInterfaceList.Unlock;
begin
  FList.UnlockList;
end;

function TCMInterfaceList.GetEnumerator: TCMInterfaceListEnumerator;
begin
  Result := TCMInterfaceListEnumerator.Create(Self)
end;

function TCMInterfaceList.Expand: TCMInterfaceList;
begin
  FList.Locklist;
  try
    FList.FList.Expand;
    Result := Self;
  finally
    FList.UnlockList;
  end;
end;

function TCMInterfaceList.Clone: TCMInterfaceList;
var
  i, p: Integer;
begin
  Result := TCMInterfaceList.Create;
  FList.Locklist;
  try
    for i:=0 to FList.FList.Count-1 do
      begin
        p := Result.FList.FList.Add(nil);
        IUnknown(Result.FList.FList.List^[p]) := IUnknown(FList.FList.List^[i]);
      end;
  finally
    FList.UnlockList;
  end;
end;

{ TCMInterfaceListEnumerator }

constructor TCMInterfaceListEnumerator.Create(AList: TCMInterfaceList);
begin
  inherited create;
  FList := AList;
  FPosition := -1;
end;

function TCMInterfaceListEnumerator.GetCurrent: IUnknown;
begin
  Result := FList[FPosition];
end;

function TCMInterfaceListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition<FList.Count;
end;

end.

