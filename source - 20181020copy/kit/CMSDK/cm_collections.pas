{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_collections

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_collections;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  { TCMHashStringList }

  TCMHashStringList = class
  private type TStringObj = class
    private
      FString: string;
    public
      constructor Create(const AString: string);
    end;
  private
    FList: TFPHashObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): string;
    procedure SetCount(AValue: Integer);
    procedure SetItem(Index: Integer; const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const AName: ShortString; const AValue: string): Integer;
    function NameOfIndex(AIndex: Integer): ShortString;
    procedure Delete(AIndex: Integer);
    function Remove(const AItem: string): Integer;
    function IndexOf(const AItem: string): Integer;
    function Find(const AName: ShortString): string;
    function FindIndexOf(const AName: ShortString): Integer;
    function Rename(const AOldName, ANewName: ShortString): Integer;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: string read GetItem write SetItem; default;
  end;

  { TCMHashInterfaceList }

  TCMHashInterfaceList = class
  private type TInterfaceObj = class
    private
      FInterface: IUnknown;
    public
      constructor Create(AInterface: IUnknown);
      destructor Destroy; override;
    end;
  private
    FList: TFPHashObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): IUnknown;
    procedure SetCount(AValue: Integer);
    procedure SetItem(Index: Integer; AValue: IUnknown);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const AName: ShortString; AItem: IUnknown): Integer;
    function NameOfIndex(AIndex: Integer): ShortString;
    procedure Delete(AIndex: Integer);
    function Remove(AItem: IUnknown): Integer;
    function IndexOf(AItem: IUnknown): Integer;
    function Find(const AName: ShortString): IUnknown;
    function FindIndexOf(const AName: ShortString): Integer;
    function Rename(const AOldName, ANewName: ShortString): Integer;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: IUnknown read GetItem write SetItem; default;
  end;




implementation

{ TCMHashStringList.TStringObj }

constructor TCMHashStringList.TStringObj.Create(const AString: string);
begin
  FString := AString;
end;

{ TCMHashStringList }

function TCMHashStringList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCMHashStringList.GetItem(Index: Integer): string;
begin
  Result := TStringObj(FList[Index]).FString;
end;

procedure TCMHashStringList.SetCount(AValue: Integer);
begin
  FList.Count := AValue;
end;

procedure TCMHashStringList.SetItem(Index: Integer; const AValue: string);
begin
  TStringObj(FList[Index]).FString := AValue;
end;

constructor TCMHashStringList.Create;
begin
  FList := TFPHashObjectList.Create(True);
end;

destructor TCMHashStringList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TCMHashStringList.Clear;
begin
  FList.Clear;
end;

function TCMHashStringList.Add(const AName: ShortString; const AValue: string): Integer;
begin
  Result := FList.Add(AName, TStringObj.Create(AValue));
end;

function TCMHashStringList.NameOfIndex(AIndex: Integer): ShortString;
begin
  Result := FList.NameOfIndex(AIndex);
end;

procedure TCMHashStringList.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

function TCMHashStringList.Remove(const AItem: string): Integer;
begin
  Result := IndexOf(AItem);
  If Result <> -1 then
    Self.Delete(Result);
end;

function TCMHashStringList.IndexOf(const AItem: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i:=0 to FList.Count-1 do
    begin
      if TStringObj(FList[i]).FString = AItem then
        begin
          Result := i;
          Exit;
        end;
    end;
end;

function TCMHashStringList.Find(const AName: ShortString): string;
var
  sobj: TStringObj;
begin
  Result := '';
  sobj := TStringObj(FList.Find(AName));
  if Assigned(sobj) then
    Result := sobj.FString;
end;

function TCMHashStringList.FindIndexOf(const AName: ShortString): Integer;
begin
  Result := FList.FindIndexOf(AName);
end;

function TCMHashStringList.Rename(const AOldName, ANewName: ShortString): Integer;
begin
  Result := FList.Rename(AOldName, ANewName);
end;

{ TCMHashInterfaceList.TInterfaceObj }

constructor TCMHashInterfaceList.TInterfaceObj.Create(AInterface: IUnknown);
begin
  FInterface := AInterface;
end;

destructor TCMHashInterfaceList.TInterfaceObj.Destroy;
begin
  FInterface := nil;
  inherited Destroy;
end;

{ TCMHashInterfaceList }

function TCMHashInterfaceList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCMHashInterfaceList.GetItem(Index: Integer): IUnknown;
begin
  Result := TInterfaceObj(FList[Index]).FInterface;
end;

procedure TCMHashInterfaceList.SetCount(AValue: Integer);
begin
  FList.Count := AValue;
end;

procedure TCMHashInterfaceList.SetItem(Index: Integer; AValue: IUnknown);
begin
  TInterfaceObj(FList[Index]).FInterface := AValue;
end;

constructor TCMHashInterfaceList.Create;
begin
  FList := TFPHashObjectList.Create(True);
end;

destructor TCMHashInterfaceList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TCMHashInterfaceList.Clear;
begin
  FList.Clear;
end;

function TCMHashInterfaceList.Add(const AName: ShortString; AItem: IUnknown): Integer;
begin
  Result := -1;
  if Assigned(AItem) then
    Result := FList.Add(AName, TInterfaceObj.Create(AItem));
end;

function TCMHashInterfaceList.NameOfIndex(AIndex: Integer): ShortString;
begin
  Result := FList.NameOfIndex(AIndex);
end;

procedure TCMHashInterfaceList.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

function TCMHashInterfaceList.Remove(AItem: IUnknown): Integer;
begin
  Result := IndexOf(AItem);
  If Result <> -1 then
    Self.Delete(Result);
end;

function TCMHashInterfaceList.IndexOf(AItem: IUnknown): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i:=0 to FList.Count-1 do
    begin
      if TInterfaceObj(FList[i]).FInterface = AItem then
        begin
          Result := i;
          Exit;
        end;
    end;
end;

function TCMHashInterfaceList.Find(const AName: ShortString): IUnknown;
var
  iobj: TInterfaceObj;
begin
  Result := nil;
  iobj := TInterfaceObj(FList.Find(AName));
  if Assigned(iobj) then
    Result := iobj.FInterface;
end;

function TCMHashInterfaceList.FindIndexOf(const AName: ShortString): Integer;
begin
  Result := FList.FindIndexOf(AName);
end;

function TCMHashInterfaceList.Rename(const AOldName, ANewName: ShortString): Integer;
begin
  Result := FList.Rename(AOldName, ANewName);
end;




end.

