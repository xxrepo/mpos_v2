{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_LCLUtils

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_LCLUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Forms,
  cm_messager,
  cm_LCL;

type

  { TCMLCLManager }

  TCMLCLManager = class(TCMMessageable, ICMLCLManager)
  private
    FMainLCLGlobalSet: ILCLGlobalSet;
    FLCLGlobalSetList: TInterfaceList;
    FExceptionEvent: TExceptionEvent;
    procedure SetApplicationExceptionHandler(AHandler: TExceptionEvent);
  public
    constructor Create;
    destructor Destroy; override;
    property ApplicationExceptionEvent: TExceptionEvent read FExceptionEvent write SetApplicationExceptionHandler;
  public
    function SetMainLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
    function GetMainLCLGlobalSet: ILCLGlobalSet;
    function AddLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
    function RemoveLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
  end;

  { TCMLCLGenerator }

  TCMLCLGenerator = class(TCMMessageable, ICMLCLGenerator)
  private
    FClassList: TFPHashList;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterClass(const AClassName: string; APersistentClass: TPersistentClass): Boolean;
  public
    function NewComponent(const AClassName: string; AOwner: TComponent): TComponent;
    function GetComponentClass(const AClassName: string): TComponentClass;
    function NewPersistent(const AClassName: string): TPersistent;
    function GetPersistentClass(const AClassName: string): TPersistentClass;
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

function TCMLCLGenerator.RegisterClass(const AClassName: string; APersistentClass: TPersistentClass): Boolean;
begin
  Result := FClassList.Add(AClassName, APersistentClass) >= 0;
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

function TCMLCLGenerator.NewPersistent(const AClassName: string): TPersistent;
var
  pclass: TPersistentClass;
begin
  Result := nil;
  pclass := TPersistentClass(FClassList.Find(AClassName));
  if Assigned(pclass) then
    Result := TPersistentClass(pclass).Create;
end;

function TCMLCLGenerator.GetPersistentClass(const AClassName: string): TPersistentClass;
var
  pclass: TPersistentClass;
begin
  Result := nil;
  pclass := TPersistentClass(FClassList.Find(AClassName));
  if Assigned(pclass) then
    Result := TPersistentClass(pclass);
end;



{ TCMLCLManager }

constructor TCMLCLManager.Create;
begin
  inherited Create;
  FMainLCLGlobalSet := nil;
  FLCLGlobalSetList := TInterfaceList.Create;
  FExceptionEvent := nil;
end;

destructor TCMLCLManager.Destroy;
begin
  FMainLCLGlobalSet := nil;
  FLCLGlobalSetList.Free;
  inherited Destroy;
end;

procedure TCMLCLManager.SetApplicationExceptionHandler(AHandler: TExceptionEvent);
var
  enumerator: TInterfaceListEnumerator;
  app: TApplication;
begin
  try
    enumerator := FLCLGlobalSetList.GetEnumerator;
    while enumerator.MoveNext do
      begin
        app := ILCLGlobalSet(enumerator.GetCurrent).GetApplication;
        if Assigned(app) then
          begin
            if Assigned(FExceptionEvent) then
              app.RemoveOnExceptionHandler(FExceptionEvent);
            app.AddOnExceptionHandler(AHandler);
          end;
      end;
  finally
    FExceptionEvent := AHandler;
  end;
end;

function TCMLCLManager.SetMainLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
begin
  Result := False;
  Messager.Debug('SetMainLCLGlobalSet()...');
  FMainLCLGlobalSet := ASet;
  Result := Self.AddLCLGlobalSet(FMainLCLGlobalSet);
end;

function TCMLCLManager.GetMainLCLGlobalSet: ILCLGlobalSet;
begin
  Result := nil;
  Messager.Debug('GetMainLCLGlobalSet()...');
  if Assigned(FMainLCLGlobalSet) then
    if FLCLGlobalSetList.IndexOf(FMainLCLGlobalSet) >= 0 then
      Result := FMainLCLGlobalSet;
end;

function TCMLCLManager.AddLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
begin
  Result := False;
  Messager.Debug('AddLCLGlobalSet()...');
  Result := FLCLGlobalSetList.Add(ASet) >= 0;
  //
  if Assigned(FExceptionEvent) then
    if Assigned(ASet.GetApplication) then
      ASet.GetApplication.AddOnExceptionHandler(FExceptionEvent);
  if (ASet <> FMainLCLGlobalSet) and Assigned(FMainLCLGlobalSet) then
    ASet.SetMessageBoxFunction(FMainLCLGlobalSet.GetMessageBoxFunction);
end;

function TCMLCLManager.RemoveLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
begin
  Result := False;
  Messager.Debug('RemoveLCLGlobalSet()...');
  Result := FLCLGlobalSetList.Remove(ASet) >= 0;
end;

end.

