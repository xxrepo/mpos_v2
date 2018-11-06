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
  Classes, SysUtils, Forms,
  cm_interfaces, cm_messager,
  cm_LCL;

type

  { TCMLCLGlobalManager }

  TCMLCLGlobalManager = class(TCMMessageable, ICMLCLGlobalManager)
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

implementation

{ TCMLCLGlobalManager }

constructor TCMLCLGlobalManager.Create;
begin
  inherited Create;
  FMainLCLGlobalSet := nil;
  FLCLGlobalSetList := TInterfaceList.Create;
  FExceptionEvent := nil;
end;

destructor TCMLCLGlobalManager.Destroy;
begin
  FMainLCLGlobalSet := nil;
  FLCLGlobalSetList.Free;
  inherited Destroy;
end;

procedure TCMLCLGlobalManager.SetApplicationExceptionHandler(AHandler: TExceptionEvent);
var
  enumerator: TInterfaceListEnumerator;
  app: TApplication;
begin
  try
    enumerator := FLCLGlobalSetList.GetEnumerator;
    try
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
      enumerator.Free;
    end;
  finally
    FExceptionEvent := AHandler;
  end;
end;

function TCMLCLGlobalManager.SetMainLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
begin
  Result := False;
  Messager.Debug('SetMainLCLGlobalSet()...');
  FMainLCLGlobalSet := ASet;
  Result := Self.AddLCLGlobalSet(FMainLCLGlobalSet);
end;

function TCMLCLGlobalManager.GetMainLCLGlobalSet: ILCLGlobalSet;
begin
  Result := nil;
  Messager.Debug('GetMainLCLGlobalSet()...');
  if Assigned(FMainLCLGlobalSet) then
    if FLCLGlobalSetList.IndexOf(FMainLCLGlobalSet) >= 0 then
      Result := FMainLCLGlobalSet;
end;

function TCMLCLGlobalManager.AddLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
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

function TCMLCLGlobalManager.RemoveLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
begin
  Result := False;
  Messager.Debug('RemoveLCLGlobalSet()...');
  Result := FLCLGlobalSetList.Remove(ASet) >= 0;
end;




end.

