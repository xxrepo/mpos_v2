{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_theme

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_theme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces,
  cm_parameter;

type

  ITheme = interface(ICMBase)
    ['{CC4CB4B4-CD3E-4DD1-8265-FBB4F16955EA}']
    function GetName: string;
    function GetTitle: string;
    function GetParameter: ICMParameter;
  end;

  IThemeable = interface(ICMBase)
    ['{65EA0F68-10F4-4B19-9CB2-D4126E4F49CF}']
    procedure SetTheme(ATheme: ITheme);
  end;

  IThemeableSet = interface(ICMBase)
    ['{FA2F9F2A-CA01-48F9-9CC7-457ED58B7D49}']
    function Add(AThemeable: IThemeable): Boolean;
    function Remove(AThemeable: IThemeable): Boolean;
  end;

  { IThemeController }

  IThemeController = interface(ICMBase)
    ['{DD88310A-A5BA-4C49-8418-CC8879E0114A}']
    function AddTheme(ATheme: ITheme): Boolean;
    function GetThemeCount: Integer;
    function GetTheme(AIndex: Integer): ITheme;
    function GetCurrTheme: ITheme;
    function SwitchTheme(const AThemeName: string): Boolean;
    function GetThemeNames: TStrings;
  end;

  { TThemeableManager }

  TThemeableManager = class
  private
    class var FTManager: TThemeableManager;
  private
    FSetList: TInterfaceList;
    constructor Create; //Please do not use this constructor
  public
    class function GetInstance: TThemeableManager;
    function AddThemeableSet(ASet: IThemeableSet): Boolean;
    function AddThemeable(AThemeable: IThemeable): Boolean;
    function RemoveThemeable(AThemeable: IThemeable): Boolean;
  end;

implementation

{ TThemeableManager }

constructor TThemeableManager.Create;
begin
  FSetList := TInterfaceList.Create;
end;

class function TThemeableManager.GetInstance: TThemeableManager;
begin
  Result := nil;
  if not Assigned(TThemeableManager.FTManager) then
    TThemeableManager.FTManager := TThemeableManager.Create;
  Result := TThemeableManager.FTManager;
end;

function TThemeableManager.AddThemeableSet(ASet: IThemeableSet): Boolean;
begin
  Result := False;
  if Assigned(ASet) then
    Result := FSetList.Add(ASet) >= 0;
end;

function TThemeableManager.AddThemeable(AThemeable: IThemeable): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=0 to FSetList.Count-1 do
    begin
      Result := IThemeableSet(FSetList[i]).Add(AThemeable) and Result;
    end;
end;

function TThemeableManager.RemoveThemeable(AThemeable: IThemeable): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=0 to FSetList.Count-1 do
    begin
      Result := IThemeableSet(FSetList[i]).Remove(AThemeable) and Result;
    end;
end;

initialization
  TThemeableManager.FTManager := nil;

finalization
  if Assigned(TThemeableManager.FTManager) then
    TThemeableManager.FTManager.Free;

end.

