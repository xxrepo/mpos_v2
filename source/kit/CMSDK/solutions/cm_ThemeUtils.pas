unit cm_ThemeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_parameter,
  cm_theme,
  cm_collections;

type

  { TCMTheme }

  TCMTheme = class(TCMBase, ITheme)
  private
    FName: string;
    FTitle: string;
    FParameter: ICMParameter;
  public
    constructor Create(const AName, ATitle: string; AParameter: ICMParameter);
    function GetName: string;
    function GetTitle: string;
    function Parameter: ICMParameter;
  end;

  { TCMThemeUtil }

  TCMThemeUtil = class(TCMBase, IThemeableSet, IThemeController)
  private
    FAbleList: TInterfaceList;
    FThemeList: TCMHashInterfaceList;
    FReNames: TStrings;
    FFirstThemeName: string;
    FCurrTheme: ITheme;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetFirstTheme;
  public //IThemeableSet
    function Add(AThemeable: IThemeable): Boolean;
    function Remove(AThemeable: IThemeable): Boolean;
  public //IThemeController
    function AddTheme(ATheme: ITheme): Boolean;
    function GetThemeCount: Integer;
    function GetTheme(AIndex: Integer): ITheme;
    function GetCurrTheme: ITheme;
    function SwitchTheme(const AThemeName: string): Boolean;
    function GetThemeNames: TStrings;
  end;

implementation

{ TCMTheme }

constructor TCMTheme.Create(const AName, ATitle: string; AParameter: ICMParameter);
begin
  FName := AName;
  FTitle := ATitle;
  FParameter := AParameter;
end;

function TCMTheme.GetName: string;
begin
  Result := FName;
end;

function TCMTheme.GetTitle: string;
begin
  Result := FTitle;
end;

function TCMTheme.Parameter: ICMParameter;
begin
  Result := FParameter;
end;

{ TCMThemeUtil }

constructor TCMThemeUtil.Create;
begin
  FAbleList := TInterfaceList.Create;
  FThemeList := TCMHashInterfaceList.Create;
  FReNames := nil;
  FFirstThemeName := '';
  FCurrTheme := nil;
end;

destructor TCMThemeUtil.Destroy;
begin
  FThemeList.Free;
  FAbleList.Free;
  if Assigned(FReNames) then
    FReNames.Free;
  FCurrTheme := nil;
  inherited Destroy;
end;

procedure TCMThemeUtil.SetFirstTheme;
begin
  if FFirstThemeName <> '' then
    SwitchTheme(FFirstThemeName);
end;

function TCMThemeUtil.Add(AThemeable: IThemeable): Boolean;
begin
  Result := False;
  if Assigned(AThemeable) then
    begin
      Result := FAbleList.Add(AThemeable) >= 0;
      if Result and Assigned(FCurrTheme) then
        AThemeable.SetTheme(FCurrTheme);
    end;
end;

function TCMThemeUtil.Remove(AThemeable: IThemeable): Boolean;
begin
  Result := FAbleList.Remove(AThemeable) >= 0;
  if FAbleList.Count = 0 then
    FFirstThemeName := '';
end;

function TCMThemeUtil.AddTheme(ATheme: ITheme): Boolean;
begin
  Result := False;
  if Assigned(ATheme) then
    begin
      Result := FThemeList.Add(ATheme.GetName, ATheme) >= 0;
      if Result then
        if FFirstThemeName = '' then
          FFirstThemeName := ATheme.GetName;
    end;
end;

function TCMThemeUtil.GetThemeCount: Integer;
begin
  Result := FThemeList.Count;
end;

function TCMThemeUtil.GetTheme(AIndex: Integer): ITheme;
begin
  Result := nil;
  if AIndex < FThemeList.Count then
    Result := ITheme(FThemeList[AIndex]);
end;

function TCMThemeUtil.GetCurrTheme: ITheme;
begin
  Result := FCurrTheme;
end;

function TCMThemeUtil.SwitchTheme(const AThemeName: string): Boolean;
var
  theme: ITheme;
  ile: TInterfaceListEnumerator;
begin
  Result := False;
  theme := ITheme(FThemeList.Find(AThemeName));
  if Assigned(theme) then
    begin
      FCurrTheme := theme;
      ile := FAbleList.GetEnumerator;
      try
        while ile.MoveNext do
          IThemeable(ile.Current).SetTheme(theme);
      finally
        ile.Free;
      end;
      Result := True;
    end;
end;

function TCMThemeUtil.GetThemeNames: TStrings;
var
  i: Integer;
begin
  Result := nil;
  if not Assigned(FReNames) then
    FReNames := TStringList.Create;
  Result := FReNames;
  FReNames.Clear;
  for i:=0 to FThemeList.Count-1 do
    FReNames.Add(ITheme(FThemeList[i]).GetName);
end;


end.

