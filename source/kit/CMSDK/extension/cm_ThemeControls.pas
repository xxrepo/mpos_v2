unit cm_ThemeControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls,
  cm_theme;

type

  { TCMThemePanel }

  TCMThemePanel = class(TPanel, IThemeable)
  protected
    FTheme: ITheme;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetImplementorName: string; virtual;
    procedure SetTheme(ATheme: ITheme); virtual;
  end;

implementation

{ TCMThemePanel }

constructor TCMThemePanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  GetThemeableManager.AddThemeable(Self);
  FTheme := nil;
end;

destructor TCMThemePanel.Destroy;
begin
  GetThemeableManager.RemoveThemeable(Self);
  FTheme := nil;
  inherited Destroy;
end;

function TCMThemePanel.GetImplementorName: string;
begin
  Result := Self.UnitName + '.' + Self.ClassName;
end;

procedure TCMThemePanel.SetTheme(ATheme: ITheme);
begin
  FTheme := ATheme;
end;

end.

