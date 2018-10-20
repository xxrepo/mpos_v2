unit uFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  cm_theme;

type

  { TPOSFrame }

  TPOSFrame = class(TFrame, IThemeable)
  private
    FTheme: ITheme;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function GetImplementorName: string; virtual;
    procedure SetTheme(ATheme: ITheme); virtual;
  end;

implementation

{$R *.frm}

{ TPOSFrame }

destructor TPOSFrame.Destroy;
begin
  TThemeableManager.GetInstance.RemoveThemeable(Self);
  inherited Destroy;
end;

procedure TPOSFrame.AfterConstruction;
begin
  inherited AfterConstruction;
  TThemeableManager.GetInstance.AddThemeable(Self);
end;

function TPOSFrame.GetImplementorName: string;
begin
  Result := Self.UnitName + '.' + Self.ClassName;
end;

procedure TPOSFrame.SetTheme(ATheme: ITheme);
begin
  FTheme := ATheme;
  Self.Color := ATheme.GetParameter.Get('boardColor').AsInteger;
  Self.Font.Size := ATheme.GetParameter.Get('defaultFont').Get('size').AsInteger;
  Self.Font.Name := ATheme.GetParameter.Get('defaultFont').Get('name').AsString;
end;

end.

