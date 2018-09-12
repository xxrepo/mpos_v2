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
    function GetImplementorName: string; virtual;
    procedure SetTheme(ATheme: ITheme); virtual;
  end;

implementation

{$R *.frm}

{ TPOSFrame }

function TPOSFrame.GetImplementorName: string;
begin
  Result := Self.UnitName + '.' + Self.ClassName;
end;

procedure TPOSFrame.SetTheme(ATheme: ITheme);
begin
  FTheme := ATheme;
  Self.Color := ATheme.Parameter.Get('boardColor').AsInteger;
  Self.Font.Size := ATheme.Parameter.Get('defaultFont').Get('size').AsInteger;
  Self.Font.Name := ATheme.Parameter.Get('defaultFont').Get('name').AsString;
end;

end.

