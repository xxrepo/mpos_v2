unit uForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  cm_theme, cm_messager;

type

  { TPOSForm }

  TPOSForm = class(TForm, IThemeable, ICMMessageable)
  private
    FTheme: ITheme;
    FMessager: TCMMessager;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function GetImplementorName: string; virtual;
    procedure SetTheme(ATheme: ITheme); virtual;
    function Messager: TCMMessager;
  end;


implementation

{$R *.frm}

{ TPOSForm }

constructor TPOSForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMessager := TCMMessageManager.GetInstance.GetMessager(Self);
end;

destructor TPOSForm.Destroy;
begin
  TThemeableManager.GetInstance.RemoveThemeable(Self);
  inherited Destroy;
end;

procedure TPOSForm.AfterConstruction;
begin
  inherited AfterConstruction;
  TThemeableManager.GetInstance.AddThemeable(Self);
end;

function TPOSForm.GetImplementorName: string;
begin
  Result := Self.UnitName + '.' + Self.ClassName;
end;

procedure TPOSForm.SetTheme(ATheme: ITheme);
begin
  FTheme := ATheme;
  Self.Color := ATheme.GetParameter.Get('boardColor').AsInteger;
  Self.Font.Size := ATheme.GetParameter.Get('defaultFont').Get('size').AsInteger;
  Self.Font.Name := ATheme.GetParameter.Get('defaultFont').Get('name').AsString;
end;

function TPOSForm.Messager: TCMMessager;
begin
  Result := FMessager;
end;

end.

