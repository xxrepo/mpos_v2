unit uDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_dialogs, cm_theme;

type

  { TPOSMsgBox }

  TPOSMsgBox = class(TCMMsgBox, IThemeable)
  private
    FTheme: ITheme;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetImplementorName: string; virtual;
    procedure SetTheme(ATheme: ITheme); virtual;
  end;

implementation

{ TPOSMsgBox }

constructor TPOSMsgBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TThemeableManager.GetInstance.AddThemeable(Self);
end;

destructor TPOSMsgBox.Destroy;
begin
  inherited Destroy;
  TThemeableManager.GetInstance.RemoveThemeable(Self);
end;

function TPOSMsgBox.GetImplementorName: string;
begin
  Result := Self.UnitName + '.' + Self.ClassName;
end;

procedure TPOSMsgBox.SetTheme(ATheme: ITheme);
begin
  FTheme := ATheme;
  if not ATheme.Parameter.Get('popup').IsNull then
    begin
      Self.MsgBoard.TitleColor := ATheme.Parameter.Get('popup.titleColor').AsInteger;
      Self.MsgBoard.TitleFont.Color := ATheme.Parameter.Get('popup.titleFont.color').AsInteger;
      Self.MsgBoard.TitleFont.Size := ATheme.Parameter.Get('popup.titleFont.size').AsInteger;
      Self.MsgBoard.TitleFont.Name := ATheme.Parameter.Get('popup.titleFont.name').AsString;
      Self.MsgBoard.Color := ATheme.Parameter.Get('popup.backgroundColor').AsInteger;
      Self.MsgBoard.DefaultMsgFont.Color := ATheme.Parameter.Get('popup.defaultFont.color').AsInteger;
      Self.MsgBoard.DefaultMsgFont.Size := ATheme.Parameter.Get('popup.defaultFont').Get('size').AsInteger;
      Self.MsgBoard.DefaultMsgFont.Name := ATheme.Parameter.Get('popup.defaultFont').Get('name').AsString;
      Self.MsgBoard.SelectedButtonFontColor := ATheme.Parameter.Get('popup.selectedButtonFontColor').AsInteger;
    end;
end;


end.

