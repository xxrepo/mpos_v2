unit uAForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_AWTBase, cm_AWT,
  cm_theme;

type

  { TAPOSForm }

  TAPOSForm = class(TAForm, IThemeable)
  protected
    FTitlePanel: TAPanel;
  protected
    FTheme: ITheme;
    procedure SetTheme(ATheme: ITheme); virtual;
  public
    constructor Create(AOwner: TAComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  public
    procedure SetTitle(const ATitle: string);
  end;

implementation

{ TAPOSForm }

procedure TAPOSForm.SetTheme(ATheme: ITheme);
begin
  FTheme := ATheme;
  Self.Color := ATheme.GetParameter.Get('boardColor').AsInteger;
  Self.Font.Size := ATheme.GetParameter.Get('defaultFont').Get('size').AsInteger;
  Self.Font.Name := ATheme.GetParameter.Get('defaultFont').Get('name').AsString;
  FTitlePanel.Color := ATheme.GetParameter.Get('service.titleColor').AsInteger;
end;

constructor TAPOSForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FTitlePanel := TAPanel.Create(Self);
  FTitlePanel.Parent := Self;
  FTitlePanel.Align := alTop;
  FTitlePanel.Height := 40;
  FTitlePanel.Font.Size := 20;
end;

destructor TAPOSForm.Destroy;
begin
  TThemeableManager.GetInstance.RemoveThemeable(Self);
  inherited Destroy;
end;

procedure TAPOSForm.AfterConstruction;
begin
  inherited AfterConstruction;
  TThemeableManager.GetInstance.AddThemeable(Self);
end;

procedure TAPOSForm.SetTitle(const ATitle: string);
begin
  FTitlePanel.Caption := ATitle;
end;

end.

