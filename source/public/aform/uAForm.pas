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
    FTheme: ITheme;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure SetTheme(ATheme: ITheme); virtual;
  end;

  { TTitledForm }

  TTitledForm = class(TAPOSForm)
  protected
    FTitlePanel: TAPanel;
    FTitleLab: TALabel;
  public
    constructor Create(AOwner: TAComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
  public
    procedure SetTitle(const ATitle: string);
  end;

  TServiceForm = class(TTitledForm)
  //protected
  //  FWorkPanel: TAPanel;
  //  FToolPanel: TAPanel;
  //public
  //  constructor Create(AOwner: TAComponent); override;
  //  procedure SetTheme(ATheme: ITheme); override;
  end;

implementation

{ TAPOSForm }

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

procedure TAPOSForm.SetTheme(ATheme: ITheme);
begin
  FTheme := ATheme;
  Self.Color := ATheme.GetParameter.Get('boardColor').AsInteger;
  Self.Font.Size := ATheme.GetParameter.Get('defaultFont').Get('size').AsInteger;
  Self.Font.Name := ATheme.GetParameter.Get('defaultFont').Get('name').AsString;
end;

{ TTitledForm }

constructor TTitledForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FTitlePanel := TAPanel.Create(Self);
  FTitlePanel.Parent := Self;
  FTitlePanel.Align := alTop;
  FTitleLab := TALabel.Create(Self);
  FTitleLab.Parent := FTitlePanel;
  FTitleLab.Align := alLeft;
  FTitleLab.BorderSpacing.Left := 100;
  //FTitleLab.Layout := tlCenter;
end;

procedure TTitledForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  FTitlePanel.Color := ATheme.GetParameter.Get('service.titleColor').AsInteger;
  FTitleLab.Font.Size := ATheme.GetParameter.Get('service.titleFont').Get('size').AsInteger;
  FTitleLab.Font.Color := ATheme.GetParameter.Get('service.titleFont').Get('color').AsInteger;
  FTitleLab.Font.Name := ATheme.GetParameter.Get('service.titleFont').Get('name').AsString;
end;

procedure TTitledForm.SetTitle(const ATitle: string);
begin
  FTitleLab.Caption := ATitle;
end;

end.

