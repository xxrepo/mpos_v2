unit uAForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_theme, cm_AWT;

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

  { TAServiceForm }

  TAServiceForm = class(TAPOSForm)
  protected
    FHeadPanel: TAPanel;
    FMainPanel: TAPanel;
    FFootPanel: TAPanel;
  public
    constructor Create(AOwner: TAComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
  end;

  { TATitledServiceForm }

  TATitledServiceForm = class(TAServiceForm)
  protected
    FTitleLab: TALabel;
  public
    constructor Create(AOwner: TAComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
  public
    procedure SetTitle(const ATitle: string);
  end;

  { TAControllableServiceForm }

  TAControllableServiceForm = class(TATitledServiceForm)
  public
    constructor Create(AOwner: TAComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
  public
    procedure AddButton(const ACaption: string);
  end;

  //Popup

  { TAQueryServiceForm }

  TAQueryServiceForm = class(TAControllableServiceForm)
  protected
    FQueryPanel: TAPanel;
    FDataPanel: TAPanel;
  public
    constructor Create(AOwner: TAComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
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

{ TAServiceForm }

constructor TAServiceForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FHeadPanel := TAPanel.Create(Self);
  FHeadPanel.Parent := Self;
  FHeadPanel.Align := alTop;
  FHeadPanel.BevelOuter := bvNone;
  FHeadPanel.Visible := False;
  FMainPanel := TAPanel.Create(Self);
  FMainPanel.Parent := Self;
  FMainPanel.Align := alClient;
  FMainPanel.BevelOuter := bvNone;
  FFootPanel := TAPanel.Create(Self);
  FFootPanel.Parent := Self;
  FFootPanel.Align := alBottom;
  FFootPanel.BevelOuter := bvNone;
  FFootPanel.Visible := False;
end;

procedure TAServiceForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  FHeadPanel.Color := ATheme.GetParameter.Get('service.head.color').AsInteger;
  FHeadPanel.Height := ATheme.GetParameter.Get('service.head.height').AsInteger;
  FMainPanel.Color := ATheme.GetParameter.Get('panelColor').AsInteger;
  FFootPanel.Color := ATheme.GetParameter.Get('service.foot.color').AsInteger;
  FFootPanel.Height := ATheme.GetParameter.Get('service.foot.height').AsInteger;
end;

{ TATitledServiceForm }

constructor TATitledServiceForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FHeadPanel.Visible := True;
  FTitleLab := TALabel.Create(Self);
  FTitleLab.Parent := FHeadPanel;
  FTitleLab.Align := alLeft;
  FTitleLab.BorderSpacing.Left := 10;
  FTitleLab.Layout := tlCenter;
end;

procedure TATitledServiceForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  FTitleLab.Font.Size := ATheme.GetParameter.Get('service.head.titleFont.size').AsInteger;
  FTitleLab.Font.Color := ATheme.GetParameter.Get('service.head.titleFont.color').AsInteger;
  FTitleLab.Font.Name := ATheme.GetParameter.Get('service.head.titleFont.name').AsString;
end;

procedure TATitledServiceForm.SetTitle(const ATitle: string);
begin
  FTitleLab.Caption := ATitle;
end;

{ TAControllableServiceForm }

constructor TAControllableServiceForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FFootPanel.Visible := True;
end;

procedure TAControllableServiceForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
end;

procedure TAControllableServiceForm.AddButton(const ACaption: string);
var
  p: TAPanel;
begin
  p := TAPanel.Create(FFootPanel);
  p.Parent := FFootPanel;
  p.Left := 10;
  p.Top := 4;
  p.Height := FFootPanel.Height - 8;
  p.Width := 100;
  p.Caption := ACaption
end;

{ TAQueryServiceForm }

constructor TAQueryServiceForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FQueryPanel := TAPanel.Create(FMainPanel);
  FQueryPanel.Parent := FMainPanel;
  FQueryPanel.Align := alTop;
  FQueryPanel.BevelOuter := bvNone;
  FDataPanel := TAPanel.Create(FMainPanel);
  FDataPanel.Parent := FMainPanel;
  FDataPanel.Align := alClient;
  FDataPanel.Color := clYellow;
  FDataPanel.BevelOuter := bvNone;
end;

procedure TAQueryServiceForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
end;


end.

