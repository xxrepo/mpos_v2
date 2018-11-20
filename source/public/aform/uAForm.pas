{
    项目抽象的窗体单元

        如果你采用 AWT 的方案实现图形界面功能的话，你应当引用这一单元。
    并且项目中的所有窗体都应源自 TAPOSForm（加载和主窗体可以例外）。

 **********************************************************************}

unit uAForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_theme, cm_AWT, cm_messager;

type

  { TAPOSForm
    // POS 窗体
  }

  TAPOSForm = class(TAForm, IThemeable)
  protected
    FTheme: ITheme;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure SetTheme(ATheme: ITheme); virtual;
  end;

  { TAServiceForm
    // 业务窗体
  }

  TAServiceForm = class(TAPOSForm, IFormListener, ICustomControlListener, IWinControlListener, IControlListener, IKeyListener)
  private
    FOpenDefaultFormEvent: Boolean;
    FOpenDefaultKeyEvent: Boolean;
    procedure SetOpenDefaultFormEvent(AValue: Boolean);
    procedure SetOpenDefaultKeyEvent(AValue: Boolean);
    procedure ControlClick(e: IControlEvent);
    procedure ControlDblClick(e: IControlEvent);
    procedure ControlResize(e: IControlEvent);
    procedure ControlEnter(e: IWinControlEvent);
    procedure ControlExit(e: IWinControlEvent);
    procedure ControlPaint(e: ICustomControlEvent);
    procedure KeyPressed(e: IKeyEvent);
    procedure KeyReleased(e: IKeyEvent);
    procedure KeyTyped(e: IKeyEvent);
  protected
    procedure FormClick(e: IControlEvent); virtual;
    procedure FormDblClick(e: IControlEvent); virtual;
    procedure FormResize(e: IControlEvent); virtual;
    procedure FormEnter(e: IWinControlEvent); virtual;
    procedure FormExit(e: IWinControlEvent); virtual;
    procedure FormPaint(e: ICustomControlEvent); virtual;
    procedure FormActivate(e: IFormEvent); virtual;
    procedure FormClose(e: IFormEvent); virtual;
    procedure FormDeactivate(e: IFormEvent); virtual;
    procedure FormHide(e: IFormEvent); virtual;
    procedure FormShow(e: IFormEvent); virtual;
    procedure FormKeyPressed(e: IKeyEvent); virtual;
    procedure FormKeyReleased(e: IKeyEvent); virtual;
    procedure FormKeyTyped(e: IKeyEvent); virtual;
    property OpenDefaultFormEvent: Boolean read FOpenDefaultFormEvent write SetOpenDefaultFormEvent;
    property OpenDefaultKeyEvent: Boolean read FOpenDefaultKeyEvent write SetOpenDefaultKeyEvent;
  protected
    FHeadPanel: TAPanel;
    FMainPanel: TAPanel;
    FFootPanel: TAPanel;
  public
    constructor Create(AOwner: TAComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
  end;

  { TAMessageableServiceForm }

  TAMessageableServiceForm = class(TAServiceForm, ICMMessageable)
  private
    FMessager: TCMMessager;
  protected
    function Messager: TCMMessager;
  public
    constructor Create(AOwner: TAComponent); override;
  end;

  { TATitledServiceForm
    // 具备标题的业务窗体
  }

  TATitledServiceForm = class(TAMessageableServiceForm)
  protected
    FTitleLab: TALabel;
  public
    constructor Create(AOwner: TAComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
  public
    procedure SetTitle(const ATitle: string);
  end;

  { TAToolableServiceForm
    // 具备工具栏功能的业务窗体
  }

  TAToolableServiceForm = class(TATitledServiceForm)
  public
    constructor Create(AOwner: TAComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
  public
    procedure AddButton(const ACaption: string);
  end;

  { TACustomServiceForm
    // 具备一个自动居中的 Panel
  }

  TACustomServiceForm = class(TAToolableServiceForm)
  protected
    FCenterPanel: TAPanel;
    procedure FormShow(e: IFormEvent); override;
    procedure FormKeyPressed(e: IKeyEvent); override;
  public
    constructor Create(AOwner: TAComponent); override;
  end;

  //Popup

  { TAQueryServiceForm
    // 查询业务窗体
  }

  TAQueryServiceForm = class(TAToolableServiceForm)
  protected
    FQueryPanel: TAPanel;
    FDataPanel: TAPanel;
  public
    constructor Create(AOwner: TAComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
    property QueryPanel: TAPanel read FQueryPanel;
  end;

implementation

{ TAPOSForm }

destructor TAPOSForm.Destroy;
begin
  GetThemeableManager.RemoveThemeable(Self);
  inherited Destroy;
end;

procedure TAPOSForm.AfterConstruction;
begin
  inherited AfterConstruction;
  GetThemeableManager.AddThemeable(Self);
end;

procedure TAPOSForm.SetTheme(ATheme: ITheme);
begin
  FTheme := ATheme;
  Self.Color := ATheme.GetParameter.Get('boardColor').AsInteger;
  Self.Font.Size := ATheme.GetParameter.Get('defaultFont').Get('size').AsInteger;
  Self.Font.Name := ATheme.GetParameter.Get('defaultFont').Get('name').AsString;
end;

{ TAServiceForm }

procedure TAServiceForm.SetOpenDefaultFormEvent(AValue: Boolean);
begin
  if FOpenDefaultFormEvent = AValue then
    Exit;
  FOpenDefaultFormEvent := AValue;
  Self.RemoveFormListener(Self);
  if FOpenDefaultFormEvent then
    Self.AddFormListener(Self);
end;

procedure TAServiceForm.SetOpenDefaultKeyEvent(AValue: Boolean);
begin
  if FOpenDefaultKeyEvent = AValue then
    Exit;
  FOpenDefaultKeyEvent := AValue;
  Self.RemoveKeyListener(Self);
  if FOpenDefaultKeyEvent then
    Self.AddKeyListener(Self);
end;

procedure TAServiceForm.ControlClick(e: IControlEvent);
begin
  FormClick(e);
end;

procedure TAServiceForm.ControlDblClick(e: IControlEvent);
begin
  FormDblClick(e);
end;

procedure TAServiceForm.ControlResize(e: IControlEvent);
begin
  FormResize(e);
end;

procedure TAServiceForm.ControlEnter(e: IWinControlEvent);
begin
  FormEnter(e);
end;

procedure TAServiceForm.ControlExit(e: IWinControlEvent);
begin
  FormExit(e);
end;

procedure TAServiceForm.ControlPaint(e: ICustomControlEvent);
begin
  FormPaint(e);
end;

procedure TAServiceForm.KeyPressed(e: IKeyEvent);
begin
  FormKeyPressed(e);
end;

procedure TAServiceForm.KeyReleased(e: IKeyEvent);
begin
  FormKeyReleased(e);
end;

procedure TAServiceForm.KeyTyped(e: IKeyEvent);
begin
  FormKeyTyped(e);
end;

procedure TAServiceForm.FormClick(e: IControlEvent);
begin
  //There's nothing to do here.
end;

procedure TAServiceForm.FormDblClick(e: IControlEvent);
begin

end;

procedure TAServiceForm.FormResize(e: IControlEvent);
begin

end;

procedure TAServiceForm.FormEnter(e: IWinControlEvent);
begin
  //There's nothing to do here.
end;

procedure TAServiceForm.FormExit(e: IWinControlEvent);
begin

end;

procedure TAServiceForm.FormPaint(e: ICustomControlEvent);
begin
  //There's nothing to do here.
end;

procedure TAServiceForm.FormActivate(e: IFormEvent);
begin
  //There's nothing to do here.
end;

procedure TAServiceForm.FormClose(e: IFormEvent);
begin

end;

procedure TAServiceForm.FormDeactivate(e: IFormEvent);
begin

end;

procedure TAServiceForm.FormHide(e: IFormEvent);
begin

end;

procedure TAServiceForm.FormShow(e: IFormEvent);
begin

end;

procedure TAServiceForm.FormKeyPressed(e: IKeyEvent);
begin
  //There's nothing to do here.
end;

procedure TAServiceForm.FormKeyReleased(e: IKeyEvent);
begin

end;

procedure TAServiceForm.FormKeyTyped(e: IKeyEvent);
begin

end;

constructor TAServiceForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FOpenDefaultFormEvent := False;
  FOpenDefaultKeyEvent := False;
  //
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
  //
  Self.BorderStyle := bsNone;
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

{ TAToolableServiceForm }

constructor TAToolableServiceForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FFootPanel.Visible := True;
end;

procedure TAToolableServiceForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
end;

procedure TAToolableServiceForm.AddButton(const ACaption: string);
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

{ TACustomServiceForm }

procedure TACustomServiceForm.FormShow(e: IFormEvent);
begin
  FCenterPanel.Left := (FMainPanel.Width - FCenterPanel.Width) div 2;
  FCenterPanel.Top := (FMainPanel.Height - FCenterPanel.Height) div 2;
end;

procedure TACustomServiceForm.FormKeyPressed(e: IKeyEvent);
begin
  if e.GetKeyCode = 27 then
    Close;
end;

constructor TACustomServiceForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FCenterPanel := TAPanel.Create(Self);
  FCenterPanel.Parent := FMainPanel;
  FCenterPanel.BevelOuter := bvNone;
  FCenterPanel.Width := 300;
  FCenterPanel.Height := 200;
  //
  OpenDefaultFormEvent := True;
  OpenDefaultKeyEvent := True;
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
  FDataPanel.BevelOuter := bvNone;
end;

procedure TAQueryServiceForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  FDataPanel.Color := ATheme.GetParameter.Get('boardColor').AsInteger;
end;

{ TAMessageableServiceForm }

function TAMessageableServiceForm.Messager: TCMMessager;
begin
  Result := FMessager;
end;

constructor TAMessageableServiceForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FMessager := TCMMessageManager.GetInstance.GetMessager(Self);
end;


end.

