unit uFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLType, StdCtrls, ComCtrls,
  cm_theme, cm_Plat, cm_dialogs,
  uForm, uConmponents,
  uSale, uFrameSale,
  uFrameNavigator, uFrameTest,
  uDAO, uSystem, uInitialize;

type

  { TMainForm }

  TMainForm = class(TPOSForm)
    PanelWork: TPanel;
    PanelTest: TPanel;
    PanelService: TPanel;
    PanelRightHint: TPanel;
    PanelRight: TPanel;
    PanelClient: TPanel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PanelRightHintDblClick(Sender: TObject);
  private
    FPOSTitlePanel: TPOSTitlePanel;
    FMsgBar: TCMMsgBar;
    FHasSet: Boolean;
    FSaleFrame: TSaleFrame;
    FNavigatorFrame: TNavigatorFrame;
    FTestFrame: TTestFrame;
  public
    procedure SetTheme(ATheme: ITheme); override;
  end;

var
  MainForm: TMainForm;

implementation

uses uConstant;

{$R *.frm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPOSTitlePanel := TPOSTitlePanel.Create(Self);
  FPOSTitlePanel.Parent := PanelTop;
  FPOSTitlePanel.Align := alTop;
  PanelTop.Height := FPOSTitlePanel.Height;
  FMsgBar := TCMMsgBar.Create(PanelBottom);
  FMsgBar.Align := alTop;
  FMsgBar.InherentHeight := 14;
  FMsgBar.Font.Size := 10;
  POSInitialize.MsgBar := FMsgBar;
  FHasSet := False;
  //
  FSaleFrame := TSaleFrame.Create(Self);
  FSaleFrame.Parent := PanelClient;
  FSaleFrame.Align := alClient;
  InterfaceRegister.PutInterface('ISaleBoard', ISaleBoard, FSaleFrame);
  //
  FNavigatorFrame := TNavigatorFrame.Create(Self);
  FNavigatorFrame.Parent := PanelRight;
  FNavigatorFrame.Align := alClient;
  //
  FTestFrame := TTestFrame.Create(Self);
  FTestFrame.Parent := PanelTest;
  FTestFrame.Align := alClient;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  POSInitialize.NotifySystem('Closing');
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FMsgBar.Visible := False;
  if Key = 27 then
    Close;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  //FMsgBar.Top := pa;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Left := 0;
  Self.Top := 0;
  if Assigned(AppSystem) then
    begin
      if not FHasSet then
        begin
          FPOSTitlePanel.SetTitle('');
          FPOSTitlePanel.SetVersion('v' + AppSystem.GetVersion);
          FPOSTitlePanel.SetImage(AppSystem.GetParameter.Get(LogoParameterName).AsString);
          FHasSet := True;
        end;
      //
      if AppSystem.IsTestMode then
        begin
          PanelTest.Visible := True;
          PanelTest.Width := 240;
          Self.Width := 1024 + PanelTest.Width;
          Self.Height := 768;
        end;
      //
      POSInitialize.WorkRectControl := PanelWork;
      POSInitialize.ServiceRectControl := PanelService;
    end;
  //TODO 登陆
  POSInitialize.NotifySystem('Logined');
  FNavigatorFrame.LoadConfig;
  PanelRightHint.Width := 12;
end;

procedure TMainForm.PanelRightHintDblClick(Sender: TObject);
begin
  if TPanel(Sender).Width < 200 then
    TPanel(Sender).Width := 200
  else
    TPanel(Sender).Width := 12;
end;

procedure TMainForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  PanelClient.Color := ATheme.GetParameter.Get('panelColor').AsInteger;
  PanelRight.Color := PanelClient.Color;
  PanelBottom.Color := ATheme.GetParameter.Get('footer.color').AsInteger;
  if not ATheme.GetParameter.Get('mainForm').IsNull then
    PanelRight.Width := ATheme.GetParameter.Get('mainForm.rightWidth').AsInteger;
end;

end.

