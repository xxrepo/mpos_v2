unit uFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLType, StdCtrls, ComCtrls,
  cm_Plat,
  uForm, cm_theme, uConmponents, uSystem,
  uTest, uFormService, uFormPopupService,
  uStart, uFormSetting,
  uSetting,
  uDAO,
  uFrameSale, uSale, uSaleDTO,
  uFrameNavigator;

type

  { TMainForm }

  TMainForm = class(TPOSForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    PanelWork: TPanel;
    PanelRightHint: TPanel;
    PanelRight: TPanel;
    PanelClient: TPanel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure Panel4Click(Sender: TObject);
    procedure Panel5Click(Sender: TObject);
    procedure Panel6Click(Sender: TObject);
    procedure Panel7Click(Sender: TObject);
    procedure PanelRightHintDblClick(Sender: TObject);
  private
    FPOSTitlePanel: TPOSTitlePanel;
    FHasSet: Boolean;
    FSaleFrame: TSaleFrame;
    FNavigatorFrame: TNavigatorFrame;
  public
    procedure SetTheme(ATheme: ITheme); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.frm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPOSTitlePanel := TPOSTitlePanel.Create(Self);
  FPOSTitlePanel.Parent := PanelTop;
  FPOSTitlePanel.Align := alTop;
  PanelTop.Height := FPOSTitlePanel.Height;
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
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    Close;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  workRect: TRect;
begin
  Self.Left := 0;
  Self.Top := 0;
  if Assigned(AppSystem) then
    begin
      if not FHasSet then
        begin
          FPOSTitlePanel.SetTitle('');
          FPOSTitlePanel.SetVersion('v' + AppSystem.GetVersion);
          FPOSTitlePanel.SetImage(AppSystem.GetParameter.Get('mpos.resources.logo').AsString);
          FHasSet := True;
        end;
      //
      if AppSystem.IsTestMode then
        begin
          Self.Width := 1024;
          Self.Height := 768;
        end;
      workRect := PanelWork.BoundsRect;

      workRect.Width := workRect.Width - 12;

      POSStart.SetWorkRect(workRect);
    end;
  POSStart.AfterLogin;


  FNavigatorFrame.LoadConfig;

  //test
  PanelRightHint.Width := 12;
end;

procedure TMainForm.Panel1Click(Sender: TObject);
var
  ic: IThemeController;
begin
  InterfaceRegister.OutInterface(IThemeController, ic);
  ic.SwitchTheme('sweet');
end;

procedure TMainForm.Panel2Click(Sender: TObject);
var
  it: ITest;
begin
  if InterfaceRegister.OutInterface(ITest, it) then
    begin
      try
        //it.Test;
        //it.Test2;
        it.Test3;
      except
        on e: Exception do
          begin
            ShowMessage('Panel2Click ex:' + e.ClassName);
            //Application.ShowException(e);
          end;
      end;
    end;
end;

procedure TMainForm.Panel3Click(Sender: TObject);
var
  f: TServiceForm;
begin
  f := TSettingForm.Create(Self);
  f.BorderStyle := bsNone;
  f.BoundsRect := AppSystem.GetWorkRect;



  f.ShowModal;
end;

procedure TMainForm.Panel4Click(Sender: TObject);
var
  f: TPopupServiceForm;
begin
  f := TPopupServiceForm.Create(Self);
  f.BorderStyle := bsNone;
  f.Position := poScreenCenter;
  f.ShowModal;
end;

procedure TMainForm.Panel5Click(Sender: TObject);
var
  s: TPOSSetting;
begin
  s := TPOSSetting.Create(Self);
  s.MenuForm.ShowModal;
end;

procedure TMainForm.Panel6Click(Sender: TObject);
var
  vo: TShowItem;
  vos: TShowItemList;
  i: Integer;
begin
  vos := TShowItemList.Create(False);
  for i:=0 to 800 do
    begin
      vo := TShowItem.Create;
      vo.Name := '椰树椰子汁 1000ml/罐';
      vo.BarCode := '632014412014';
      vo.Price := 6.98;
      vo.Quantity := i;
      vos.Add(vo.UUID, vo);
    end;
  FSaleFrame.SetShowItemList(vos);
  vos.Free;
end;

procedure TMainForm.Panel7Click(Sender: TObject);
var
  vo: TShowItem;
begin
  vo := TShowItem.Create;
  vo.Name := '椰树椰子汁 1000ml/罐';
  vo.BarCode := '632014412014';
  vo.Price := 6.98;
  vo.Quantity := Random(88);
  FSaleFrame.AddShowItem(vo);
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
  //
  Panel1.Color := ATheme.GetParameter.Get('color1').AsInteger;
  Panel2.Color := ATheme.GetParameter.Get('color2').AsInteger;
  Panel3.Color := ATheme.GetParameter.Get('color3').AsInteger;
  Panel4.Color := ATheme.GetParameter.Get('color4').AsInteger;
end;

end.

