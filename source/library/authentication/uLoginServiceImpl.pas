unit uLoginServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPJson, LCLType, InterfaceBase, LCLIntf, StdCtrls, ComCtrls, ExtCtrls, Controls, LCLStrConsts, Forms,
  cm_messager, cm_LCL,
  uFormService, uForm, uApp, uLoginService;

type

  { TLoginService }

  TLoginService = class(TCMMessageable, ILoginService)
  private
    FInitialized: boolean;
    FForm: TServiceForm;
    FUserCodeEdit: TEdit;
    FPasswordEdit: TEdit;
    FUserCodeLabel: TLabel;
    FPasswordLabel: TLabel;
    FComfirmButton: TButton;
    FCancelButton: TButton;
    function DoInit(): boolean;
  private
    procedure ComfirmEvent(Sender: TObject);
    procedure CancelEvent(Sender: TObject);
    procedure PromptMessage(Et: TEventType; const AMsg: string);

  public
    function Login(): boolean;
    function LogOut(): boolean;
    function ReLogin(): boolean;
  end;

implementation

uses
  uAuthenticationService;

{ TLoginService }

function TLoginService.DoInit(): boolean;
var
  prw: ICMLCLPropertyReaderWriter;
  lclg: ICMLCLGenerator;
  lclm: ICMLCLManager;
begin
  if InterfaceRegister.OutInterface(ICMLCLPropertyReaderWriter, prw) then
    if InterfaceRegister.OutInterface(ICMLCLGenerator, lclg) then
      if InterfaceRegister.OutInterface(ICMLCLManager, lclm) then
      begin
        FForm := TServiceForm(lclg.NewComponent('TServiceForm', nil));
        lclm.GetMainLCLGlobalSet.SetRequireDerivedFormResource(False);
        try
          FForm.BorderStyle := TFormBorderStyle.bsNone;

          FForm.PanelTop.Caption := '操作员登陆';
          prw.SetOrdProp(FForm, 'Width', 360);
          prw.SetOrdProp(FForm, 'Height', 480);

          FUserCodeLabel := TLabel(lclg.NewComponent('TLabel', FForm));
          FUserCodeLabel.Parent := FForm.PanelClient;
          FUserCodeLabel.Left := 80;
          FUserCodeLabel.Top := 40;
          FUserCodeLabel.Caption := '操作员';

          FPasswordLabel := TLabel(lclg.NewComponent('TLabel', FForm));
          FPasswordLabel.Parent := FForm.PanelClient;
          FPasswordLabel.Left := 80;
          FPasswordLabel.Top := 80;
          FPasswordLabel.Caption := '密码';

          FUserCodeEdit := TEdit(lclg.NewComponent('TEdit', FForm));
          FUserCodeEdit.Parent := FForm.PanelClient;
          FUserCodeEdit.Left := 160;
          FUserCodeEdit.Top := 40;

          FPasswordEdit := TEdit(lclg.NewComponent('TEdit', FForm));
          FPasswordEdit.Parent := FForm.PanelClient;
          FPasswordEdit.Left := 160;
          FPasswordEdit.Top := 80;

          FComfirmButton := TButton(lclg.NewComponent('TButton', FForm));
          FComfirmButton.Parent := FForm.PanelClient;
          FComfirmButton.Left := 80;
          FComfirmButton.Top := 120;
          FComfirmButton.OnClick := @ComfirmEvent;

          FCancelButton := TButton(lclg.NewComponent('TButton', FForm));
          FCancelButton.Parent := FForm.PanelClient;
          FCancelButton.Left := 160;
          FCancelButton.Top := 120;
          FCancelButton.OnClick := @CancelEvent;

          FInitialized := True;
        except
          on e: Exception do
            Messager.Error('Create fail!', e);
        end;
        //FForm.Show;

      end;
end;

procedure TLoginService.ComfirmEvent(Sender: TObject);
var
  AuthenticationService: IAuthenticationService;
begin
  if InterfaceRegister.OutInterface(IAuthenticationService, AuthenticationService) then
    if AuthenticationService.Login(Self.FUserCodeEdit.Text, Self.FPasswordEdit.Text) then
    begin
      AppSystem.GetParameter.Get('mpos.system').AddString('UserCode', Self.FUserCodeEdit.Text);
      FForm.ModalResult := mrOk;
    end;
end;

procedure TLoginService.CancelEvent(Sender: TObject);
begin
  FForm.ModalResult := mrCancel;
end;

procedure TLoginService.PromptMessage(Et: TEventType; const AMsg: string);
begin
  if Et = TEventType.etInfo then
    FForm.PanelBottom.Caption := Format(' 提示: %s', [''])
  else
    FForm.PanelBottom.Caption := Format(' 错误: %s', ['']);
end;

function TLoginService.Login(): boolean;
begin
  if not FInitialized then
    Self.DoInit();
  if Assigned(FForm) then
    FForm.ShowModal;
end;

function TLoginService.LogOut(): boolean;
begin

end;

function TLoginService.ReLogin(): boolean;
begin
  if not FInitialized then
    Self.DoInit();
  if Assigned(FForm) then
    FForm.ShowModal;
end;

end.
