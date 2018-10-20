unit uFormRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, LCLType,
  {$IFDEF UNIX}
  Unix,
  {$ENDIF}
  uRegisterService,
  LazUtf8,
  cm_theme, uForm;

type

  { TRegisterForm }

  TRegisterForm = class(TPOSForm)
    cbkCompany: TComboBox;
    edtTermCode: TEdit;
    edtShopCode: TEdit;
    BtnCancel: TPanel;
    btnConfrim: TPanel;
    edtTermUUID: TMemo;
    ToolImage: TImage;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PanelTitel: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure btnConfrimClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cbkCompanyChange(Sender: TObject);
    procedure cbkCompanyExit(Sender: TObject);
    procedure cbkCompanyKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edtMachineIDEnter(Sender: TObject);
    procedure edtTermUUIDKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edtTermCodeEnter(Sender: TObject);
    procedure edtTermCodeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edtShopCodeChange(Sender: TObject);
    procedure edtShopCodeEnter(Sender: TObject);
    procedure edtShopCodeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FRegisterService: IRegisterService;
    FIsDialogVisible: boolean;
    procedure InitUi;
  public
    { public declarations }
    procedure SetTheme(ATheme: ITheme); override;
  public
    { public declarations }
    property IsDialogVisible: boolean read FIsDialogVisible write FIsDialogVisible;
    property RegisterService: IRegisterService read FRegisterService write FRegisterService;
  end;


var
  RegisterForm: TRegisterForm;

implementation


{$R *.frm}

{ TRegisterForm }

procedure TRegisterForm.FormShow(Sender: TObject);
var
  s: string;
begin
  //InitUi;
  //FRegInfo := TStringList.Create;
  //cbkCompanyChange(nil);
end;


procedure TRegisterForm.InitUi;
begin
  edtShopCode.Clear;
  edtShopCode.SetFocus;
end;


procedure TRegisterForm.btnCancelClick(Sender: TObject);
begin
  self.ModalResult := mrCancel;
  Application.Terminate;
end;

procedure TRegisterForm.cbkCompanyChange(Sender: TObject);
var
  strCbkCom, strCodeCom: string;
begin
  if Utf8Length(cbkCompany.Text) > 2 then
    strCbkCom := Utf8LeftStr(Utf8RightStr(cbkCompany.Text, 2), 1);
  if Utf8Length(edtShopCode.Text) > 0 then
    strCodeCom := Utf8LeftStr(edtShopCode.Text, 1);
  if (strCodeCom = strCbkCom) then
    Exit;

  if Utf8Length(strCodeCom) = Length(strCodeCom) then
    edtShopCode.Text := strCbkCom + edtShopCode.Text
  else
    edtShopCode.Text := StringReplace(edtShopCode.Text, strCodeCom, strCbkCom, [rfReplaceAll]);
end;

procedure TRegisterForm.cbkCompanyExit(Sender: TObject);
begin
  //edtShopCode.SetFocus;
  //edtShopCode.SelStart := Utf8Length(edtShopCode.Text);
end;

procedure TRegisterForm.cbkCompanyKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    //Key := 0;
    edtShopCode.SetFocus;
  end;
end;



procedure TRegisterForm.edtMachineIDEnter(Sender: TObject);
begin
  Application.ProcessMessages;
  self.edtTermUUID.SelStart := Length(edtTermUUID.Text);
end;

procedure TRegisterForm.edtTermUUIDKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    btnConfrimClick(nil);
    //BtnConfrim.SetFocus;
  end;
end;

procedure TRegisterForm.edtTermCodeEnter(Sender: TObject);
begin
  Application.ProcessMessages;
  self.edtTermCode.SelStart := Length(edtTermCode.Text);
end;

procedure TRegisterForm.edtTermCodeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    //Key := 0;
    edtTermUUID.SetFocus;
  end;
end;

procedure TRegisterForm.edtShopCodeChange(Sender: TObject);
begin
  edtTermCode.Text := edtShopCode.Text;
end;

procedure TRegisterForm.edtShopCodeEnter(Sender: TObject);
begin
  Application.ProcessMessages;
  edtShopCode.SelStart := Length(edtShopCode.Text);
end;

procedure TRegisterForm.edtShopCodeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    //Key := 0;
    self.edtTermCode.SetFocus;
  end;
end;

procedure TRegisterForm.btnConfrimClick(Sender: TObject);
var
  ShopCode, TremCode, TremUUID, RetMsg: string;
begin
  //判断店铺号是否为空
  if Trim(edtShopCode.Text) = '' then
  begin
    edtShopCode.SetFocus;
    Application.MessageBox('店铺号不能允许为空,请输入', '注册', MB_OK + MB_ICONWARNING);
    Exit;
  end;

  //判断设备号是否为空
  if Trim(edtTermCode.Text) = '' then
  begin
    edtTermCode.SetFocus;
    Application.MessageBox('设备号不能允许为空,请输入', '注册', MB_OK + MB_ICONWARNING);
    Exit;
  end;

  //判断设备ID是否为空
  if Trim(edtTermUUID.Text) = '' then
  begin
    edtTermCode.SetFocus;
    Application.MessageBox('设备ID不能允许为空,请输入', '注册', MB_OK + MB_ICONWARNING);
    Exit;
  end;

  ShopCode := edtShopCode.Text;
  TremCode := edtTermCode.Text;
  TremUUID := edtTermUUID.Text;
  if not RegisterService.DoRegisterSubmit(edtShopCode.Text, edtTermCode.Text, edtTermUUID.Text, RetMsg) then
  begin
    Application.MessageBox(PChar(' 注册失败:'#13#13 + '  ' + RetMsg), '提示', MB_OK + MB_ICONWARNING);
    Exit;
  end;

  Application.MessageBox(PChar(' 设备注册成功!'), '提示', MB_OK + MB_ICONWARNING);
  self.ModalResult := mrOk;

end;



procedure TRegisterForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  Self.PanelTitel.Color := TColor(ATheme.Parameter.Get('backgroundColor').AsInteger);
  Self.Font.Size := ATheme.Parameter.Get('defaultFont').Get('size').AsInteger;
  Self.Font.Name := ATheme.Parameter.Get('defaultFont').Get('name').AsString;
end;

end.
