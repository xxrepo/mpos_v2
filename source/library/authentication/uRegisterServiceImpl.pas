unit uRegisterServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPJson, LCLType, InterfaceBase, LCLIntf, StdCtrls, ComCtrls, ExtCtrls, Controls, LCLStrConsts, Forms, Dialogs,
  cm_messager, cm_LCL, cm_parameter,
  uFormService, uForm, uApp, uRegisterService, uCompany, uCompanyDAOImpl;

type

  { TRegisterService }

  TRegisterService = class(TCMMessageable, IRegisterService)
  private
    //////////UI/////////////
    FForm: TServiceForm;
    FCompCodeEdit: TComboBox;
    FShopCodeEdit: TEdit;
    FTermCodeEdit: TEdit;
    FTermUUIDEdit: TEdit;
    FCompCodeLabel: TLabel;
    FShopCodeLabel: TLabel;
    FTermCodeLabel: TLabel;
    FTermUUIDLabel: TLabel;
    FComfirmButton: TButton;
    FCancelButton: TButton;
    ///////////////////////
    FMACList: TStrings;
    FTermUUID, FMACAddress, FDiskNo: string;
    FCompCode, FShopCode, FTermCode: string;
    FAuthCode: string;
    FIsSwitchingSystem: boolean;
    procedure DoChangeCompany(Sender: TObject);

    function SaveAuthFile(): boolean;
    procedure DoInitialization;
    procedure DoLoadingAuthInfoForIniFile;
    procedure DoLoadingAuthInfoForParamter;
    procedure SetAreaSeviceConfig(const ACompCode: string);
  public
    constructor Create;
    destructor Destroy; override;
  private
    procedure ComfirmEvent(Sender: TObject);
    procedure CancelEvent(Sender: TObject);
    procedure PromptMessage(Et: TEventType; const AMsg: string);
  public
    property IsSwitchingSystem: boolean read FIsSwitchingSystem write FIsSwitchingSystem;
    function DoCheck(): boolean;
  public //Impl
    function DoPOSRegister(): boolean;
    function DoAuthorityCheck(): boolean;
    function DoRegisterSubmit(const ShopCode, TermCode, TermUUID: string): boolean;
  end;

const
  IniAuthFile = 'Config/authlicense.ini';
  XmlAuthFile = 'config/authlicense.xml';

implementation

uses
  uConstant, Process, RegExpr, LazUTF8, LSUtils, MD5, cm_DOM, cm_XML, cm_SysUtils, uRemoteService, PosService, uPOS;

{ TRegisterService }

constructor TRegisterService.Create;
begin
  inherited Create();
  FMACList := TStringList.Create;

  DoInitialization;
end;

destructor TRegisterService.Destroy;
begin
  FMACList.Free;
  inherited Destroy;
end;

procedure TRegisterService.ComfirmEvent(Sender: TObject);
var
  registerService: IRegisterService;
  company: TCOmpany;
begin
  if InterfaceRegister.OutInterface(IRegisterService, registerService) then
  begin
    company := TCOmpany(FCompCodeEdit.Items.Objects[FCompCodeEdit.ItemIndex]);
    if Assigned(company) then
    begin
      SetAreaSeviceConfig(company.Code);
      if registerService.DoRegisterSubmit(FShopCodeEdit.Text, FTermCodeEdit.Text, FTermUUIDEdit.Text) then
      begin
        AppSystem.GetMsgBox.MessageBox('注册成功!', '错误');
        FForm.ModalResult := mrOk;
      end;

    end;
  end;
end;

procedure TRegisterService.CancelEvent(Sender: TObject);
begin
  FForm.ModalResult := mrCancel;
end;

procedure TRegisterService.PromptMessage(Et: TEventType; const AMsg: string);
begin

end;

procedure TRegisterService.DoInitialization;

  function GetLocalMacAddresses(): string;

    function IsMacAddressFmt(const MacStr: string): boolean;
    var
      re: TRegExpr;
    begin
      Result := False;
      re := TRegExpr.Create;
      try
        re.Expression := '^([0-9]|[A-Z]|[a-z]){2}((-|:)([0-9]|[A-Z]|[a-z]){2}){5}$';
        Result := RE.Exec(MacStr);
      finally
        FreeAndNil(re);
      end;
    end;

  var
    AProcess: TProcess;
    AStringList, list: TStringList;
    iPos, i: integer;
    vStr, vMac, vTempStr: string;
  begin
    Result := '';
  {$IFDEF WINDOWS}
    Result := LSGetMACAddress();
  {$ELSE}
    try
      list := TStringList.Create;
      try
        AProcess := TProcess.Create(nil);
        AStringList := TStringList.Create;
        AProcess.CommandLine := 'ip addr';
        AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
        AProcess.Execute;
        AStringList.LoadFromStream(AProcess.Output);

        for i := 0 to AStringList.Count - 1 do
        begin
          vTempStr := Trim(AStringList[i]);
          iPos := Utf8Pos('link/ether', vTempStr);
          if (iPos = 0) and (Utf8Length(vTempStr) <= 28) then
            Continue;

          vTempStr := TRIM(StringReplace(vTempStr, 'link/ether', '', [rfReplaceAll, rfIgnoreCase]));
          vStr := Trim(Utf8LeftStr(vTempStr, 18));
          if IsMacAddressFmt(vStr) then
          begin
            vMac := UPPERCASE(StringReplace(vStr, ':', '-', [rfReplaceAll, rfIgnoreCase]));
            list.Add(vMac);
          end;
        end;
        Result := list.Text;
      except
        on e: Exception do
        begin

        end;
      end;
    finally
      FreeAndNil(list);
      FreeAndNil(AStringList);
      Aprocess.Destroy;
    end;
 {$ENDIF}
  end;

var
  i: integer;
begin
  IsSwitchingSystem := False;

  //FDiskNo := LSGetPhysicalSerialOfCurrentDisk;

  if not (Trim(LSGetMACAddress()) = '') then
  begin
    FMACAddress := LSGetMACAddress();
    FMACList.Add(FMACAddress);
  end
  else
  begin
    FMACList.Text := GetLocalMacAddresses();
  end;

  if FMACList.Count >= 1 then
    if not (Trim(FMACList[0]) = '') then
      POSSupport.GetParameter.Get(POS_System_MacAddress).ReString(Trim(FMACList[0]));

  for i := 0 to FMACList.Count - 1 do
    messager.Debug('FMACList: %s', [FMACList[i]]);
end;




function TRegisterService.DoAuthorityCheck(): boolean;
var
  AParameter: ICMParameter = nil;
begin
  Result := False;
  try
    //没有新装机或老机没有新版本的注册信息
    AParameter := AppSystem.GetParameter.Get('Authlicense');
    if AParameter.IsNull then
      if not FileExists(XmlAuthFile) then
        if FileExists(IniAuthFile) then
          IsSwitchingSystem := True;   //切换系统状态

    //验证授权
    Result := DoCheck();
  except
    on e: Exception do
      Messager.Error('POSRegister: %s %s', [e.ClassName, e.Message]);
  end;
end;


function TRegisterService.DoCheck(): boolean;
var
  i: integer;
  RegedtCode: string;
begin
  Result := False;
  try
    //切换系统从INI文件加载授权信息,否则从参数接口中加载
    if not IsSwitchingSystem then
      DoLoadingAuthInfoForParamter
    else
      DoLoadingAuthInfoForIniFile;

    if ((Trim(FAuthCode) = '') or (Trim(FTermUUID) = '')) then
    begin
      Messager.Error('DoVerification: 获取授权信息错误, 验证失败!');
      Exit;
    end;

    Messager.Debug('VerificationInfo: %s %s', [FTermUUID, FAuthCode]);

    for i := 0 to FMacList.Count - 1 do
    begin
      FMACAddress := FMacList[i];
      //计算注册码
      RegedtCode := MD5Print(MD5String(EncodeHexString(FMacAddress + FDiskNo + FTermUUID)));
      Messager.Debug('VerificationInfo: %s %s %s', [FTermUUID, FAuthCode, RegedtCode]);
      if not (RegedtCode = '') then
        if (Trim(FAuthCode) = Trim(RegedtCode)) then
        begin
          //解码保存在验证文件中的注册信息
          Messager.Debug('DoVerification 授权验证成功: %s %s %s', [FCompCode, FShopCode, FTermCode]);

          //验证成功, 加载网络服务地址
          SetAreaSeviceConfig(FCompCode);

          POSSupport.GetParameter.Get(POS_System_CompCode).ReString(FCompCode);
          POSSupport.GetParameter.Get(POS_System_ShopCode).ReString(FShopCode);
          POSSupport.GetParameter.Get(POS_System_TermCode).ReString(FTermCode);
          POSSupport.GetParameter.Get(POS_System_TermUUID).ReString(FTermUUID);
          POSSupport.GetParameter.Get(POS_System_AuthCode).ReString(FAuthCode);
          POSSupport.GetParameter.Get(POS_System_MacAddress).ReString(FMACAddress);

          //验证成功,切换系统状态则保存授权信息
          if IsSwitchingSystem then
            Self.SaveAuthFile();

          Result := True;
          Exit;
        end;
    end;
  except
    on e: Exception do
      Messager.Error('DoVerification: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TRegisterService.DoRegisterSubmit(const ShopCode, TermCode, TermUUID: string): boolean;
var
  PosSrv: IRemoteService;
  License, CompCode, AuthCode: string;
  regResp: RegisterResponse;
begin
  Result := False;
  try

    if not InterfaceRegister.OutInterface(IRemoteService, PosSrv) then
    begin
      Messager.Debug('注册使用的接口 %s 不存在,不可以注册!', ['IRemoteService']);
      Exit;
    end;

    License := EncodeHexString(FMacAddress + FDiskNo + TermUUID);
    regResp := PosSrv.RegisterMachine(ShopCode, TermCode, TermUUID, License, 1);
    if not (regResp.ResponseCode = ResponseResult.Success) then
    begin
      AppSystem.GetMsgBox.MessageBox('注册失败, 请通知管理员', '错误');
      Messager.Info('注册失败请求失败: %s', [regResp.ResponseMsg]);
      Exit;
    end;
    AuthCode := regResp.RegisterCode;
    CompCode := regResp.CompanyCode;

    Messager.Debug('AuthCode: %s', [AuthCode]);
    Messager.Debug('MD5Print(MD5String(License)): %s', [MD5Print(MD5String(License))]);

    //注册码验证成功
    if (AuthCode = MD5Print(MD5String(License))) then
    begin
      //解码保存在验证文件中的注册信息
      messager.Debug('DoVerification 授权验证成功: %s %s %s %s', [CompCode, ShopCode, TermCode, TermUUID]);

      POSSupport.GetParameter.Get(POS_System_CompCode).ReString(CompCode);
      POSSupport.GetParameter.Get(POS_System_ShopCode).ReString(ShopCode);
      POSSupport.GetParameter.Get(POS_System_TermCode).ReString(TermCode);
      POSSupport.GetParameter.Get(POS_System_TermUUID).ReString(TermUUID);
      POSSupport.GetParameter.Get(POS_System_AuthCode).ReString(AuthCode);


      //保存授权信息
      if Self.SaveAuthFile() then
        Result := True;
    end;
  except
    on e: Exception do
      Messager.Error('DoRegisterSubmit: %s %s', [e.ClassName, e.Message]);
  end;
end;




function TRegisterService.SaveAuthFile(): boolean;
var
  ns: TCMDOMNodeStreamer = nil;
  node: TCMDOMNode = nil;
begin
  Result := False;
  try
    //保存XML配置文件
    ns := TCMDOMNodeStreamer.Create(nil);
    node := TCMDOMNode.Create('Authlicense');
    try
      TCMDOMNode.Create('compCode', EncodeHexString(POSSupport.GetParameter.Get(POS_System_CompCode).AsString), node);
      TCMDOMNode.Create('shopCode', EncodeHexString(POSSupport.GetParameter.Get(POS_System_ShopCode).AsString), node);
      TCMDOMNode.Create('termCode', EncodeHexString(POSSupport.GetParameter.Get(POS_System_TermCode).AsString), node);
      TCMDOMNode.Create('termUUID', EncodeHexString(POSSupport.GetParameter.Get(POS_System_TermUUID).AsString), node);
      TCMDOMNode.Create('authCode', POSSupport.GetParameter.Get(POS_System_AuthCode).AsString, node);
      if ns.WriteXML(node, XmlAuthFile) then
        Result := True;
    except
      on e: Exception do
        Messager.Error('SaveAuthFile: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    FreeAndNil(node);
    FreeAndNil(ns);
  end;
end;



procedure TRegisterService.DoLoadingAuthInfoForParamter;
begin
  try
    Messager.Debug('DoLoadingAuthInfoForParamter.Start!');
    if AppSystem.GetParameter.Get('Authlicense').IsNull then
    begin
      Messager.Info('');
      Exit;
    end;
    FAuthCode := AppSystem.GetParameter.Get('Authlicense.authCode').AsString;
    FCompCode := DecodeHexString(AppSystem.GetParameter.Get('Authlicense.compCode').AsString);
    FShopCode := DecodeHexString(AppSystem.GetParameter.Get('Authlicense.shopCode').AsString);
    FTermCode := DecodeHexString(AppSystem.GetParameter.Get('Authlicense.termCode').AsString);
    FTermUUID := DecodeHexString(AppSystem.GetParameter.Get('Authlicense.termUUID').AsString);

    Messager.Debug(FAuthCode);
    Messager.Debug(FTermUUID);
    Messager.Debug('DoLoadingAuthInfoForParamter.End!');

  except
    on e: Exception do
      Messager.Error('DoLoadingAuthInfoForParamter: %s %s', [e.ClassName, e.Message]);
  end;
end;

procedure TRegisterService.SetAreaSeviceConfig(const ACompCode: string);
var
  company: TCompany;
  CompanyDAO: ICompanyDAO;
begin
  if InterfaceRegister.OutInterface(ICompanyDAO, CompanyDAO) then
  begin
    company := CompanyDAO.FindByCode(ACompCode);
    if assigned(company) then
      POSSupport.GetParameter.Get(POS_System_AreaConfig).ReString(company.AreaConfig);
  end;
end;

procedure TRegisterService.DoLoadingAuthInfoForIniFile;
var
  FRegInfo: TStrings;
  AParameter: ICMParameter;
begin
  try
    FRegInfo := TStringList.Create;
    try
      Messager.Debug('DoLoadingAuthInfoForIniFile.Start!');
      FRegInfo.LoadFromFile(IniAuthFile);

      FAuthCode := Copy(FRegInfo.Strings[4], 10, Length(FRegInfo.Strings[4]) - 9);
      FCompCode := DecodeHexString(Copy(FRegInfo.Strings[0], 10, Length(FRegInfo.Strings[0]) - 9));
      FShopCode := DecodeHexString(Copy(FRegInfo.Strings[1], 10, Length(FRegInfo.Strings[1]) - 9));
      FTermCode := DecodeHexString(Copy(FRegInfo.Strings[2], 10, Length(FRegInfo.Strings[2]) - 9));
      FTermUUID := DecodeHexString(Copy(FRegInfo.Strings[3], 10, Length(FRegInfo.Strings[3]) - 9));

      Messager.Debug(FAuthCode);
      Messager.Debug(FTermUUID);

      Messager.Debug('DoLoadingAuthInfoForIniFile.End!');
    except
      on e: Exception do
        Messager.Error('DoLoadingAuthInfoForIniFile: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    FRegInfo.Free;
  end;
end;

procedure TRegisterService.DoChangeCompany(Sender: TObject);
var
  ACompany: TCompany;
begin
  if (Sender is TComboBox) then
  begin
    ACompany := TCompany(TComboBox(Sender).Items.Objects[TComboBox(Sender).ItemIndex]);
    if Assigned(ACompany) then
      SetAreaSeviceConfig(ACompany.Code);
  end;
end;

function TRegisterService.DoPOSRegister(): boolean;
var
  prw: ICMLCLPropertyReaderWriter;
  lclg: ICMLCLGenerator;
  lclm: ICMLCLManager;
  CompanyDAO: ICompanyDAO;
  i: integer;
  CompanyList: TCompanyList;
begin
  if InterfaceRegister.OutInterface(ICMLCLPropertyReaderWriter, prw) then
    if InterfaceRegister.OutInterface(ICMLCLGenerator, lclg) then
      if InterfaceRegister.OutInterface(ICMLCLManager, lclm) then
      begin
        try
          FForm := TServiceForm(lclg.NewComponent('TServiceForm', nil));
          lclm.GetMainLCLGlobalSet.SetRequireDerivedFormResource(False);
          try
            FForm.BorderStyle := TFormBorderStyle.bsNone;

            //FForm.PanelTop.Caption := '设备注册';
            prw.SetStrProp(FForm, 'Caption', '设备注册');
            prw.SetOrdProp(FForm, 'Width', 360);
            prw.SetOrdProp(FForm, 'Height', 480);

            FCompCodeLabel := TLabel(lclg.NewComponent('TLabel', FForm));
            FCompCodeLabel.Parent := FForm.PanelClient;
            prw.SetOrdProp(FCompCodeLabel, 'Left', 40);
            prw.SetOrdProp(FCompCodeLabel, 'Top', 40);
            prw.SetStrProp(FCompCodeLabel, 'Caption', '公  司');

            FCompCodeEdit := TComboBox(lclg.NewComponent('TComboBox', FForm));
            FCompCodeEdit.Parent := FForm.PanelClient;
            FCompCodeEdit.Left := 120;
            FCompCodeEdit.Top := 40;
            FCompCodeEdit.Width := 200;
            FCompCodeEdit.ItemHeight := 30;

            FCompCodeEdit.OnChange := @DoChangeCompany;

            FShopCodeLabel := TLabel(lclg.NewComponent('TLabel', FForm));
            FShopCodeLabel.Parent := FForm.PanelClient;
            FShopCodeLabel.Left := 40;
            FShopCodeLabel.Top := 80;
            FShopCodeLabel.Caption := '店铺号';

            FShopCodeEdit := TEdit(lclg.NewComponent('TEdit', FForm));
            FShopCodeEdit.Parent := FForm.PanelClient;
            FShopCodeEdit.Left := 120;
            FShopCodeEdit.Top := 80;
            FShopCodeEdit.Width := 200;

            FTermCodeLabel := TLabel(lclg.NewComponent('TLabel', FForm));
            FTermCodeLabel.Parent := FForm.PanelClient;
            FTermCodeLabel.Left := 40;
            FTermCodeLabel.Top := 120;
            FTermCodeLabel.Caption := '设备号';

            FTermCodeEdit := TEdit(lclg.NewComponent('TEdit', FForm));
            FTermCodeEdit.Parent := FForm.PanelClient;
            FTermCodeEdit.Left := 120;
            FTermCodeEdit.Top := 120;
            FTermCodeEdit.Width := 200;

            FTermUUIDLabel := TLabel(lclg.NewComponent('TLabel', FForm));
            FTermUUIDLabel.Parent := FForm.PanelClient;
            FTermUUIDLabel.Left := 40;
            FTermUUIDLabel.Top := 160;
            FTermUUIDLabel.Caption := '设备ID';

            FTermUUIDEdit := TEdit(lclg.NewComponent('TEdit', FForm));
            FTermUUIDEdit.Parent := FForm.PanelClient;
            FTermUUIDEdit.Left := 120;
            FTermUUIDEdit.Top := 160;
            FTermUUIDEdit.Width := 200;

            FComfirmButton := TButton(lclg.NewComponent('TButton', FForm));
            FComfirmButton.Parent := FForm.PanelClient;
            FComfirmButton.Left := 40;
            FComfirmButton.Top := 200;
            FComfirmButton.OnClick := @ComfirmEvent;

            FCancelButton := TButton(lclg.NewComponent('TButton', FForm));
            FCancelButton.Parent := FForm.PanelClient;
            FCancelButton.Left := 120;
            FCancelButton.Top := 200;
            FCancelButton.OnClick := @CancelEvent;

            FShopCodeEdit.Text := '0198';
            FTermCodeEdit.Text := '019828';
            FTermUUIDEdit.Text := 'eeed4fb8a0f04480900dc93c5945705b';

            if InterfaceRegister.OutInterface(ICompanyDAO, CompanyDAO) then
            begin
              CompanyList := CompanyDAO.GetList;
              for i := 0 to CompanyList.Count - 1 do
                FCompCodeEdit.Items.AddObject('%s - 【%s】', [CompanyList[i].Name, CompanyList[i].AbbrName], CompanyList[i]);

              if FCompCodeEdit.Items.Count > 0 then
                FCompCodeEdit.ItemIndex := 0;
            end;

            if FForm.ShowModal() = mrOk then
              Result := True;
          except
            on e: Exception do
              Messager.Error('Create fail!', e);
          end;
        finally
          FForm.Free;
        end;
      end;
end;


end.















































