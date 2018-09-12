unit uRegisterServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_messager, uRegisterService, uMPos, cm_Parameter, Controls, FileUtil;

type

  { TRegisterServiceImpl }

  TRegisterServiceImpl = class(TCMMessageableComponent, IRegisterService)
  private
    FParameter: ICMParameter;
    FMACList: TStrings;
    FTermUUID, FMACAddress, FDiskNo: string;
    FCompCode, FShopCode, FTermCode: string;
    FAuthCode: string;
    procedure DoInitialization;
    procedure DoLoadingAuthInfoForIniFile;
    procedure DoLoadingAuthInfoForParamter;
    procedure SetMachineParameter;
    function SaveAuthFile(): boolean;
    ////
    function IsMacAddressFmt(const MacStr: string): boolean;
    function GetLocalMacAddresses(): string;
  public
    constructor Create(AOwner: TComponent; AParameter: ICMParameter);
    destructor Destroy; override;
  public
    function DoCheck(): boolean;
    function DoRegister(): boolean;
  public //Impl
    function POSRegister(): boolean;
    function DoRegisterSubmit(const ShopCode, TermCode, TermUUID: string; out RetMsg: string): boolean;
  end;

const
  IniAuthFile = 'Config/authlicense.ini';
  XmlAuthFile = 'config/authlicense.xml';

implementation

uses
  Process, RegExpr, LazUTF8, LSUtils, MD5, cm_DOM, cm_XML, cm_SysUtils, uFormRegister, uRemoteService;

{ TRegisterServiceImpl }

constructor TRegisterServiceImpl.Create(AOwner: TComponent; AParameter: ICMParameter);
begin
  inherited Create(AOwner);
  FParameter := AParameter;
  FMACList := TStringList.Create;

  DoInitialization;
end;

destructor TRegisterServiceImpl.Destroy;
begin
  FMACList.Free;
  inherited Destroy;
end;

procedure TRegisterServiceImpl.DoInitialization;
var
  i: integer;
begin
  FParameter.AddString('system', '');
  FDiskNo := LSGetPhysicalSerialOfCurrentDisk;

  if not (Trim(LSGetMACAddress()) = '') then
  begin
    FMACAddress := LSGetMACAddress();
    FMACList.Add(FMACAddress);
  end
  else
    FMACList.Text := GetLocalMacAddresses();

  for i := 0 to FMACList.Count - 1 do
    messager.Debug('FMACList: %s', [FMACList[i]]);
end;




function TRegisterServiceImpl.POSRegister(): boolean;
var
  IsSwitchingSystem: boolean = False;
  AParameter: ICMParameter = nil;
begin
  Result := False;

  try
    //没有新装机或老机没有新版本的注册信息
    AParameter := FParameter.Get('Authlicense');
    if AParameter.IsNull then
      if not FileExists(XmlAuthFile) then
        if FileExists(IniAuthFile) then
          IsSwitchingSystem := True;   //切换系统状态

    //切换系统从INI文件加载授权信息,否则从参数接口中加载
    if not IsSwitchingSystem then
      DoLoadingAuthInfoForParamter
    else
      DoLoadingAuthInfoForIniFile;

    //验证授权
    if DoCheck() then
    begin
      Result := True;
      //验证成功,切换系统状态则保存授权信息
      if IsSwitchingSystem then
        Self.SaveAuthFile();
    end
    else
    begin
      if DoRegister() then
        Result := True;
    end;
  except
    on e: Exception do
      Messager.Error('POSRegister: %s %s', [e.ClassName, e.Message]);
  end;
end;


function TRegisterServiceImpl.DoCheck(): boolean;
var
  i: integer;
  RegedtCode: string;
begin
  Result := False;
  try
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
          SetMachineParameter();

          Result := True;
          Exit;
        end;
    end;
  except
    on e: Exception do
      Messager.Error('DoVerification: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TRegisterServiceImpl.DoRegister(): boolean;
var
  RegForm: TRegisterForm;
begin
  Result := False;
  try
    RegForm := TRegisterForm.Create(Self);
    RegForm.RegisterService := IRegisterService(Self);
    if (RegForm.ShowModal() = mrOk) then
      Result := True;
  except
    on e: Exception do
      Messager.Error('DoRegister: %s %s', [e.ClassName, e.Message]);
  end;
end;

function TRegisterServiceImpl.DoRegisterSubmit(const ShopCode, TermCode, TermUUID: string; out RetMsg: string): boolean;
var
  SrvUrl: string;
  PosSrv: IRemoteService;
  License, CompCode, AuthCode, ARetMsg: string;
  RetCode: TResponseResult;
begin
  Result := False;
  try

    if not InterfaceRegister.OutInterface(IRemoteService, PosSrv) then
    begin
      Messager.Debug('注册使用的接口 %s 不存在,不可以注册!', ['IRemoteService']);
      Exit;
    end;

    SrvUrl := 'http://mpos.myj.com.cn/PosService.asmx';
    License := EncodeHexString(FMacAddress + FDiskNo + TermUUID);
    PosSrv.SetMachineParameterInterface(FParameter);
    if not (PosSrv.RegisterMachine(SrvUrl, ShopCode, TermCode, TermUUID, License, FMacAddress, 1, CompCode, AuthCode, RetCode, ARetMsg)) then
    begin
      RetMsg := ARetMsg;
      Messager.Debug('远程服务访问不成功!', ['']);
      Exit;
    end;

    RetMsg := ARetMsg;
    if not (RetCode = TResponseResult.Success) then
    begin
      Messager.Info('注册失败请求失败: %s', [RetMsg]);
      Exit;
    end;

    //注册码验证成功
    if (AuthCode = MD5Print(MD5String(License))) then
    begin
      //解码保存在验证文件中的注册信息
      FCompCode := CompCode;
      FShopCode := ShopCode;
      FTermCode := TermCode;
      FTermUUID := TermUUID;
      FAuthCode := AuthCode;
      messager.Debug('DoVerification 授权验证成功: %s %s %s', [FCompCode, FShopCode, FTermCode]);

      //重新设置注册信息参数
      SetMachineParameter;

      //保存注册信息
      if Self.SaveAuthFile() then
        Result := True;
    end;
  except
    on e: Exception do
      Messager.Error('DoRegisterSubmit: %s %s', [e.ClassName, e.Message]);
  end;
end;




function TRegisterServiceImpl.GetLocalMacAddresses(): string;
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




function TRegisterServiceImpl.SaveAuthFile(): boolean;
var
  ns: TCMDOMNodeStreamer = nil;
  node: TCMDOMNode = nil;
begin
  Result := False;
  try
    //保存XML配置文件
    ns := TCMDOMNodeStreamer.Create(Self);
    node := TCMDOMNode.Create('Authlicense');
    try
      TCMDOMNode.Create('compCode', EncodeHexString(FCompCode), node);
      TCMDOMNode.Create('shopCode', EncodeHexString(FShopCode), node);
      TCMDOMNode.Create('termCode', EncodeHexString(FTermCode), node);
      TCMDOMNode.Create('termUUID', EncodeHexString(FTermUUID), node);
      TCMDOMNode.Create('authCode', FAuthCode, node);
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



procedure TRegisterServiceImpl.DoLoadingAuthInfoForParamter;
begin
  try
    Messager.Debug('DoLoadingAuthInfoForParamter.Start!');

    FAuthCode := FParameter.Get('Authlicense.authCode').AsString;
    FCompCode := DecodeHexString(FParameter.Get('Authlicense.compCode').AsString);
    FShopCode := DecodeHexString(FParameter.Get('Authlicense.shopCode').AsString);
    FTermCode := DecodeHexString(FParameter.Get('Authlicense.termCode').AsString);
    FTermUUID := DecodeHexString(FParameter.Get('Authlicense.termUUID').AsString);
    Messager.Debug('DoLoadingAuthInfoForParamter.End!');

  except
    on e: Exception do
      Messager.Error('DoLoadingAuthInfoForParamter: %s %s', [e.ClassName, e.Message]);
  end;
end;

procedure TRegisterServiceImpl.DoLoadingAuthInfoForIniFile();
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
      Messager.Debug('DoLoadingAuthInfoForIniFile.End!');
    except
      on e: Exception do
        Messager.Error('DoLoadingAuthInfoForIniFile: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    FRegInfo.Free;
  end;
end;

procedure TRegisterServiceImpl.SetMachineParameter;
begin
  FParameter.Get('system').AddString('compCode', FCompCode);
  FParameter.Get('system').AddString('shopCode', FShopCode);
  FParameter.Get('system').AddString('termCode', FTermCode);
  FParameter.Get('system').AddString('authCode', FAuthCode);
  FParameter.Get('system').AddString('MacAddress', FMACAddress);
end;


function TRegisterServiceImpl.IsMacAddressFmt(const MacStr: string): boolean;
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

end.
























