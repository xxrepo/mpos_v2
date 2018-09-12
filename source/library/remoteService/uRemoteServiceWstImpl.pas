unit uRemoteServiceWstImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_messager, FPJSon, IDHttp, LazUtf8, DateUtils, cm_DOM, cm_XML, uRemoteService, TypInfo,
  PosService, PosService_Proxy, indy_http_protocol, soap_formatter, cm_Parameter;

type

  { TWstRemoteService }

  TWstRemoteService = class(TCMMessageableComponent, IRemoteService)
  private
    FParameter: ICMParameter;
    FPosSrvURL: string;
    //FConnectionTimeOut: integer;
    //FReadTimeOut: integer;
    //设置公共访问参数
    procedure SetRequestParamInfo(request: BaseRequest);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetMachineParameterInterface(AParameter: ICMParameter);
  public
    //返回服务器时间
    function GetCurrentTime(const AUrl: string; out ADatetime: TDatetime; out RetCode: TResponseResult; out RetMsg: string;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //注册设备
    function RegisterMachine(const AUrl: string; ShopCode, TermNo, TermID, License, MACAddress: string; iType: integer;
      out CompCode, AuthCode: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000;
      AReadTimeOut: integer = 4000): boolean;
    //获取版本详细信息
    function GetClientVersion(const AUrl: string; verId: integer; out VerNo: string; out VerDesc: string;
      out IsMust: boolean; out FileUrl: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000;
      AReadTimeOut: integer = 4000): boolean;
    //获取本设备最后初始化任务
    function GetLastInitialId(const AUrl: string; out InitID: integer; out InitStep: string; out RetCode: TResponseResult;
      out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取初始化任务文件
    function InitialClient(const AUrl: string; TskId, iInitType, iInitStep: integer; out FileType: integer;
      out FileUrl: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取待执行的任务
    function GetUpdateTasks(const AUrl: string; out Tasks: TCMDOMNode; out RetCode: TResponseResult; out RetMsg: string;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取待执行的指令
    function GetCommandTasks(const AUrl: string; out Tasks: TCMDOMNode; out RetCode: TResponseResult; out RetMsg: string;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //上传数据接口
    function UploadData(const AUrl: string; uSQL: string; out RetCode: TResponseResult; out RetMsg: string;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //上传销售数据接口
    function UploadShopSaleData(const AUrl: string; Uuids, uSQL: string; out RetCode: TResponseResult; out RetMsg: string;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //分类别上传数据接口
    function UnifyUploadData(const AUrl: string; IDs, uSQL: string; iDataType: integer; out RetCode: TResponseResult;
      out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //查询数据接口
    function QueryData(const AUrl: string; DataType: integer; SearchKey: string; out RetCode: TResponseResult;
      out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取单据打印清单
    function GetPosPrintBillInfo(const AUrl: string; OrderNO, BeginDate, EndDate: string; out Orders: TCMDOMNode;
      out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取单据打印内容
    function GetPosPrintBillData(const AUrl: string; OrderNO: string; out Orders: TCMDOMNode; out RetCode: TResponseResult;
      out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取标签数据
    function GetLabelData(const AUrl: string; iDataType, iGDGID, iPageSize, iPageIndex: integer; vSortCode: string;
      out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
  end;


implementation


procedure TWstRemoteService.SetRequestParamInfo(request: BaseRequest);
begin
  request.CompanyCode := Self.FParameter.Get('compCode').AsString;    //'GD'
  request.ShopCode := Self.FParameter.Get('shopCode').AsString;       //'1111';
  request.MacAddr := Self.FParameter.Get('MacAddress').AsString;        //  'DF-11-E3-33-44-55';
  request.TerminalNo := Self.FParameter.Get('termCode').AsString;     //'111101';
  request.AuthCode := Self.FParameter.Get('authCode').AsString;       // '1';
end;

constructor TWstRemoteService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TWstRemoteService.SetMachineParameterInterface(AParameter: ICMParameter);
begin

end;



function TWstRemoteService.GetCurrentTime(const AUrl: string; out ADatetime: TDatetime; out RetCode: TResponseResult;
  out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
var
  Srv: PosServiceSoap;
  rpt: GetCurrentTime_Type = nil;
  resp: GetCurrentTimeResponse = nil;
begin
  Result := False;
  try
    try
      //创建请求参数类实例并设置公共参数
      rpt := GetCurrentTime_Type.Create();
      SetRequestParamInfo(rpt.request);
      Messager.Debug('创建请求参数类实例并设置公共参数');

      //创建服务实例
      Indy_RegisterHTTP_Transport();
      Srv := wst_CreateInstance_PosServiceSoap('SOAP:', 'HTTP:', FPosSrvURL);
      Messager.Debug('创建服务实例');

      //发出请求
      resp := Srv.GetCurrentTime(rpt);
      if assigned(resp) then
      begin
        RetCode := TResponseResult(Ord(resp.GetCurrentTimeResult.ResponseCode));
        Retmsg := resp.GetCurrentTimeResult.ResponseMsg;
        ADateTime := StrToDatetimeDef(resp.GetCurrentTimeResult.CurrentTime, ADateTime);
        Result := True;
      end;

    except
      on e: Exception do

    end;
  finally

  end;
end;

function TWstRemoteService.RegisterMachine(const AUrl: string; ShopCode, TermNo, TermID, License, MACAddress: string;
  iType: integer; out CompCode, AuthCode: string; out RetCode: TResponseResult; out RetMsg: string;
  AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;

var
  Srv: POSServiceSoap;
  rpt: RegisterMachine_Type = nil;
  resp: RegisterMachineResponse = nil;
begin
  Result := False;
  try
    try
      //创建请求参数类实例并设置公共参数
      rpt := RegisterMachine_Type.Create;
      rpt.request.ShopCode := ShopCode;
      rpt.request.MacAddr := MACAddress;
      rpt.request.TerminalNo := TermNo;
      rpt.request.MUUID := TermID;
      rpt.Request.License := License;
      rpt.Request.RegisterType := iType;
      Messager.Debug('创建请求参数类实例并设置参数');

      //创建服务实例
      Indy_RegisterHTTP_Transport();
      Srv := wst_CreateInstance_PosServiceSoap('SOAP:', 'HTTP:', FPosSrvURL);
      Messager.Debug('创建服务实例' + FPosSrvURL);

      //发出服务请求
      resp := Srv.RegisterMachine(rpt);
      if assigned(resp) then
      begin
        RetCode := TResponseResult(Ord(resp.RegisterMachineResult.ResponseCode));
        Retmsg := resp.RegisterMachineResult.ResponseMsg;
        Messager.Debug('服务请求响应: %s %s %s', ['RegisterMachine', GetEnumName(TypeInfo(TResponseResult), Ord(RetCode)), RetMsg]);

        CompCode := string(resp.RegisterMachineResult.CompanyCode);
        AuthCode := resp.RegisterMachineResult.RegisterCode;
        Messager.Debug('创建请求参数类实例并设置参数');

        FreeAndNil(resp);
        Result := True;
      end;
    except
      on e: Exception do
      begin
        RetCode := TResponseResult.Failure;
        Retmsg := 'Network Error';
        Messager.Error('设备注册验证 | 失败:', e);
      end;
    end;
  finally
    rpt.Free;
  end;
end;

function TWstRemoteService.GetClientVersion(const AUrl: string; verId: integer; out VerNo: string; out VerDesc: string;
  out IsMust: boolean; out FileUrl: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000;
  AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;

function TWstRemoteService.GetLastInitialId(const AUrl: string; out InitID: integer; out InitStep: string;
  out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;

function TWstRemoteService.InitialClient(const AUrl: string; TskId, iInitType, iInitStep: integer; out FileType: integer;
  out FileUrl: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;

function TWstRemoteService.GetUpdateTasks(const AUrl: string; out Tasks: TCMDOMNode; out RetCode: TResponseResult;
  out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;

function TWstRemoteService.GetCommandTasks(const AUrl: string; out Tasks: TCMDOMNode; out RetCode: TResponseResult;
  out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;

function TWstRemoteService.UploadData(const AUrl: string; uSQL: string; out RetCode: TResponseResult; out RetMsg: string;
  AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;

function TWstRemoteService.UploadShopSaleData(const AUrl: string; Uuids, uSQL: string; out RetCode: TResponseResult;
  out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;

function TWstRemoteService.UnifyUploadData(const AUrl: string; IDs, uSQL: string; iDataType: integer;
  out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;

function TWstRemoteService.QueryData(const AUrl: string; DataType: integer; SearchKey: string; out RetCode: TResponseResult;
  out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;

function TWstRemoteService.GetPosPrintBillInfo(const AUrl: string; OrderNO, BeginDate, EndDate: string; out Orders: TCMDOMNode;
  out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;



begin
  Result := False;
end;

function TWstRemoteService.GetPosPrintBillData(const AUrl: string; OrderNO: string; out Orders: TCMDOMNode;
  out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;

function TWstRemoteService.GetLabelData(const AUrl: string; iDataType, iGDGID, iPageSize, iPageIndex: integer;
  vSortCode: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
begin
  Result := False;
end;



initialization

end.
