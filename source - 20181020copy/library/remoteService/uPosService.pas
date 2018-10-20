unit uPosService;

{$mode objfpc}{$H+}

interface

uses
  MD5,
  Base64,
  fpJson,
  JsonParser,
  Classes,
  SysUtils,
  TypInfo,
  Forms,
  IdHTTP,
  LazUtf8,
  FileUtil,
  FPJsonRtti,
  POS.BaseUtils,
  POS.Intface,
  POS.Entity.System,
  PosService,
  PosService_proxy,
  fpc_http_protocol,
  soap_formatter,
  cm_JsonUtils;

type

  { TPosService }
  TPosService = class(TInterfacedObject, IPosService)
  private
    LOG: ILOG;
    SPI: TSystemParamInfo;
    procedure SetRequestParamInfo(request: BaseRequest);        //设置请求参数信息
  public
    constructor Create(_SPI: TSystemParamInfo; _SysLog: ILog);
    function GetHttpInstance(): TIdhttp;

    //返回服务器时间,设备注册
    function IsSrvConnection(): boolean;
    function GetCurrentTime(): GetCurTimeResponse;
    function RegisterMachine(ShopCode, TermNo, TermID, License: string; iType: integer): RegisterResponse;

    //升级获取版本信息方法
    function GetLastInitialId(): LastInitialIdResponse;
    function InitialClient(TskId, iInitType, iInitStep: integer): InitialClientResponse;
    function GetClientVersion(verId: integer): ClientVersion;

    //获取下载任务及上传数据接口方法
    function GetUpdateTasks(): GetUpdateResponse;
    function GetCommandTasks(): GetCommandResponse;
    function UploadData(uSQL: string): UploadResponse;
    function UploadShopSaleData(Uuids, uSQL: string): UploadShopSaleDataResponse;
    function UnifyUploadData(IDs, uSQL: string; iDataType: integer): UploadResponse;

    function GetMTicketInfo(Param: TStringList): TJSONObject;
    function DestoryMTicket(Param: TStringList; var rsStr: string): boolean;
    function CancelMTicket(Param: TStringList; var rsStr: string): boolean; //劵冲正

    //获取下载票据打印数据接口方法
    function GetPosPrintBillInfo(OrderNO, BeginDate, EndDate: string): TJSONObject;
    function GetPosPrintBillData(OrderNO: string): TJSONObject;

    //获取下载标签打印数据接口方法
    function GetLabelData(iDataType, iGDGID, iPageSize, iPageIndex: integer; vSortCode: string): GetLabelDataResponse;

    //检索云端数据方法
    function QueryData(DataType: integer; SearchKey: string): QueryDataResponse;
  end;

const
  NoneJSONStr = '{}';
  NoneMethodStr = '{"RetCode" : -99, "RetMessage" : "你所访问的服务不存在!"}';

var

  VStreamer: TJSONStreamer;

implementation

constructor TPosService.Create(_SPI: TSystemParamInfo; _SysLog: ILog);
begin
  SPI := _SPI;
  LOG := _SysLog;
end;

function TPosService.GetHttpInstance(): TIdhttp;
begin
  Result := TIDHttp.Create;

  Result.ReadTimeout := 3000;
  Result.ConnectTimeout := 3000;
  Result.Request.Accept :=
    'image/gif, image/x-xbitmap, image/jpeg, imagepeg, application/x-' + 'shockwave-flash, */*';
  Result.Request.AcceptLanguage := 'zh-cn';
  Result.Request.ContentType := 'application/x-www-form-urlencoded';
  Result.Request.UserAgent :=
    'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; Maxthon; .NET ' + 'CLR 1.1.4322)';
  Result.HandleRedirects := True;
end;

//连接是否正常
function TPosService.IsSrvConnection(): boolean;
var
  ctr: GetCurTimeResponse;
begin
  Result := False;
  try
    ctr := GetCurrentTime();
    if (ctr.ResponseCode = ResponseResult.Success) then
      Result := True;
  finally
    ctr.Free;
  end;
end;

function TPosService.GetCurrentTime(): GetCurTimeResponse;
var
  Srv: POSServiceSoap;
  ctt: GetCurrentTime_Type;
  RetJSON, ParmJSON: TJSONObject;
begin
  Result := nil;
  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);

    ctt := GetCurrentTime_Type.Create();
    SetRequestParamInfo(ctt.request);

    try
      Result := Srv.GetCurrentTime(ctt).GetCurrentTimeResult;
      if not (Result.ResponseCode = ResponseResult.Success) then                 //请求获取服务主机时间不成功
      begin
        try
          LOG.Log('请求主机时间 | 地址: ' + SPI.Service.PosSrvURL);
          ParmJSON := VStreamer.ObjectToJSON(ctt);
          LOG.Log('请求主机时间 | 参数: ' + JSONObjAsJSON(ParmJSON));
          RetJSON := VStreamer.ObjectToJSON(Result);
          LOG.Log('请求主机时间 | 返回: ' + JSONObjAsJSON(RetJSON));
        finally
          ParmJSON.Free;
          RetJSON.Free;
        end;
      end;
    finally
      ctt.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := GetCurTimeResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
      end;
      SPI.System.SetNetConn(False);
      LOG.log(0, PChar('请求主机时间 | 失败:' + e.Message));
    end;
  end;

end;

//function GetPrintBillInfo(const GetPrintBillInfoParam: GetPrintBillInfo_Type): GetPrintBillInfoResponse;
//function GetPrintBillData(const GetPrintBillDataParam: GetPrintBillData_Type): GetPrintBillDataResponse;
//获取下载票据打印数据接口方法
function TPosService.GetPosPrintBillInfo(OrderNO, BeginDate, EndDate: string): TJSONObject;
var
  Srv: POSServiceSoap;
  pbt: GetPrintBillInfo_Type;
  pbr: GetBillListResponse;
  pbi: PrintBillInfo;
  Inx: integer;
  arr: TJSONArray;
  jso: TJSONObject;
begin
  Result := nil;
  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);

    pbt := GetPrintBillInfo_Type.Create;
    pbt.request.OrderNo := OrderNO;
    pbt.request.BeginDateTime := BeginDate;
    pbt.request.EndDateTime := EndDate;
    SetRequestParamInfo(pbt.request);

    try
      pbr := Srv.GetPrintBillInfo(pbt).GetPrintBillInfoResult;
      Result := VStreamer.ObjectToJSON(pbr);

      Result.Delete('PrintList');
      arr := TJSONArray.Create();
      for Inx := 0 to pbr.PrintList.Length - 1 do
      begin
        pbi := pbr.PrintList.Item[Inx];
        jso := TJSONObject.Create;
        jso.Add('BillPId', pbi.BillPId);
        jso.Add('OrderNo', pbi.OrderNo);
        jso.Add('OrderTime', pbi.OrderTime);
        jso.Add('BillTypeName', pbi.BillTypeName);
        arr.add(jso);
      end;
      Result.Add('PrintList', arr);

      LOG.Log(0, PChar('打印信息列表 | 返回: ' + JSONObjAsJSON(Result)));
    finally
      pbt.Free;
      pbr.Free;
    end;
  except
    on e: Exception do
    begin
      if (Result = nil) then
      begin
        Result := TJSONObject.Create;
        Result.Add('ResponseCode', GetEnumName(TypeInfo(ResponseResult), Ord(Failure)));
        Result.Add('ResponseMsg', 'Network Error!');
      end;
      SPI.System.SetNetConn(False);
      LOG.log(0, PChar('打印信息列表 | 失败:' + e.Message));
    end;
  end;

end;

//function GetPrintBillData(const GetPrintBillDataParam: GetPrintBillData_Type): GetPrintBillDataResponse;
function TPosService.GetPosPrintBillData(OrderNO: string): TJSONObject;
var
  Srv: POSServiceSoap;
  pbt: GetPrintBillData_Type;
  pbr: DownloadBillResponse;
  pbd: PrintBillData;
  Inx: integer;
  arr: TJSONArray;
  jso: TJSONObject;
begin

  Result := nil;
  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);

    pbt := GetPrintBillData_Type.Create;
    pbt.request.BillPId := OrderNO;
    SetRequestParamInfo(pbt.request);

    try
      pbr := Srv.GetPrintBillData(pbt).GetPrintBillDataResult;
      Result := VStreamer.ObjectToJSON(pbr);

      Result.Delete('PosPrintList');
      arr := TJSONArray.Create();
      for Inx := 0 to pbr.PosPrintList.Length - 1 do
      begin
        pbd := pbr.PosPrintList.Item[Inx];
        jso := TJSONObject(GetJSON(NoneJSONStr));
        jso.Add('OrderNo', pbd.OrderNo);
        jso.Add('BillPId', pbd.BillPId);
        jso.Add('PTContent', pbd.PTContent);
        arr.add(jso);
      end;
      Result.Add('PosPrintList', arr);

      LOG.Log(0, PChar('打印数据内容 | 返回: ' + JSONObjAsJSON(Result)));
    finally
      pbt.Free;
      pbr.Free;
    end;
  except
    on e: Exception do
    begin
      if (Result = nil) then
      begin
        Result := TJSONObject.Create;
        Result.Add('ResponseCode', GetEnumName(TypeInfo(ResponseResult), Ord(Failure)));
        Result.Add('ResponseMsg', 'Network Error!');
      end;
      SPI.System.SetNetConn(False);
      LOG.log(0, PChar('打印数据内容 | 失败:' + e.Message));
    end;
  end;
end;

//vCode, vMAC, vTermNo, vTermID, vLicense: string; vType: integer
function TPosService.RegisterMachine(ShopCode, TermNo, TermID, License: string; iType: integer): RegisterResponse;
var
  Srv: POSServiceSoap;
  rmt: RegisterMachine_Type;
  RetJSON, ParmJSON: TJSONObject;
begin
  Result := nil;
  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);

    rmt := RegisterMachine_Type.Create;
    rmt.request.ShopCode := ShopCode;
    rmt.request.MacAddr := SPI.System.MacAddress;
    rmt.request.TerminalNo := TermNo;
    rmt.request.MUUID := TermID;
    rmt.Request.License := License;
    rmt.Request.RegisterType := iType;

    try
      Result := Srv.RegisterMachine(rmt).RegisterMachineResult;
      LOG.Log(0, PChar('设备注册验证 | 地址: ' + SPI.Service.PosSrvURL));
      ParmJSON := VStreamer.ObjectToJSON(rmt);
      LOG.Log(0, PChar('设备注册验证 | 参数: ' + JSONObjAsJSON(ParmJSON)));
      RetJSON := VStreamer.ObjectToJSON(Result);
      LOG.Log(0, PChar('设备注册验证 | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      rmt.Free;
      ParmJSON.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if (Result = nil) then
      begin
        Result := RegisterResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
        RegisterResponse.
      end;
      SPI.System.SetNetConn(False);
      LOG.log(0, PChar('设备注册验证 | 失败:' + e.Message));
    end;
  end;
end;


function TPosService.GetUpdateTasks(): GetUpdateResponse;
var
  Srv: PosServiceSoap;
  utt: GetUpdateTasks_Type;
  RetJSON, JO: TJSONObject;
  Tsk: DownloadTask;
  Arr: TJSONArray;
  i: integer;
begin
  Result := nil;
  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);

    utt := GetUpdateTasks_Type.Create();
    SetRequestParamInfo(utt.request);

    try
      Result := Srv.GetUpdateTasks(utt).GetUpdateTasksResult;
      RetJSON := VStreamer.ObjectToJSON(Result);

      RetJSON.Delete('Tasks');
      Arr := TJSONArray.Create();
      for i := 0 to Result.Tasks.Length - 1 do
      begin
        Tsk := Result.Tasks.Item[i];
        JO := TJSONObject.Create();
        JO.Add('Pid', Tsk.PId);
        JO.Add('KeyId', Tsk.KeyId);
        JO.Add('FormatType', Tsk.FormatType);
        Arr.add(JO);
      end;
      RetJSON.Add('Tasks', Arr);

      LOG.Log(0, PChar('请求更新任务 | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      utt.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := GetUpdateResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
      end;
      SPI.System.SetNetConn(False);
      LOG.log(0, PChar('请求更新任务 | 失败:' + e.Message));
    end;
  end;
end;

function TPosService.GetCommandTasks(): GetCommandResponse;
var
  Srv: POSServiceSoap;
  ctt: GetCommandTasks_Type;
  RetJSON: TJSONObject;
  Tsk: CommandTask;
  JO: TJSONObject;
  arr: TJSONArray;
  i: integer;
begin
  Result := nil;
  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);
    ctt := GetCommandTasks_Type.Create();
    SetRequestParamInfo(ctt.request);

    try
      Result := Srv.GetCommandTasks(ctt).GetCommandTasksResult;
      RetJSON := VStreamer.ObjectToJSON(Result);

      RetJSON.Delete('Tasks');
      Arr := TJSONArray.Create();
      for i := 0 to Result.Tasks.Length - 1 do
      begin
        Tsk := Result.Tasks.Item[i];
        JO := TJSONObject(GetJSON(NoneJSONStr));
        JO.Add('Pid', Tsk.Pid);
        JO.Add('CmdType', Tsk.CmdType);
        JO.Add('CmdInfo', SysToUtf8(Tsk.CmdInfo));
        Arr.add(JO);
      end;
      RetJSON.Add('Tasks', Arr);

      LOG.Log(0, PChar('请求命令任务 | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      ctt.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := GetCommandResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
      end;
      SPI.System.SetNetConn(False);
      LOG.log(0, PChar('请求命令任务 | 失败:' + e.Message));
    end;
  end;
end;

function TPosService.UploadData(uSQL: string): UploadResponse;
var
  Srv: POSServiceSoap;
  udp: UploadData_Type;
  RetJSON: TJSONObject;
begin
  Result := nil;

  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);
    udp := UploadData_Type.Create;

    udp.request.ExecSQL := Base64.EncodeStringBase64(uSQL);
    SetRequestParamInfo(udp.request);

    try
      Result := Srv.UploadData(udp).UploadDataResult;
      RetJSON := VStreamer.ObjectToJSON(Result);
      LOG.Log(0, PChar('数据上传任务 | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      udp.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := UploadResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
      end;
      SPI.System.SetNetConn(False);
      LOG.Log(2, PChar('数据上传任务 | 失败:' + e.Message));
      LOG.Log(2, PChar('数据上传任务 | 参数:' + uSQL));
    end;
  end;

end;

function TPosService.UploadShopSaleData(Uuids, uSQL: string): UploadShopSaleDataResponse;
var
  Srv: POSServiceSoap;
  usd: UploadSaleData;
  RetJSON: TJSONObject;
begin
  Result := nil;
  try
    FPC_RegisterHTTP_Transport();
    //synapse_http_protocol.THTTPTransport.

    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);
    usd := UploadSaleData.Create;

    usd.request.UUID := Uuids;
    usd.request.ExecSQL := Base64.EncodeStringBase64(uSQL);
    SetRequestParamInfo(usd.request);


    try
      Result := Srv.UploadShopSaleData(usd).UploadSaleDataResult;
      RetJSON := VStreamer.ObjectToJSON(Result);
      LOG.Log(0, PChar('销售上传任务 | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      usd.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := UploadShopSaleDataResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
      end;
      SPI.System.SetNetConn(False);
      LOG.Log(2, PChar('销售上传任务 | 失败:' + e.Message));
      LOG.Log(2, PChar('销售上传任务 | 参数:' + uSQL));
    end;
  end;
end;

//function UnifyUploadData(const UnifyUploadDataParam: UnifyUploadData_Type): UnifyUploadDataResponse;
function TPosService.UnifyUploadData(IDs, uSQL: string; iDataType: integer): UploadResponse;
var
  Srv: POSServiceSoap;
  uud: UnifyUploadData_Type;
  RetJSON: TJSONObject;
begin
  Result := nil;
  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);
    uud := UnifyUploadData_Type.Create;

    uud.request.Id := IDs;
    uud.request.ExecSQL := Base64.EncodeStringBase64(uSQL);
    uud.request.DataType := iDataType;
    SetRequestParamInfo(uud.request);

    try
      Result := Srv.UnifyUploadData(uud).UnifyUploadDataResult.UploadDataResult;
      RetJSON := VStreamer.ObjectToJSON(Result);
      LOG.Log(0, PChar('通用上传任务 | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      uud.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := UploadResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
      end;
      SPI.System.SetNetConn(False);
      LOG.Log(2, PChar('销售上传任务 | 失败:' + e.Message));
      LOG.Log(2, PChar('销售上传任务 | 参数:' + uSQL));
    end;
  end;
end;

function TPosService.QueryData(DataType: integer; SearchKey: string): QueryDataResponse;
var
  Srv: POSServiceSoap;
  qdt: QueryDataForSql;
  RetJSON: TJSONObject;
begin

  Result := nil;

  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);
    qdt := QueryDataForSql.Create();
    qdt.request.DataType := DataType;
    qdt.request.SearchKey := SearchKey;
    SetRequestParamInfo(qdt.request);

    try
      Result := Srv.QueryData(qdt).QueryDataForSqlResult;
      RetJSON := VStreamer.ObjectToJSON(Result);
      LOG.Log(0, PChar('请求检索数据 | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      qdt.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := QueryDataResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
        Result.DataState := 0;
        Result.ExecSql := '';
      end;
      SPI.System.SetNetConn(False);
      LOG.Log(0, PChar('请求检索数据 | 失败: ' + e.Message));
    end;
  end;
end;


function TPosService.GetLastInitialId(): LastInitialIdResponse;
var
  Srv: POSServiceSoap;
  lii: GetLastInitialId_Type;
  RetJSON: TJSONObject;
begin
  Result := nil;
  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);
    lii := GetLastInitialId_Type.Create();
    SetRequestParamInfo(lii.request);


    try
      Result := Srv.GetLastInitialId(lii).GetLastInitialIdResult;
      RetJSON := VStreamer.ObjectToJSON(Result);
      LOG.Log(0, PChar('请求初始化ID | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      lii.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := LastInitialIdResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
      end;
      SPI.System.SetNetConn(False);
      LOG.Log(0, PChar('请求初始化ID | 失败:' + e.Message));
    end;
  end;
end;

function TPosService.InitialClient(TskId, iInitType, iInitStep: integer): InitialClientResponse;
var
  Srv: POSServiceSoap;
  ipt: InitialPos;
  RetJSON: TJSONObject;
begin
  Result := nil;
  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);

    ipt := InitialPos.Create();
    ipt.request.Id := TskID;
    ipt.request.InitType := iInitType;
    ipt.request.InitStep := iInitStep;
    SetRequestParamInfo(ipt.request);

    try
      Result := Srv.InitialClient(ipt).InitialPosResult;
      RetJSON := VStreamer.ObjectToJSON(Result);
      LOG.Log(0, PChar('请求初始化信息 | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      ipt.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := InitialClientResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
      end;
      SPI.System.SetNetConn(False);
      LOG.Log(0, PChar('请求初始化信息 | 失败:' + e.Message));
    end;
  end;
end;

function TPosService.GetClientVersion(verId: integer): ClientVersion;
var
  Srv: POSServiceSoap;
  cvp: GetClientVersion_Type;
  cvr: GetClientVerResponse;
  RetJSON: TJSONObject;
begin
  Result := nil;

  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);

    cvp := GetClientVersion_Type.Create();
    cvp.request.VerId := verid;
    SetRequestParamInfo(cvp.request);

    try
      cvr := Srv.GetClientVersion(cvp).GetClientVersionResult;
      Result := cvr.ClientVersion;
      RetJSON := VStreamer.ObjectToJSON(Result);
      LOG.Log(0, PChar('请求版本信息 | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      cvp.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := ClientVersion.Create();
        Result.VerNo := '';
      end;
      SPI.System.SetNetConn(False);
      LOG.Log(0, PChar('请求版本信息 | 失败:' + e.Message));
    end;
  end;
end;

//GetLabelData(const GetLabelDataParam: GetLabelData_Type): GetLabelDataResponse;
function TPosService.GetLabelData(iDataType, iGDGID, iPageSize, iPageIndex: integer; vSortCode: string): GetLabelDataResponse;
var
  Srv: POSServiceSoap;
  ldp: GetLabelForPrint;
  RetJSON: TJSONObject;
begin
  Result := nil;

  try
    FPC_RegisterHTTP_Transport();
    Srv := wst_CreateInstance_POSServiceSoap('SOAP:', 'HTTP:', SPI.Service.PosSrvURL);

    ldp := GetLabelForPrint.Create();
    ldp.request.DataType := iDataType;
    ldp.request.SortCode := vSortCode;
    ldp.request.GDGID := iGDGID;
    ldp.request.PageSize := iPageSize;
    ldp.request.PageIndex := iPageIndex;
    SetRequestParamInfo(ldp.request);

    try
      Result := Srv.GetLabelData(ldp).GetLabelForPrintResult;
      RetJSON := VStreamer.ObjectToJSON(Result);
      LOG.Log(0, PChar('请求标签数据 | 返回: ' + JSONObjAsJSON(RetJSON)));
    finally
      ldp.Free;
      RetJSON.Free;
    end;
  except
    on e: Exception do
    begin
      if Result = nil then
      begin
        Result := GetLabelDataResponse.Create();
        Result.ResponseCode := ResponseResult.Failure;
        Result.ResponseMsg := 'Network Error!';
      end;
      SPI.System.SetNetConn(False);
      LOG.Log(0, PChar('请求标签数据 | 失败:' + e.Message));
    end;
  end;
end;

function TPosService.GetMTicketInfo(Param: TStringList): TJSONObject;
var
  ResultData, ParamData: TStringStream;
  http: TIDHttp;
  vStrParam, sign: string;
  RetJSON: TJSONobject;
  list: TStringList;
  i: integer;
begin
  Result := TJSONObject.Create;
  if not SPI.System.IsNetConn then
    exit;
  try
    list := TStringList.Create;
    ParamData := TStringStream.Create('');
    ResultData := TStringStream.Create('');
    http := GetHttpInstance();
    try
      vStrParam := '';
      Param.Add(Format('%s=%s', ['PartnerCode', 'MYJ']));
      Param.Add(Format('%s=%s', ['AppCode', '1008']));
      Param.Sort;
      if Param.Count > 0 then
        for i := 0 to Param.Count - 1 do
          if vStrParam = '' then
            vStrParam := vStrParam + Param.Strings[i]
          else
            vStrParam := vStrParam + '&' + Param.Strings[i];

      sign := MD5Print(MD5String(vStrParam + 'ca6ecbaa26524f4f8156a5ebab5105a3'));
      vStrParam := vStrParam + '&Sign=' + sign;
      LOG.Log(0, PChar('请求券中心数据 | 参数: ' + vStrParam));

      //ParamData := TStringStream.Create(vStrParam);
      ParamData.WriteString(vStrParam);
      Http.Post(SPI.Service.MTicketURL, ParamData, ResultData);

      list.Clear;
      list.Delimiter := '&';
      list.DelimitedText := ResultData.DataString;

      if List.Count > 0 then
        for i := 0 to list.Count - 1 do
          Result.Add(list.Names[i], URLDecode(list.ValueFromIndex[i]));

      LOG.Log(0, PChar('请求券中心数据 | 返回: ' + JSONObjAsJSON(Result)));
    except
      on e: Exception do
      begin
        if (Result = nil) then
        begin
          Result := TJSONObject.Create;
          Result.Add('ResponseCode', GetEnumName(TypeInfo(ResponseResult), Ord(ResponseResult.Failure)));
          Result.Add('ResponseMsg', 'Network Error!');
        end;
        LOG.Log(0, PChar('请求券中心数据 | 失败: ' + e.Message));
      end
    end;
  finally
    FreeAndNil(http);
    FreeAndNil(list);
    FreeAndNil(ParamData);
    FreeAndNil(ResultData);
  end;
end;

function TPosService.DestoryMTicket(Param: TStringList; var rsStr: string): boolean;
var
  ResultData, ParamData: TStringStream;
  http: TIDHttp;
  vStrParam, sign: string;
  list: TStringList;
  i: integer;
  tStr: string;
  tRsJson: TJSONObject;
begin
  tRsJson := TJSONObject(GetJSON(NoneJSONStr));
  Result := False;
  if not SPI.System.IsNetConn then
    exit;
  try
    list := TStringList.Create;
    ParamData := TStringStream.Create('');
    ResultData := TStringStream.Create('');
    http := GetHttpInstance();
    try
      //开始核销劵////////////////////////////////////
      vStrParam := '';
      Param.Add('ServiceName=TicketService');
      Param.Add('Interface=CancelAfterVerification');
      Param.Add('VerifyTime=' + FormatDateTime('yyyy-mm-dd hh:mm:ss', now));
      Param.Add(Format('%s=%s', ['PartnerCode', 'MYJ']));
      Param.Add(Format('%s=%s', ['AppCode', '1008']));
      Param.Sort;
      if Param.Count > 0 then
        for i := 0 to Param.Count - 1 do
          if vStrParam = '' then
            vStrParam := vStrParam + Param.Strings[i]
          else
            vStrParam := vStrParam + '&' + Param.Strings[i];
      //log.Log(0,pchar('source==='+vStrParam + 'ca6ecbaa26524f4f8156a5ebab5105a3'));
      sign := MD5Print(MD5String(vStrParam + 'ca6ecbaa26524f4f8156a5ebab5105a3'));
      {
      //转码
      vStrParam := '';
      if Param.Count > 0 then
        for i := 0 to Param.Count - 1 do begin
          tStr := Trim(Param.Strings[i]);
          //log.Log(0,pchar('tstr==='+tstr));
          if system.Pos('Products=',tStr) > 0 then
          begin
            tStr := copy(tStr, system.Pos('=',tStr) + 1, length(tStr));
            zmJson := TJsonObject(GetJson(copy(tStr, system.Pos('Products=',tStr) + 1, length(tStr))));
            //LOG.Log(0, PChar('核销劵请求券中心数据 | 原产品列表json: ' + zmJson.AsJSON));
            zmJsonAry := zmJson.Get('ProductList',TJSONArray(GetJSON('[]')));
            //LOG.Log(0, PChar('ary length='+inttostr(zmjsonary.Count)));
            for j := 0 to zmJsonAry.Count - 1 do
            begin
              tzmJson := TJsonObject(zmJsonAry.Items[j]);
              tzmJson.Strings['Name'] := URLEncode(tzmJson.Get('Name'),False);
            end;
            tStr := 'Products='+zmJson.AsJSON;
          end;
          //log.Log(0,pchar('tstr1==='+tstr));
          if vStrParam = '' then
            vStrParam := vStrParam + tStr
          else
            vStrParam := vStrParam + '&' + tStr;
        end;
      //log.Log(0,pchar('new==='+vStrParam));
      }
      vStrParam := vStrParam + '&Sign=' + sign;
      LOG.Log(0, PChar('核销劵请求券中心数据 | 参数: ' + vStrParam));

      //ParamData := TStringStream.Create(vStrParam);
      ParamData.WriteString(vStrParam);
      Http.Post(SPI.Service.MTicketURL, ParamData, ResultData);

      list.Clear;
      list.Delimiter := '&';
      list.DelimitedText := ResultData.DataString;
      tRsJson.Clear;
      if List.Count > 0 then
        for i := 0 to list.Count - 1 do
          tRsJson.Add(list.Names[i], URLDecode(list.ValueFromIndex[i]));

      LOG.Log(0, PChar('核销劵请求券中心数据 | 返回: ' + JSONObjAsJSON(tRsJson)));
      //LOG.Log(0, PChar('核销劵请求券中心数据 | 返回: ' + tRsJson.AsJSON));
      tStr := tRsJson.Get('ResponseCode');
      if tStr = 'SUCCESS' then
      begin
        Result := True;
        rsStr := tRsJson.Get('ResponseMsg');
      end
      else
      begin
        Result := False;
        rsStr := tRsJson.Get('ResponseMsg');
      end;

    except
      on e: Exception do
      begin
        LOG.Log(0, PChar('请求券中心数据 | 失败: ' + e.Message));
      end
    end;
  finally
    FreeAndNil(http);
    FreeAndNil(list);
    FreeAndNil(ParamData);
    FreeAndNil(ResultData);
  end;
end;


function TPosService.CancelMTicket(Param: TStringList; var rsStr: string): boolean; //劵冲正
var
  ResultData, ParamData: TStringStream;
  http: TIDHttp;
  vStrParam, sign: string;
  list: TStringList;
  i: integer;
  tStr: string;
  tRsJson: TJSONObject;
begin
  tRsJson := TJSONObject(GetJSON(NoneJSONStr));
  Result := False;
  if not SPI.System.IsNetConn then
    Exit;
  try
    list := TStringList.Create;
    ParamData := TStringStream.Create('');
    ResultData := TStringStream.Create('');
    http := GetHttpInstance();
    try
      //开始冲正////////////////////////////////////
      vStrParam := '';
      Param.Add('ServiceName=TicketOrderService');
      Param.Add('Interface=RefundOrder');
      Param.Add('RefundTime=' + FormatDateTime('yyyy-mm-dd hh:mm:ss', now));
      Param.Add(Format('%s=%s', ['PartnerCode', 'MYJ']));
      Param.Add(Format('%s=%s', ['AppCode', '1008']));
      Param.Sort;
      if Param.Count > 0 then
        for i := 0 to Param.Count - 1 do
          if vStrParam = '' then
            vStrParam := vStrParam + Param.Strings[i]
          else
            vStrParam := vStrParam + '&' + Param.Strings[i];

      sign := MD5Print(MD5String(vStrParam + 'ca6ecbaa26524f4f8156a5ebab5105a3'));
      vStrParam := vStrParam + '&Sign=' + sign;
      LOG.Log(0, PChar('劵冲正请求券中心数据 | 参数: ' + vStrParam));

      //ParamData := TStringStream.Create(vStrParam);
      ParamData.WriteString(vStrParam);
      Http.Post(SPI.Service.MTicketURL, ParamData, ResultData);

      list.Clear;
      list.Delimiter := '&';
      list.DelimitedText := ResultData.DataString;
      tRsJson.Clear;
      if List.Count > 0 then
        for i := 0 to list.Count - 1 do
          tRsJson.Add(list.Names[i], URLDecode(list.ValueFromIndex[i]));

      LOG.Log(0, PChar('核销劵请求券中心数据 | 返回: ' + JSONObjAsJSON(tRsJson)));
      //LOG.Log(0, PChar('劵冲正请求券中心数据 | 返回: ' + tRsJson.AsJSON));
      tStr := tRsJson.Get('ResponseCode');
      if tStr = 'SUCCESS' then
      begin
        Result := True;
        rsStr := tRsJson.Get('ResponseMsg');
      end
      else
      begin
        Result := False;
        rsStr := tRsJson.Get('ResponseMsg');
      end;
    except
      on e: Exception do
      begin
        LOG.Log(0, PChar('请求券中心数据 | 失败: ' + e.Message));
      end
    end;
  finally
    FreeAndNil(http);
    FreeAndNil(list);
    FreeAndNil(ParamData);
    FreeAndNil(ResultData);
  end;
end;

function GetCouponInfo(vStr: string): TJSONObject;
begin
  Result := nil;
end;


procedure TPosService.SetRequestParamInfo(request: BaseRequest);
begin
  request.MacAddr := SPI.System.MacAddress;
  request.AuthCode := SPI.System.AuthCode;
  request.TerminalNo := SPI.System.TerminalNo;
  request.CompanyCode := SPI.System.CompanyCode;
  request.ShopCode := SPI.System.ShopCode;
end;

initialization
  VStreamer := TJSONStreamer.Create(nil);

end.










