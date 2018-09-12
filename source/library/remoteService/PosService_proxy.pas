{
This unit has been produced by ws_helper.
  Input unit name : "x".
  This unit name  : "x_proxy".
  Date            : "2018-06-29 16:47:22".
}

unit PosService_Proxy;

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, PosService;

type


  TPosServiceSoap_Proxy = class(TBaseProxy, PosService.PosServiceSoap)
  protected
    class function GetServiceType(): PTypeInfo; override;
    function RegisterMachine(const RegisterMachineParam: RegisterMachine_Type): RegisterMachineResponse;
    function GetLastInitialId(const GetLastInitialIdParam: GetLastInitialId_Type): GetLastInitialIdResponse;
    function InitialClient(const InitialPosParam: InitialPos): InitialPosResponse;
    function GetCommandTasks(const GetCommandTasksParam: GetCommandTasks_Type): GetCommandTasksResponse;
    function GetUpdateTasks(const GetUpdateTasksParam: GetUpdateTasks_Type): GetUpdateTasksResponse;
    function GetClientVersion(const GetClientVersionParam: GetClientVersion_Type): GetClientVersionResponse;
    function UploadData(const UploadDataParam: UploadData_Type): UploadDataResponse;
    function GetCurrentTime(const GetCurrentTimeParam: GetCurrentTime_Type): GetCurrentTimeResponse;
    function QueryData(const QueryDataForSqlParam: QueryDataForSql): QueryDataForSqlResponse;
    function UploadShopSaleData(const UploadSaleDataParam: UploadSaleData): UploadSaleDataResponse;
    function GetPrintBillInfo(const GetPrintBillInfoParam: GetPrintBillInfo_Type): GetPrintBillInfoResponse;
    function GetPrintBillData(const GetPrintBillDataParam: GetPrintBillData_Type): GetPrintBillDataResponse;
    function UnifyUploadData(const UnifyUploadDataParam: UnifyUploadData_Type): UnifyUploadDataResponse;
    function ProcessExceptionData(const ProcessExceptionDataParam: ProcessExceptionData_Type): ProcessExceptionDataResponse;
    function GetLabelData(const GetLabelForPrintParam: GetLabelForPrint): GetLabelForPrintResponse;
  end;

function wst_CreateInstance_PosServiceSoap(const AFormat: string = 'SOAP:'; const ATransport: string = 'HTTP:';
  const AAddress: string = ''): PosServiceSoap;

implementation

uses wst_resources_imp, metadata_repository;

function wst_CreateInstance_PosServiceSoap(const AFormat: string; const ATransport: string; const AAddress: string): PosServiceSoap;
var
  locAdr: string;
begin
  locAdr := AAddress;
  if (locAdr = '') then
    locAdr := GetServiceDefaultAddress(TypeInfo(PosServiceSoap));
  Result := TPosServiceSoap_Proxy.Create('PosServiceSoap', AFormat + GetServiceDefaultFormatProperties(TypeInfo(PosServiceSoap)),
    ATransport + 'address=' + locAdr);
end;

{ TPosServiceSoap_Proxy implementation }

class function TPosServiceSoap_Proxy.GetServiceType(): PTypeInfo;
begin
  Result := TypeInfo(PosService.PosServiceSoap);
end;

function TPosServiceSoap_Proxy.RegisterMachine(const RegisterMachineParam: RegisterMachine_Type): RegisterMachineResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('RegisterMachine', GetTarget(), locCallContext);
    locSerializer.Put('RegisterMachine', TypeInfo(RegisterMachine_Type), RegisterMachineParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'RegisterMachineResponse';
    locSerializer.Get(TypeInfo(RegisterMachineResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.GetLastInitialId(const GetLastInitialIdParam: GetLastInitialId_Type): GetLastInitialIdResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('GetLastInitialId', GetTarget(), locCallContext);
    locSerializer.Put('GetLastInitialId', TypeInfo(GetLastInitialId_Type), GetLastInitialIdParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'GetLastInitialIdResponse';
    locSerializer.Get(TypeInfo(GetLastInitialIdResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.InitialClient(const InitialPosParam: InitialPos): InitialPosResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('InitialClient', GetTarget(), locCallContext);
    locSerializer.Put('InitialPos', TypeInfo(InitialPos), InitialPosParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'InitialPosResponse';
    locSerializer.Get(TypeInfo(InitialPosResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.GetCommandTasks(const GetCommandTasksParam: GetCommandTasks_Type): GetCommandTasksResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('GetCommandTasks', GetTarget(), locCallContext);
    locSerializer.Put('GetCommandTasks', TypeInfo(GetCommandTasks_Type), GetCommandTasksParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'GetCommandTasksResponse';
    locSerializer.Get(TypeInfo(GetCommandTasksResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.GetUpdateTasks(const GetUpdateTasksParam: GetUpdateTasks_Type): GetUpdateTasksResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('GetUpdateTasks', GetTarget(), locCallContext);
    locSerializer.Put('GetUpdateTasks', TypeInfo(GetUpdateTasks_Type), GetUpdateTasksParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'GetUpdateTasksResponse';
    locSerializer.Get(TypeInfo(GetUpdateTasksResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.GetClientVersion(const GetClientVersionParam: GetClientVersion_Type): GetClientVersionResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('GetClientVersion', GetTarget(), locCallContext);
    locSerializer.Put('GetClientVersion', TypeInfo(GetClientVersion_Type), GetClientVersionParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'GetClientVersionResponse';
    locSerializer.Get(TypeInfo(GetClientVersionResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.UploadData(const UploadDataParam: UploadData_Type): UploadDataResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('UploadData', GetTarget(), locCallContext);
    locSerializer.Put('UploadData', TypeInfo(UploadData_Type), UploadDataParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'UploadDataResponse';
    locSerializer.Get(TypeInfo(UploadDataResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.GetCurrentTime(const GetCurrentTimeParam: GetCurrentTime_Type): GetCurrentTimeResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('GetCurrentTime', GetTarget(), locCallContext);
    locSerializer.Put('GetCurrentTime', TypeInfo(GetCurrentTime_Type), GetCurrentTimeParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'GetCurrentTimeResponse';
    locSerializer.Get(TypeInfo(GetCurrentTimeResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.QueryData(const QueryDataForSqlParam: QueryDataForSql): QueryDataForSqlResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('QueryData', GetTarget(), locCallContext);
    locSerializer.Put('QueryDataForSql', TypeInfo(QueryDataForSql), QueryDataForSqlParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'QueryDataForSqlResponse';
    locSerializer.Get(TypeInfo(QueryDataForSqlResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.UploadShopSaleData(const UploadSaleDataParam: UploadSaleData): UploadSaleDataResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('UploadShopSaleData', GetTarget(), locCallContext);
    locSerializer.Put('UploadSaleData', TypeInfo(UploadSaleData), UploadSaleDataParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'UploadSaleDataResponse';
    locSerializer.Get(TypeInfo(UploadSaleDataResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.GetPrintBillInfo(const GetPrintBillInfoParam: GetPrintBillInfo_Type): GetPrintBillInfoResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('GetPrintBillInfo', GetTarget(), locCallContext);
    locSerializer.Put('GetPrintBillInfo', TypeInfo(GetPrintBillInfo_Type), GetPrintBillInfoParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'GetPrintBillInfoResponse';
    locSerializer.Get(TypeInfo(GetPrintBillInfoResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.GetPrintBillData(const GetPrintBillDataParam: GetPrintBillData_Type): GetPrintBillDataResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('GetPrintBillData', GetTarget(), locCallContext);
    locSerializer.Put('GetPrintBillData', TypeInfo(GetPrintBillData_Type), GetPrintBillDataParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'GetPrintBillDataResponse';
    locSerializer.Get(TypeInfo(GetPrintBillDataResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.UnifyUploadData(const UnifyUploadDataParam: UnifyUploadData_Type): UnifyUploadDataResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('UnifyUploadData', GetTarget(), locCallContext);
    locSerializer.Put('UnifyUploadData', TypeInfo(UnifyUploadData_Type), UnifyUploadDataParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'UnifyUploadDataResponse';
    locSerializer.Get(TypeInfo(UnifyUploadDataResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.ProcessExceptionData(const ProcessExceptionDataParam: ProcessExceptionData_Type): ProcessExceptionDataResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('ProcessExceptionData', GetTarget(), locCallContext);
    locSerializer.Put('ProcessExceptionData', TypeInfo(ProcessExceptionData_Type), ProcessExceptionDataParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'ProcessExceptionDataResponse';
    locSerializer.Get(TypeInfo(ProcessExceptionDataResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;

function TPosServiceSoap_Proxy.GetLabelData(const GetLabelForPrintParam: GetLabelForPrint): GetLabelForPrintResponse;
var
  locSerializer: IFormatterClient;
  locCallContext: ICallContext;
  locStrPrmName: string;
begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  try
    locSerializer.BeginCall('GetLabelData', GetTarget(), locCallContext);
    locSerializer.Put('GetLabelForPrint', TypeInfo(GetLabelForPrint), GetLabelForPrintParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
    Result := nil;
    locStrPrmName := 'GetLabelForPrintResponse';
    locSerializer.Get(TypeInfo(GetLabelForPrintResponse), locStrPrmName, Result);

  finally
    locSerializer.Clear();
  end;
end;


initialization
  {$i PosService.wst}

  {$IF DECLARED(Register_PosService_ServiceMetadata)}
  Register_PosService_ServiceMetadata();
  {$IFEND}
end.

























































