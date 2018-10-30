{
This unit has been produced by ws_helper.
  Input unit name : "PosService".
  This unit name  : "PosService".
  Date            : "2017-05-25 15:00:01".
}
unit PosService;

{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$DEFINE WST_RECORD_RTTI}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'http://www.meiyijia.com.cn/';
  sUNIT_NAME = 'PosService';

type

  RegisterMachine_Type = class;
  RegisterMachineResponse = class;
  GetLastInitialId_Type = class;
  GetLastInitialIdResponse = class;
  InitialPos = class;
  InitialPosResponse = class;
  GetCommandTasks_Type = class;
  GetCommandTasksResponse = class;
  GetUpdateTasks_Type = class;
  GetUpdateTasksResponse = class;
  GetClientVersion_Type = class;
  GetClientVersionResponse = class;
  UploadData_Type = class;
  UploadDataResponse = class;
  GetCurrentTime_Type = class;
  GetCurrentTimeResponse = class;
  QueryDataForSql = class;
  QueryDataForSqlResponse = class;
  UploadSaleData = class;
  UploadSaleDataResponse = class;
  GetPrintBillInfo_Type = class;
  GetPrintBillInfoResponse = class;
  GetPrintBillData_Type = class;
  GetPrintBillDataResponse = class;
  UnifyUploadData_Type = class;
  UnifyUploadDataResponse = class;
  ProcessExceptionData_Type = class;
  ProcessExceptionDataResponse = class;
  GetLabelForPrint = class;
  GetLabelForPrintResponse = class;
  RegisterRequest = class;
  BaseRequest = class;
  RegisterResponse = class;
  BaseResponse = class;
  LastInitialIdRequest = class;
  LastInitialIdResponse = class;
  InitialClientRequest = class;
  InitialClientResponse = class;
  InitialClientFile = class;
  GetCommandRequest = class;
  GetCommandResponse = class;
  ArrayOfCommandTask = class;
  CommandTask = class;
  GetUpdateRequest = class;
  GetUpdateResponse = class;
  ArrayOfDownloadTask = class;
  DownloadTask = class;
  GetClientVerRequest = class;
  GetClientVerResponse = class;
  ClientVersion = class;
  UploadRequest = class;
  UploadResponse = class;
  GetCurTimeRequest = class;
  GetCurTimeResponse = class;
  QueryDataRequest = class;
  QueryDataResponse = class;
  UploadShopSaleDataRequest = class;
  UploadShopSaleDataResponse = class;
  GetBillListRequest = class;
  GetBillListResponse = class;
  ArrayOfPrintBillInfo = class;
  PrintBillInfo = class;
  DownloadBillRequest = class;
  DownloadBillResponse = class;
  ArrayOfPrintBillData = class;
  PrintBillData = class;
  UploadDataRequest = class;
  ProcExceptDataRequest = class;
  ProcExceptDataResponse = class;
  GetLabelDataRequest = class;
  GetLabelDataResponse = class;

  ResponseResult = (
    Success
    , ParamError
    , TerminalNotExist
    , SysUnregister
    , IdentiyException
    , AuthCodeError
    , Failure
    , SysException
    );

  RegisterMachine_Type = class(TBaseComplexRemotable)
  private
    Frequest: RegisterRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: RegisterRequest read Frequest write Frequest stored wstHas_request;
  end;

  RegisterMachineResponse = class(TBaseComplexRemotable)
  private
    FRegisterMachineResult: RegisterResponse;
  private
    function wstHas_RegisterMachineResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property RegisterMachineResult: RegisterResponse read FRegisterMachineResult write FRegisterMachineResult stored wstHas_RegisterMachineResult;
  end;

  GetLastInitialId_Type = class(TBaseComplexRemotable)
  private
    Frequest: LastInitialIdRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: LastInitialIdRequest read Frequest write Frequest stored wstHas_request;
  end;

  GetLastInitialIdResponse = class(TBaseComplexRemotable)
  private
    FGetLastInitialIdResult: LastInitialIdResponse;
  private
    function wstHas_GetLastInitialIdResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property GetLastInitialIdResult: LastInitialIdResponse read FGetLastInitialIdResult
      write FGetLastInitialIdResult stored wstHas_GetLastInitialIdResult;
  end;

  InitialPos = class(TBaseComplexRemotable)
  private
    Frequest: InitialClientRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: InitialClientRequest read Frequest write Frequest stored wstHas_request;
  end;

  InitialPosResponse = class(TBaseComplexRemotable)
  private
    FInitialPosResult: InitialClientResponse;
  private
    function wstHas_InitialPosResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property InitialPosResult: InitialClientResponse read FInitialPosResult write FInitialPosResult stored wstHas_InitialPosResult;
  end;

  GetCommandTasks_Type = class(TBaseComplexRemotable)
  private
    Frequest: GetCommandRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: GetCommandRequest read Frequest write Frequest stored wstHas_request;
  end;

  GetCommandTasksResponse = class(TBaseComplexRemotable)
  private
    FGetCommandTasksResult: GetCommandResponse;
  private
    function wstHas_GetCommandTasksResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property GetCommandTasksResult: GetCommandResponse read FGetCommandTasksResult write FGetCommandTasksResult stored wstHas_GetCommandTasksResult;
  end;

  GetUpdateTasks_Type = class(TBaseComplexRemotable)
  private
    Frequest: GetUpdateRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: GetUpdateRequest read Frequest write Frequest stored wstHas_request;
  end;

  GetUpdateTasksResponse = class(TBaseComplexRemotable)
  private
    FGetUpdateTasksResult: GetUpdateResponse;
  private
    function wstHas_GetUpdateTasksResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property GetUpdateTasksResult: GetUpdateResponse read FGetUpdateTasksResult write FGetUpdateTasksResult stored wstHas_GetUpdateTasksResult;
  end;

  GetClientVersion_Type = class(TBaseComplexRemotable)
  private
    Frequest: GetClientVerRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: GetClientVerRequest read Frequest write Frequest stored wstHas_request;
  end;

  GetClientVersionResponse = class(TBaseComplexRemotable)
  private
    FGetClientVersionResult: GetClientVerResponse;
  private
    function wstHas_GetClientVersionResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property GetClientVersionResult: GetClientVerResponse read FGetClientVersionResult write FGetClientVersionResult
      stored wstHas_GetClientVersionResult;
  end;

  UploadData_Type = class(TBaseComplexRemotable)
  private
    Frequest: UploadRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: UploadRequest read Frequest write Frequest stored wstHas_request;
  end;

  UploadDataResponse = class(TBaseComplexRemotable)
  private
    FUploadDataResult: UploadResponse;
  private
    function wstHas_UploadDataResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property UploadDataResult: UploadResponse read FUploadDataResult write FUploadDataResult stored wstHas_UploadDataResult;
  end;

  GetCurrentTime_Type = class(TBaseComplexRemotable)
  private
    Frequest: GetCurTimeRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: GetCurTimeRequest read Frequest write Frequest stored wstHas_request;
  end;

  GetCurrentTimeResponse = class(TBaseComplexRemotable)
  private
    FGetCurrentTimeResult: GetCurTimeResponse;
  private
    function wstHas_GetCurrentTimeResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property GetCurrentTimeResult: GetCurTimeResponse read FGetCurrentTimeResult write FGetCurrentTimeResult stored wstHas_GetCurrentTimeResult;
  end;

  QueryDataForSql = class(TBaseComplexRemotable)
  private
    Frequest: QueryDataRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: QueryDataRequest read Frequest write Frequest stored wstHas_request;
  end;

  QueryDataForSqlResponse = class(TBaseComplexRemotable)
  private
    FQueryDataForSqlResult: QueryDataResponse;
  private
    function wstHas_QueryDataForSqlResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property QueryDataForSqlResult: QueryDataResponse read FQueryDataForSqlResult write FQueryDataForSqlResult stored wstHas_QueryDataForSqlResult;
  end;

  UploadSaleData = class(TBaseComplexRemotable)
  private
    Frequest: UploadShopSaleDataRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: UploadShopSaleDataRequest read Frequest write Frequest stored wstHas_request;
  end;

  UploadSaleDataResponse = class(TBaseComplexRemotable)
  private
    FUploadSaleDataResult: UploadShopSaleDataResponse;
  private
    function wstHas_UploadSaleDataResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property UploadSaleDataResult: UploadShopSaleDataResponse read FUploadSaleDataResult
      write FUploadSaleDataResult stored wstHas_UploadSaleDataResult;
  end;

  GetPrintBillInfo_Type = class(TBaseComplexRemotable)
  private
    Frequest: GetBillListRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: GetBillListRequest read Frequest write Frequest stored wstHas_request;
  end;

  GetPrintBillInfoResponse = class(TBaseComplexRemotable)
  private
    FGetPrintBillInfoResult: GetBillListResponse;
  private
    function wstHas_GetPrintBillInfoResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property GetPrintBillInfoResult: GetBillListResponse read FGetPrintBillInfoResult write FGetPrintBillInfoResult
      stored wstHas_GetPrintBillInfoResult;
  end;

  GetPrintBillData_Type = class(TBaseComplexRemotable)
  private
    Frequest: DownloadBillRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: DownloadBillRequest read Frequest write Frequest stored wstHas_request;
  end;

  GetPrintBillDataResponse = class(TBaseComplexRemotable)
  private
    FGetPrintBillDataResult: DownloadBillResponse;
  private
    function wstHas_GetPrintBillDataResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property GetPrintBillDataResult: DownloadBillResponse read FGetPrintBillDataResult write FGetPrintBillDataResult
      stored wstHas_GetPrintBillDataResult;
  end;

  UnifyUploadData_Type = class(TBaseComplexRemotable)
  private
    Frequest: UploadDataRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: UploadDataRequest read Frequest write Frequest stored wstHas_request;
  end;

  UnifyUploadDataResponse = class(TBaseComplexRemotable)
  private
    FUnifyUploadDataResult: UploadDataResponse;
  private
    function wstHas_UnifyUploadDataResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property UnifyUploadDataResult: UploadDataResponse read FUnifyUploadDataResult write FUnifyUploadDataResult stored wstHas_UnifyUploadDataResult;
  end;

  ProcessExceptionData_Type = class(TBaseComplexRemotable)
  private
    Frequest: ProcExceptDataRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: ProcExceptDataRequest read Frequest write Frequest stored wstHas_request;
  end;

  ProcessExceptionDataResponse = class(TBaseComplexRemotable)
  private
    FProcessExceptionDataResult: ProcExceptDataResponse;
  private
    function wstHas_ProcessExceptionDataResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property ProcessExceptionDataResult: ProcExceptDataResponse read FProcessExceptionDataResult
      write FProcessExceptionDataResult stored wstHas_ProcessExceptionDataResult;
  end;

  GetLabelForPrint = class(TBaseComplexRemotable)
  private
    Frequest: GetLabelDataRequest;
  private
    function wstHas_request(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property request: GetLabelDataRequest read Frequest write Frequest stored wstHas_request;
  end;

  GetLabelForPrintResponse = class(TBaseComplexRemotable)
  private
    FGetLabelForPrintResult: GetLabelDataResponse;
  private
    function wstHas_GetLabelForPrintResult(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property GetLabelForPrintResult: GetLabelDataResponse read FGetLabelForPrintResult write FGetLabelForPrintResult
      stored wstHas_GetLabelForPrintResult;
  end;

  BaseRequest = class(TBaseComplexRemotable)
  private
    FRequestId: UnicodeString;
    FCompanyCode: UnicodeString;
    FShopCode: UnicodeString;
    FMacAddr: UnicodeString;
    FTerminalNo: UnicodeString;
    FAuthCode: UnicodeString;
  private
    function wstHas_RequestId(): boolean;
    function wstHas_CompanyCode(): boolean;
    function wstHas_ShopCode(): boolean;
    function wstHas_MacAddr(): boolean;
    function wstHas_TerminalNo(): boolean;
    function wstHas_AuthCode(): boolean;
  published
    property RequestId: UnicodeString read FRequestId write FRequestId stored wstHas_RequestId;
    property CompanyCode: UnicodeString read FCompanyCode write FCompanyCode stored wstHas_CompanyCode;
    property ShopCode: UnicodeString read FShopCode write FShopCode stored wstHas_ShopCode;
    property MacAddr: UnicodeString read FMacAddr write FMacAddr stored wstHas_MacAddr;
    property TerminalNo: UnicodeString read FTerminalNo write FTerminalNo stored wstHas_TerminalNo;
    property AuthCode: UnicodeString read FAuthCode write FAuthCode stored wstHas_AuthCode;
  end;

  RegisterRequest = class(BaseRequest)
  private
    FMUUID: UnicodeString;
    FLicense: UnicodeString;
    FRegisterType: integer;
  private
    function wstHas_MUUID(): boolean;
    function wstHas_License(): boolean;
  published
    property MUUID: UnicodeString read FMUUID write FMUUID stored wstHas_MUUID;
    property License: UnicodeString read FLicense write FLicense stored wstHas_License;
    property RegisterType: integer read FRegisterType write FRegisterType;
  end;

  BaseResponse = class(TBaseComplexRemotable)
  private
    FResponseCode: ResponseResult;
    FResponseMsg: UnicodeString;
  private
    function wstHas_ResponseMsg(): boolean;
  published
    property ResponseCode: ResponseResult read FResponseCode write FResponseCode;
    property ResponseMsg: UnicodeString read FResponseMsg write FResponseMsg stored wstHas_ResponseMsg;
  end;

  RegisterResponse = class(BaseResponse)
  private
    FShopCode: UnicodeString;
    FCompanyCode: UnicodeString;
    FRegisterCode: UnicodeString;
    FMUUID: UnicodeString;
  private
    function wstHas_ShopCode(): boolean;
    function wstHas_CompanyCode(): boolean;
    function wstHas_RegisterCode(): boolean;
    function wstHas_MUUID(): boolean;
  published
    property ShopCode: UnicodeString read FShopCode write FShopCode stored wstHas_ShopCode;
    property CompanyCode: UnicodeString read FCompanyCode write FCompanyCode stored wstHas_CompanyCode;
    property RegisterCode: UnicodeString read FRegisterCode write FRegisterCode stored wstHas_RegisterCode;
    property MUUID: UnicodeString read FMUUID write FMUUID stored wstHas_MUUID;
  end;

  LastInitialIdRequest = class(BaseRequest)
  end;

  LastInitialIdResponse = class(BaseResponse)
  private
    FId: int64;
    FStep: UnicodeString;
  private
    function wstHas_Step(): boolean;
  published
    property Id: int64 read FId write FId;
    property Step: UnicodeString read FStep write FStep stored wstHas_Step;
  end;

  InitialClientRequest = class(BaseRequest)
  private
    FId: integer;
    FInitType: integer;
    FInitStep: integer;
  published
    property Id: integer read FId write FId;
    property InitType: integer read FInitType write FInitType;
    property InitStep: integer read FInitStep write FInitStep;
  end;

  InitialClientResponse = class(BaseResponse)
  private
    FClientFile: InitialClientFile;
  private
    function wstHas_ClientFile(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property ClientFile: InitialClientFile read FClientFile write FClientFile stored wstHas_ClientFile;
  end;

  InitialClientFile = class(TBaseComplexRemotable)
  private
    FDataType: integer;
    FFileUrl: UnicodeString;
  private
    function wstHas_FileUrl(): boolean;
  published
    property DataType: integer read FDataType write FDataType;
    property FileUrl: UnicodeString read FFileUrl write FFileUrl stored wstHas_FileUrl;
  end;

  GetCommandRequest = class(BaseRequest)
  end;

  GetCommandResponse = class(BaseResponse)
  private
    FIsHasTask: boolean;
    FTasks: ArrayOfCommandTask;
  private
    function wstHas_Tasks(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property IsHasTask: boolean read FIsHasTask write FIsHasTask;
    property Tasks: ArrayOfCommandTask read FTasks write FTasks stored wstHas_Tasks;
  end;

  CommandTask = class(TBaseComplexRemotable)
  private
    FPid: int64;
    FCmdType: integer;
    FCmdInfo: UnicodeString;
    FCreateDate: TDateTimeRemotable;
  private
    function wstHas_CmdInfo(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property Pid: int64 read FPid write FPid;
    property CmdType: integer read FCmdType write FCmdType;
    property CmdInfo: UnicodeString read FCmdInfo write FCmdInfo stored wstHas_CmdInfo;
    property CreateDate: TDateTimeRemotable read FCreateDate write FCreateDate;
  end;

  GetUpdateRequest = class(BaseRequest)
  end;

  GetUpdateResponse = class(BaseResponse)
  private
    FIsHasTask: boolean;
    FTasks: ArrayOfDownloadTask;
  private
    function wstHas_Tasks(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property IsHasTask: boolean read FIsHasTask write FIsHasTask;
    property Tasks: ArrayOfDownloadTask read FTasks write FTasks stored wstHas_Tasks;
  end;

  DownloadTask = class(TBaseComplexRemotable)
  private
    FPId: int64;
    FFormatType: integer;
    FKeyId: int64;
    FExecSql: UnicodeString;
  private
    function wstHas_ExecSql(): boolean;
  published
    property PId: int64 read FPId write FPId;
    property FormatType: integer read FFormatType write FFormatType;
    property KeyId: int64 read FKeyId write FKeyId;
    property ExecSql: UnicodeString read FExecSql write FExecSql stored wstHas_ExecSql;
  end;

  GetClientVerRequest = class(BaseRequest)
  private
    FVerId: integer;
  published
    property VerId: integer read FVerId write FVerId;
  end;

  GetClientVerResponse = class(BaseResponse)
  private
    FClientVersion: PosService.ClientVersion;
  private
    function wstHas_ClientVersion(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property ClientVersion: PosService.ClientVersion read FClientVersion write FClientVersion stored wstHas_ClientVersion;
  end;

  ClientVersion = class(TBaseComplexRemotable)
  private
    FVerId: int64;
    FVerNo: UnicodeString;
    FVerDesc: UnicodeString;
    FIsMust: boolean;
    FUpdateFileUrl: UnicodeString;
  private
    function wstHas_VerNo(): boolean;
    function wstHas_VerDesc(): boolean;
    function wstHas_UpdateFileUrl(): boolean;
  published
    property VerId: int64 read FVerId write FVerId;
    property VerNo: UnicodeString read FVerNo write FVerNo stored wstHas_VerNo;
    property VerDesc: UnicodeString read FVerDesc write FVerDesc stored wstHas_VerDesc;
    property IsMust: boolean read FIsMust write FIsMust;
    property UpdateFileUrl: UnicodeString read FUpdateFileUrl write FUpdateFileUrl stored wstHas_UpdateFileUrl;
  end;

  UploadRequest = class(BaseRequest)
  private
    FExecSql: UnicodeString;
  private
    function wstHas_ExecSql(): boolean;
  published
    property ExecSql: UnicodeString read FExecSql write FExecSql stored wstHas_ExecSql;
  end;

  UploadResponse = class(BaseResponse)
  private
    FState: integer;
  published
    property State: integer read FState write FState;
  end;

  GetCurTimeRequest = class(BaseRequest)
  end;

  GetCurTimeResponse = class(BaseResponse)
  private
    FCurrentTime: UnicodeString;
  private
    function wstHas_CurrentTime(): boolean;
  published
    property CurrentTime: UnicodeString read FCurrentTime write FCurrentTime stored wstHas_CurrentTime;
  end;

  QueryDataRequest = class(BaseRequest)
  private
    FDataType: integer;
    FSearchKey: UnicodeString;
  private
    function wstHas_SearchKey(): boolean;
  published
    property DataType: integer read FDataType write FDataType;
    property SearchKey: UnicodeString read FSearchKey write FSearchKey stored wstHas_SearchKey;
  end;

  QueryDataResponse = class(BaseResponse)
  private
    FDataState: integer;
    FExecSql: UnicodeString;
  private
    function wstHas_ExecSql(): boolean;
  published
    property DataState: integer read FDataState write FDataState;
    property ExecSql: UnicodeString read FExecSql write FExecSql stored wstHas_ExecSql;
  end;

  UploadShopSaleDataRequest = class(BaseRequest)
  private
    FUUID: UnicodeString;
    FExecSql: UnicodeString;
  private
    function wstHas_UUID(): boolean;
    function wstHas_ExecSql(): boolean;
  published
    property UUID: UnicodeString read FUUID write FUUID stored wstHas_UUID;
    property ExecSql: UnicodeString read FExecSql write FExecSql stored wstHas_ExecSql;
  end;

  UploadShopSaleDataResponse = class(BaseResponse)
  private
    FState: integer;
    FMsg: UnicodeString;
  private
    function wstHas_Msg(): boolean;
  published
    property State: integer read FState write FState;
    property Msg: UnicodeString read FMsg write FMsg stored wstHas_Msg;
  end;

  GetBillListRequest = class(BaseRequest)
  private
    FOrderNo: UnicodeString;
    FBeginDateTime: UnicodeString;
    FEndDateTime: UnicodeString;
  private
    function wstHas_OrderNo(): boolean;
    function wstHas_BeginDateTime(): boolean;
    function wstHas_EndDateTime(): boolean;
  published
    property OrderNo: UnicodeString read FOrderNo write FOrderNo stored wstHas_OrderNo;
    property BeginDateTime: UnicodeString read FBeginDateTime write FBeginDateTime stored wstHas_BeginDateTime;
    property EndDateTime: UnicodeString read FEndDateTime write FEndDateTime stored wstHas_EndDateTime;
  end;

  GetBillListResponse = class(BaseResponse)
  private
    FPrintList: ArrayOfPrintBillInfo;
  private
    function wstHas_PrintList(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property PrintList: ArrayOfPrintBillInfo read FPrintList write FPrintList stored wstHas_PrintList;
  end;

  PrintBillInfo = class(TBaseComplexRemotable)
  private
    FBillPId: UnicodeString;
    FOrderNo: UnicodeString;
    FOrderTime: UnicodeString;
    FBillTypeName: UnicodeString;
  private
    function wstHas_BillPId(): boolean;
    function wstHas_OrderNo(): boolean;
    function wstHas_OrderTime(): boolean;
    function wstHas_BillTypeName(): boolean;
  published
    property BillPId: UnicodeString read FBillPId write FBillPId stored wstHas_BillPId;
    property OrderNo: UnicodeString read FOrderNo write FOrderNo stored wstHas_OrderNo;
    property OrderTime: UnicodeString read FOrderTime write FOrderTime stored wstHas_OrderTime;
    property BillTypeName: UnicodeString read FBillTypeName write FBillTypeName stored wstHas_BillTypeName;
  end;

  DownloadBillRequest = class(BaseRequest)
  private
    FBillPId: UnicodeString;
  private
    function wstHas_BillPId(): boolean;
  published
    property BillPId: UnicodeString read FBillPId write FBillPId stored wstHas_BillPId;
  end;

  DownloadBillResponse = class(BaseResponse)
  private
    FPosPrintList: ArrayOfPrintBillData;
  private
    function wstHas_PosPrintList(): boolean;
  public
    constructor Create(); override;
    procedure FreeObjectProperties(); override;
  published
    property PosPrintList: ArrayOfPrintBillData read FPosPrintList write FPosPrintList stored wstHas_PosPrintList;
  end;

  PrintBillData = class(TBaseComplexRemotable)
  private
    FBillPId: UnicodeString;
    FOrderNo: UnicodeString;
    FPTContent: UnicodeString;
  private
    function wstHas_BillPId(): boolean;
    function wstHas_OrderNo(): boolean;
    function wstHas_PTContent(): boolean;
  published
    property BillPId: UnicodeString read FBillPId write FBillPId stored wstHas_BillPId;
    property OrderNo: UnicodeString read FOrderNo write FOrderNo stored wstHas_OrderNo;
    property PTContent: UnicodeString read FPTContent write FPTContent stored wstHas_PTContent;
  end;

  UploadDataRequest = class(BaseRequest)
  private
    FDataType: integer;
    FId: UnicodeString;
    FExecSql: UnicodeString;
  private
    function wstHas_Id(): boolean;
    function wstHas_ExecSql(): boolean;
  published
    property DataType: integer read FDataType write FDataType;
    property Id: UnicodeString read FId write FId stored wstHas_Id;
    property ExecSql: UnicodeString read FExecSql write FExecSql stored wstHas_ExecSql;
  end;

  ProcExceptDataRequest = class(BaseRequest)
  private
    FBatchSize: integer;
  published
    property BatchSize: integer read FBatchSize write FBatchSize;
  end;

  ProcExceptDataResponse = class(BaseResponse)
  end;

  GetLabelDataRequest = class(BaseRequest)
  private
    FDataType: integer;
    FSortCode: UnicodeString;
    FGDGID: integer;
    FPageSize: integer;
    FPageIndex: integer;
  private
    function wstHas_SortCode(): boolean;
  published
    property DataType: integer read FDataType write FDataType;
    property SortCode: UnicodeString read FSortCode write FSortCode stored wstHas_SortCode;
    property GDGID: integer read FGDGID write FGDGID;
    property PageSize: integer read FPageSize write FPageSize;
    property PageIndex: integer read FPageIndex write FPageIndex;
  end;

  GetLabelDataResponse = class(BaseResponse)
  private
    FResponseValue: UnicodeString;
  private
    function wstHas_ResponseValue(): boolean;
  published
    property ResponseValue: UnicodeString read FResponseValue write FResponseValue stored wstHas_ResponseValue;
  end;

  ArrayOfCommandTask = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: integer): CommandTask;
  public
    class function GetItemClass(): TBaseRemotableClass; override;
    function Add(): CommandTask; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition: integer): CommandTask; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex: integer]: CommandTask read GetItem; default;
  end;

  ArrayOfDownloadTask = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: integer): DownloadTask;
  public
    class function GetItemClass(): TBaseRemotableClass; override;
    function Add(): DownloadTask; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition: integer): DownloadTask; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex: integer]: DownloadTask read GetItem; default;
  end;

  ArrayOfPrintBillInfo = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: integer): PrintBillInfo;
  public
    class function GetItemClass(): TBaseRemotableClass; override;
    function Add(): PrintBillInfo; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition: integer): PrintBillInfo; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex: integer]: PrintBillInfo read GetItem; default;
  end;

  ArrayOfPrintBillData = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: integer): PrintBillData;
  public
    class function GetItemClass(): TBaseRemotableClass; override;
    function Add(): PrintBillData; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition: integer): PrintBillData; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex: integer]: PrintBillData read GetItem; default;
  end;

  PosServiceSoap = interface(IInvokable)
    ['{9861EA14-1527-4818-A66F-4C1C5B868B35}']
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

procedure Register_PosService_ServiceMetadata();

implementation

uses metadata_repository, record_rtti, wst_types;

{ RegisterMachine_Type }

constructor RegisterMachine_Type.Create();
begin
  inherited Create();
  Frequest := RegisterRequest.Create();
end;

procedure RegisterMachine_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function RegisterMachine_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ RegisterMachineResponse }

constructor RegisterMachineResponse.Create();
begin
  inherited Create();
  FRegisterMachineResult := RegisterResponse.Create();
end;

procedure RegisterMachineResponse.FreeObjectProperties();
begin
  if Assigned(FRegisterMachineResult) then
    FreeAndNil(FRegisterMachineResult);
  inherited FreeObjectProperties();
end;

function RegisterMachineResponse.wstHas_RegisterMachineResult(): boolean;
begin
  Result := (FRegisterMachineResult <> nil);
end;

{ GetLastInitialId_Type }

constructor GetLastInitialId_Type.Create();
begin
  inherited Create();
  Frequest := LastInitialIdRequest.Create();
end;

procedure GetLastInitialId_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function GetLastInitialId_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ GetLastInitialIdResponse }

constructor GetLastInitialIdResponse.Create();
begin
  inherited Create();
  FGetLastInitialIdResult := LastInitialIdResponse.Create();
end;

procedure GetLastInitialIdResponse.FreeObjectProperties();
begin
  if Assigned(FGetLastInitialIdResult) then
    FreeAndNil(FGetLastInitialIdResult);
  inherited FreeObjectProperties();
end;

function GetLastInitialIdResponse.wstHas_GetLastInitialIdResult(): boolean;
begin
  Result := (FGetLastInitialIdResult <> nil);
end;

{ InitialPos }

constructor InitialPos.Create();
begin
  inherited Create();
  Frequest := InitialClientRequest.Create();
end;

procedure InitialPos.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function InitialPos.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ InitialPosResponse }

constructor InitialPosResponse.Create();
begin
  inherited Create();
  FInitialPosResult := InitialClientResponse.Create();
end;

procedure InitialPosResponse.FreeObjectProperties();
begin
  if Assigned(FInitialPosResult) then
    FreeAndNil(FInitialPosResult);
  inherited FreeObjectProperties();
end;

function InitialPosResponse.wstHas_InitialPosResult(): boolean;
begin
  Result := (FInitialPosResult <> nil);
end;

{ GetCommandTasks_Type }

constructor GetCommandTasks_Type.Create();
begin
  inherited Create();
  Frequest := GetCommandRequest.Create();
end;

procedure GetCommandTasks_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function GetCommandTasks_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ GetCommandTasksResponse }

constructor GetCommandTasksResponse.Create();
begin
  inherited Create();
  FGetCommandTasksResult := GetCommandResponse.Create();
end;

procedure GetCommandTasksResponse.FreeObjectProperties();
begin
  if Assigned(FGetCommandTasksResult) then
    FreeAndNil(FGetCommandTasksResult);
  inherited FreeObjectProperties();
end;

function GetCommandTasksResponse.wstHas_GetCommandTasksResult(): boolean;
begin
  Result := (FGetCommandTasksResult <> nil);
end;

{ GetUpdateTasks_Type }

constructor GetUpdateTasks_Type.Create();
begin
  inherited Create();
  Frequest := GetUpdateRequest.Create();
end;

procedure GetUpdateTasks_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function GetUpdateTasks_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ GetUpdateTasksResponse }

constructor GetUpdateTasksResponse.Create();
begin
  inherited Create();
  FGetUpdateTasksResult := GetUpdateResponse.Create();
end;

procedure GetUpdateTasksResponse.FreeObjectProperties();
begin
  if Assigned(FGetUpdateTasksResult) then
    FreeAndNil(FGetUpdateTasksResult);
  inherited FreeObjectProperties();
end;

function GetUpdateTasksResponse.wstHas_GetUpdateTasksResult(): boolean;
begin
  Result := (FGetUpdateTasksResult <> nil);
end;

{ GetClientVersion_Type }

constructor GetClientVersion_Type.Create();
begin
  inherited Create();
  Frequest := GetClientVerRequest.Create();
end;

procedure GetClientVersion_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function GetClientVersion_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ GetClientVersionResponse }

constructor GetClientVersionResponse.Create();
begin
  inherited Create();
  FGetClientVersionResult := GetClientVerResponse.Create();
end;

procedure GetClientVersionResponse.FreeObjectProperties();
begin
  if Assigned(FGetClientVersionResult) then
    FreeAndNil(FGetClientVersionResult);
  inherited FreeObjectProperties();
end;

function GetClientVersionResponse.wstHas_GetClientVersionResult(): boolean;
begin
  Result := (FGetClientVersionResult <> nil);
end;

{ UploadData_Type }

constructor UploadData_Type.Create();
begin
  inherited Create();
  Frequest := UploadRequest.Create();
end;

procedure UploadData_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function UploadData_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ UploadDataResponse }

constructor UploadDataResponse.Create();
begin
  inherited Create();
  FUploadDataResult := UploadResponse.Create();
end;

procedure UploadDataResponse.FreeObjectProperties();
begin
  if Assigned(FUploadDataResult) then
    FreeAndNil(FUploadDataResult);
  inherited FreeObjectProperties();
end;

function UploadDataResponse.wstHas_UploadDataResult(): boolean;
begin
  Result := (FUploadDataResult <> nil);
end;

{ GetCurrentTime_Type }

constructor GetCurrentTime_Type.Create();
begin
  inherited Create();
  Frequest := GetCurTimeRequest.Create();
end;

procedure GetCurrentTime_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function GetCurrentTime_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ GetCurrentTimeResponse }

constructor GetCurrentTimeResponse.Create();
begin
  inherited Create();
  FGetCurrentTimeResult := GetCurTimeResponse.Create();
end;

procedure GetCurrentTimeResponse.FreeObjectProperties();
begin
  if Assigned(FGetCurrentTimeResult) then
    FreeAndNil(FGetCurrentTimeResult);
  inherited FreeObjectProperties();
end;

function GetCurrentTimeResponse.wstHas_GetCurrentTimeResult(): boolean;
begin
  Result := (FGetCurrentTimeResult <> nil);
end;

{ QueryDataForSql }

constructor QueryDataForSql.Create();
begin
  inherited Create();
  Frequest := QueryDataRequest.Create();
end;

procedure QueryDataForSql.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function QueryDataForSql.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ QueryDataForSqlResponse }

constructor QueryDataForSqlResponse.Create();
begin
  inherited Create();
  FQueryDataForSqlResult := QueryDataResponse.Create();
end;

procedure QueryDataForSqlResponse.FreeObjectProperties();
begin
  if Assigned(FQueryDataForSqlResult) then
    FreeAndNil(FQueryDataForSqlResult);
  inherited FreeObjectProperties();
end;

function QueryDataForSqlResponse.wstHas_QueryDataForSqlResult(): boolean;
begin
  Result := (FQueryDataForSqlResult <> nil);
end;

{ UploadSaleData }

constructor UploadSaleData.Create();
begin
  inherited Create();
  Frequest := UploadShopSaleDataRequest.Create();
end;

procedure UploadSaleData.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function UploadSaleData.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ UploadSaleDataResponse }

constructor UploadSaleDataResponse.Create();
begin
  inherited Create();
  FUploadSaleDataResult := UploadShopSaleDataResponse.Create();
end;

procedure UploadSaleDataResponse.FreeObjectProperties();
begin
  if Assigned(FUploadSaleDataResult) then
    FreeAndNil(FUploadSaleDataResult);
  inherited FreeObjectProperties();
end;

function UploadSaleDataResponse.wstHas_UploadSaleDataResult(): boolean;
begin
  Result := (FUploadSaleDataResult <> nil);
end;

{ GetPrintBillInfo_Type }

constructor GetPrintBillInfo_Type.Create();
begin
  inherited Create();
  Frequest := GetBillListRequest.Create();
end;

procedure GetPrintBillInfo_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function GetPrintBillInfo_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ GetPrintBillInfoResponse }

constructor GetPrintBillInfoResponse.Create();
begin
  inherited Create();
  FGetPrintBillInfoResult := GetBillListResponse.Create();
end;

procedure GetPrintBillInfoResponse.FreeObjectProperties();
begin
  if Assigned(FGetPrintBillInfoResult) then
    FreeAndNil(FGetPrintBillInfoResult);
  inherited FreeObjectProperties();
end;

function GetPrintBillInfoResponse.wstHas_GetPrintBillInfoResult(): boolean;
begin
  Result := (FGetPrintBillInfoResult <> nil);
end;

{ GetPrintBillData_Type }

constructor GetPrintBillData_Type.Create();
begin
  inherited Create();
  Frequest := DownloadBillRequest.Create();
end;

procedure GetPrintBillData_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function GetPrintBillData_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ GetPrintBillDataResponse }

constructor GetPrintBillDataResponse.Create();
begin
  inherited Create();
  FGetPrintBillDataResult := DownloadBillResponse.Create();
end;

procedure GetPrintBillDataResponse.FreeObjectProperties();
begin
  if Assigned(FGetPrintBillDataResult) then
    FreeAndNil(FGetPrintBillDataResult);
  inherited FreeObjectProperties();
end;

function GetPrintBillDataResponse.wstHas_GetPrintBillDataResult(): boolean;
begin
  Result := (FGetPrintBillDataResult <> nil);
end;

{ UnifyUploadData_Type }

constructor UnifyUploadData_Type.Create();
begin
  inherited Create();
  Frequest := UploadDataRequest.Create();
end;

procedure UnifyUploadData_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function UnifyUploadData_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ UnifyUploadDataResponse }

constructor UnifyUploadDataResponse.Create();
begin
  inherited Create();
  FUnifyUploadDataResult := UploadDataResponse.Create();
end;

procedure UnifyUploadDataResponse.FreeObjectProperties();
begin
  if Assigned(FUnifyUploadDataResult) then
    FreeAndNil(FUnifyUploadDataResult);
  inherited FreeObjectProperties();
end;

function UnifyUploadDataResponse.wstHas_UnifyUploadDataResult(): boolean;
begin
  Result := (FUnifyUploadDataResult <> nil);
end;

{ ProcessExceptionData_Type }

constructor ProcessExceptionData_Type.Create();
begin
  inherited Create();
  Frequest := ProcExceptDataRequest.Create();
end;

procedure ProcessExceptionData_Type.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function ProcessExceptionData_Type.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ ProcessExceptionDataResponse }

constructor ProcessExceptionDataResponse.Create();
begin
  inherited Create();
  FProcessExceptionDataResult := ProcExceptDataResponse.Create();
end;

procedure ProcessExceptionDataResponse.FreeObjectProperties();
begin
  if Assigned(FProcessExceptionDataResult) then
    FreeAndNil(FProcessExceptionDataResult);
  inherited FreeObjectProperties();
end;

function ProcessExceptionDataResponse.wstHas_ProcessExceptionDataResult(): boolean;
begin
  Result := (FProcessExceptionDataResult <> nil);
end;

{ GetLabelForPrint }

constructor GetLabelForPrint.Create();
begin
  inherited Create();
  Frequest := GetLabelDataRequest.Create();
end;

procedure GetLabelForPrint.FreeObjectProperties();
begin
  if Assigned(Frequest) then
    FreeAndNil(Frequest);
  inherited FreeObjectProperties();
end;

function GetLabelForPrint.wstHas_request(): boolean;
begin
  Result := (Frequest <> nil);
end;

{ GetLabelForPrintResponse }

constructor GetLabelForPrintResponse.Create();
begin
  inherited Create();
  FGetLabelForPrintResult := GetLabelDataResponse.Create();
end;

procedure GetLabelForPrintResponse.FreeObjectProperties();
begin
  if Assigned(FGetLabelForPrintResult) then
    FreeAndNil(FGetLabelForPrintResult);
  inherited FreeObjectProperties();
end;

function GetLabelForPrintResponse.wstHas_GetLabelForPrintResult(): boolean;
begin
  Result := (FGetLabelForPrintResult <> nil);
end;

function BaseRequest.wstHas_RequestId(): boolean;
begin
  Result := (FRequestId <> '');
end;

function BaseRequest.wstHas_CompanyCode(): boolean;
begin
  Result := (FCompanyCode <> '');
end;

function BaseRequest.wstHas_ShopCode(): boolean;
begin
  Result := (FShopCode <> '');
end;

function BaseRequest.wstHas_MacAddr(): boolean;
begin
  Result := (FMacAddr <> '');
end;

function BaseRequest.wstHas_TerminalNo(): boolean;
begin
  Result := (FTerminalNo <> '');
end;

function BaseRequest.wstHas_AuthCode(): boolean;
begin
  Result := (FAuthCode <> '');
end;

function RegisterRequest.wstHas_MUUID(): boolean;
begin
  Result := (FMUUID <> '');
end;

function RegisterRequest.wstHas_License(): boolean;
begin
  Result := (FLicense <> '');
end;

function BaseResponse.wstHas_ResponseMsg(): boolean;
begin
  Result := (FResponseMsg <> '');
end;

function RegisterResponse.wstHas_ShopCode(): boolean;
begin
  Result := (FShopCode <> '');
end;

function RegisterResponse.wstHas_CompanyCode(): boolean;
begin
  Result := (FCompanyCode <> '');
end;

function RegisterResponse.wstHas_RegisterCode(): boolean;
begin
  Result := (FRegisterCode <> '');
end;

function RegisterResponse.wstHas_MUUID(): boolean;
begin
  Result := (FMUUID <> '');
end;

function LastInitialIdResponse.wstHas_Step(): boolean;
begin
  Result := (FStep <> '');
end;

{ InitialClientResponse }

constructor InitialClientResponse.Create();
begin
  inherited Create();
  FClientFile := InitialClientFile.Create();
end;

procedure InitialClientResponse.FreeObjectProperties();
begin
  if Assigned(FClientFile) then
    FreeAndNil(FClientFile);
  inherited FreeObjectProperties();
end;

function InitialClientResponse.wstHas_ClientFile(): boolean;
begin
  Result := (FClientFile <> nil);
end;

function InitialClientFile.wstHas_FileUrl(): boolean;
begin
  Result := (FFileUrl <> '');
end;

{ GetCommandResponse }

constructor GetCommandResponse.Create();
begin
  inherited Create();
  FTasks := ArrayOfCommandTask.Create();
end;

procedure GetCommandResponse.FreeObjectProperties();
begin
  if Assigned(FTasks) then
    FreeAndNil(FTasks);
  inherited FreeObjectProperties();
end;

function GetCommandResponse.wstHas_Tasks(): boolean;
begin
  Result := (FTasks <> ArrayOfCommandTask(0));
end;

{ CommandTask }

constructor CommandTask.Create();
begin
  inherited Create();
  FCreateDate := TDateTimeRemotable.Create();
end;

procedure CommandTask.FreeObjectProperties();
begin
  if Assigned(FCreateDate) then
    FreeAndNil(FCreateDate);
  inherited FreeObjectProperties();
end;

function CommandTask.wstHas_CmdInfo(): boolean;
begin
  Result := (FCmdInfo <> '');
end;

{ GetUpdateResponse }

constructor GetUpdateResponse.Create();
begin
  inherited Create();
  FTasks := ArrayOfDownloadTask.Create();
end;

procedure GetUpdateResponse.FreeObjectProperties();
begin
  if Assigned(FTasks) then
    FreeAndNil(FTasks);
  inherited FreeObjectProperties();
end;

function GetUpdateResponse.wstHas_Tasks(): boolean;
begin
  Result := (FTasks <> ArrayOfDownloadTask(0));
end;

function DownloadTask.wstHas_ExecSql(): boolean;
begin
  Result := (FExecSql <> '');
end;

{ GetClientVerResponse }

constructor GetClientVerResponse.Create();
begin
  inherited Create();
  FClientVersion := PosService.ClientVersion.Create();
end;

procedure GetClientVerResponse.FreeObjectProperties();
begin
  if Assigned(FClientVersion) then
    FreeAndNil(FClientVersion);
  inherited FreeObjectProperties();
end;

function GetClientVerResponse.wstHas_ClientVersion(): boolean;
begin
  Result := (FClientVersion <> nil);
end;

function ClientVersion.wstHas_VerNo(): boolean;
begin
  Result := (FVerNo <> '');
end;

function ClientVersion.wstHas_VerDesc(): boolean;
begin
  Result := (FVerDesc <> '');
end;

function ClientVersion.wstHas_UpdateFileUrl(): boolean;
begin
  Result := (FUpdateFileUrl <> '');
end;

function UploadRequest.wstHas_ExecSql(): boolean;
begin
  Result := (FExecSql <> '');
end;

function GetCurTimeResponse.wstHas_CurrentTime(): boolean;
begin
  Result := (FCurrentTime <> '');
end;

function QueryDataRequest.wstHas_SearchKey(): boolean;
begin
  Result := (FSearchKey <> '');
end;

function QueryDataResponse.wstHas_ExecSql(): boolean;
begin
  Result := (FExecSql <> '');
end;

function UploadShopSaleDataRequest.wstHas_UUID(): boolean;
begin
  Result := (FUUID <> '');
end;

function UploadShopSaleDataRequest.wstHas_ExecSql(): boolean;
begin
  Result := (FExecSql <> '');
end;

function UploadShopSaleDataResponse.wstHas_Msg(): boolean;
begin
  Result := (FMsg <> '');
end;

function GetBillListRequest.wstHas_OrderNo(): boolean;
begin
  Result := (FOrderNo <> '');
end;

function GetBillListRequest.wstHas_BeginDateTime(): boolean;
begin
  Result := (FBeginDateTime <> '');
end;

function GetBillListRequest.wstHas_EndDateTime(): boolean;
begin
  Result := (FEndDateTime <> '');
end;

{ GetBillListResponse }

constructor GetBillListResponse.Create();
begin
  inherited Create();
  FPrintList := ArrayOfPrintBillInfo.Create();
end;

procedure GetBillListResponse.FreeObjectProperties();
begin
  if Assigned(FPrintList) then
    FreeAndNil(FPrintList);
  inherited FreeObjectProperties();
end;

function GetBillListResponse.wstHas_PrintList(): boolean;
begin
  Result := (FPrintList <> ArrayOfPrintBillInfo(0));
end;

function PrintBillInfo.wstHas_BillPId(): boolean;
begin
  Result := (FBillPId <> '');
end;

function PrintBillInfo.wstHas_OrderNo(): boolean;
begin
  Result := (FOrderNo <> '');
end;

function PrintBillInfo.wstHas_OrderTime(): boolean;
begin
  Result := (FOrderTime <> '');
end;

function PrintBillInfo.wstHas_BillTypeName(): boolean;
begin
  Result := (FBillTypeName <> '');
end;

function DownloadBillRequest.wstHas_BillPId(): boolean;
begin
  Result := (FBillPId <> '');
end;

{ DownloadBillResponse }

constructor DownloadBillResponse.Create();
begin
  inherited Create();
  FPosPrintList := ArrayOfPrintBillData.Create();
end;

procedure DownloadBillResponse.FreeObjectProperties();
begin
  if Assigned(FPosPrintList) then
    FreeAndNil(FPosPrintList);
  inherited FreeObjectProperties();
end;

function DownloadBillResponse.wstHas_PosPrintList(): boolean;
begin
  Result := (FPosPrintList <> ArrayOfPrintBillData(0));
end;

function PrintBillData.wstHas_BillPId(): boolean;
begin
  Result := (FBillPId <> '');
end;

function PrintBillData.wstHas_OrderNo(): boolean;
begin
  Result := (FOrderNo <> '');
end;

function PrintBillData.wstHas_PTContent(): boolean;
begin
  Result := (FPTContent <> '');
end;

function UploadDataRequest.wstHas_Id(): boolean;
begin
  Result := (FId <> '');
end;

function UploadDataRequest.wstHas_ExecSql(): boolean;
begin
  Result := (FExecSql <> '');
end;

function GetLabelDataRequest.wstHas_SortCode(): boolean;
begin
  Result := (FSortCode <> '');
end;

function GetLabelDataResponse.wstHas_ResponseValue(): boolean;
begin
  Result := (FResponseValue <> '');
end;

{ ArrayOfCommandTask }

function ArrayOfCommandTask.GetItem(AIndex: integer): CommandTask;
begin
  Result := CommandTask(inherited GetItem(AIndex));
end;

class function ArrayOfCommandTask.GetItemClass(): TBaseRemotableClass;
begin
  Result := CommandTask;
end;

function ArrayOfCommandTask.Add(): CommandTask;
begin
  Result := CommandTask(inherited Add());
end;

function ArrayOfCommandTask.AddAt(const APosition: integer): CommandTask;
begin
  Result := CommandTask(inherited AddAt(APosition));
end;

{ ArrayOfDownloadTask }

function ArrayOfDownloadTask.GetItem(AIndex: integer): DownloadTask;
begin
  Result := DownloadTask(inherited GetItem(AIndex));
end;

class function ArrayOfDownloadTask.GetItemClass(): TBaseRemotableClass;
begin
  Result := DownloadTask;
end;

function ArrayOfDownloadTask.Add(): DownloadTask;
begin
  Result := DownloadTask(inherited Add());
end;

function ArrayOfDownloadTask.AddAt(const APosition: integer): DownloadTask;
begin
  Result := DownloadTask(inherited AddAt(APosition));
end;

{ ArrayOfPrintBillInfo }

function ArrayOfPrintBillInfo.GetItem(AIndex: integer): PrintBillInfo;
begin
  Result := PrintBillInfo(inherited GetItem(AIndex));
end;

class function ArrayOfPrintBillInfo.GetItemClass(): TBaseRemotableClass;
begin
  Result := PrintBillInfo;
end;

function ArrayOfPrintBillInfo.Add(): PrintBillInfo;
begin
  Result := PrintBillInfo(inherited Add());
end;

function ArrayOfPrintBillInfo.AddAt(const APosition: integer): PrintBillInfo;
begin
  Result := PrintBillInfo(inherited AddAt(APosition));
end;

{ ArrayOfPrintBillData }

function ArrayOfPrintBillData.GetItem(AIndex: integer): PrintBillData;
begin
  Result := PrintBillData(inherited GetItem(AIndex));
end;

class function ArrayOfPrintBillData.GetItemClass(): TBaseRemotableClass;
begin
  Result := PrintBillData;
end;

function ArrayOfPrintBillData.Add(): PrintBillData;
begin
  Result := PrintBillData(inherited Add());
end;

function ArrayOfPrintBillData.AddAt(const APosition: integer): PrintBillData;
begin
  Result := PrintBillData(inherited AddAt(APosition));
end;


procedure Register_PosService_ServiceMetadata();
var
  mm: IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetRepositoryCustomData(sUNIT_NAME, 'elementFormDefault', 'qualified');
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'TRANSPORT_Address',
    'http://192.168.111.115:8988/PosService.asmx'
    );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'FORMAT_Style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'RegisterMachine',
    '_E_N_',
    'RegisterMachine'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'RegisterMachine',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'RegisterMachine',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/RegisterMachine'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'RegisterMachine',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'RegisterMachine',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetLastInitialId',
    '_E_N_',
    'GetLastInitialId'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetLastInitialId',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetLastInitialId',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/GetLastInitialId'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetLastInitialId',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetLastInitialId',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'InitialClient',
    '_E_N_',
    'InitialClient'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'InitialClient',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'InitialClient',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/InitialPos'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'InitialClient',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'InitialClient',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetCommandTasks',
    '_E_N_',
    'GetCommandTasks'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetCommandTasks',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetCommandTasks',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/GetCommandTasks'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetCommandTasks',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetCommandTasks',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetUpdateTasks',
    '_E_N_',
    'GetUpdateTasks'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetUpdateTasks',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetUpdateTasks',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/GetUpdateTasks'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetUpdateTasks',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetUpdateTasks',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetClientVersion',
    '_E_N_',
    'GetClientVersion'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetClientVersion',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetClientVersion',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/GetClientVersion'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetClientVersion',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetClientVersion',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UploadData',
    '_E_N_',
    'UploadData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UploadData',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UploadData',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/UploadData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UploadData',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UploadData',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetCurrentTime',
    '_E_N_',
    'GetCurrentTime'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetCurrentTime',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetCurrentTime',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/GetCurrentTime'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetCurrentTime',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetCurrentTime',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'QueryData',
    '_E_N_',
    'QueryData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'QueryData',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'QueryData',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/QueryDataForSql'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'QueryData',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'QueryData',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UploadShopSaleData',
    '_E_N_',
    'UploadShopSaleData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UploadShopSaleData',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UploadShopSaleData',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/UploadSaleData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UploadShopSaleData',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UploadShopSaleData',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetPrintBillInfo',
    '_E_N_',
    'GetPrintBillInfo'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetPrintBillInfo',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetPrintBillInfo',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/GetPrintBillInfo'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetPrintBillInfo',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetPrintBillInfo',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetPrintBillData',
    '_E_N_',
    'GetPrintBillData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetPrintBillData',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetPrintBillData',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/GetPrintBillData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetPrintBillData',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetPrintBillData',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UnifyUploadData',
    '_E_N_',
    'UnifyUploadData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UnifyUploadData',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UnifyUploadData',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/UnifyUploadData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UnifyUploadData',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'UnifyUploadData',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'ProcessExceptionData',
    '_E_N_',
    'ProcessExceptionData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'ProcessExceptionData',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'ProcessExceptionData',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/ProcessExceptionData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'ProcessExceptionData',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'ProcessExceptionData',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetLabelData',
    '_E_N_',
    'GetLabelData'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetLabelData',
    'style',
    'document'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetLabelData',
    'TRANSPORT_soapAction',
    'http://www.meiyijia.com.cn/GetLabelForPrint'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetLabelData',
    'FORMAT_Input_EncodingStyle',
    'literal'
    );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'PosServiceSoap',
    'GetLabelData',
    'FORMAT_OutputEncodingStyle',
    'literal'
    );
end;


var
  typeRegistryInstance: TTypeRegistry = nil;

initialization
  typeRegistryInstance := GetTypeRegistry();

  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(ResponseResult), 'ResponseResult');
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(RegisterMachine_Type), 'RegisterMachine', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(RegisterMachineResponse), 'RegisterMachineResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetLastInitialId_Type), 'GetLastInitialId', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetLastInitialIdResponse), 'GetLastInitialIdResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(InitialPos), 'InitialPos', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(InitialPosResponse), 'InitialPosResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetCommandTasks_Type), 'GetCommandTasks', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetCommandTasksResponse), 'GetCommandTasksResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetUpdateTasks_Type), 'GetUpdateTasks', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetUpdateTasksResponse), 'GetUpdateTasksResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetClientVersion_Type), 'GetClientVersion', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetClientVersionResponse), 'GetClientVersionResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UploadData_Type), 'UploadData', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UploadDataResponse), 'UploadDataResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetCurrentTime_Type), 'GetCurrentTime', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetCurrentTimeResponse), 'GetCurrentTimeResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(QueryDataForSql), 'QueryDataForSql', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(QueryDataForSqlResponse), 'QueryDataForSqlResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UploadSaleData), 'UploadSaleData', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UploadSaleDataResponse), 'UploadSaleDataResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetPrintBillInfo_Type), 'GetPrintBillInfo', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetPrintBillInfoResponse), 'GetPrintBillInfoResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetPrintBillData_Type), 'GetPrintBillData', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetPrintBillDataResponse), 'GetPrintBillDataResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UnifyUploadData_Type), 'UnifyUploadData', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UnifyUploadDataResponse), 'UnifyUploadDataResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(ProcessExceptionData_Type), 'ProcessExceptionData', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(ProcessExceptionDataResponse), 'ProcessExceptionDataResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetLabelForPrint), 'GetLabelForPrint', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetLabelForPrintResponse), 'GetLabelForPrintResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(BaseRequest), 'BaseRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(RegisterRequest), 'RegisterRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(BaseResponse), 'BaseResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(RegisterResponse), 'RegisterResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(LastInitialIdRequest), 'LastInitialIdRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(LastInitialIdResponse), 'LastInitialIdResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(InitialClientRequest), 'InitialClientRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(InitialClientResponse), 'InitialClientResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(InitialClientFile), 'InitialClientFile', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetCommandRequest), 'GetCommandRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetCommandResponse), 'GetCommandResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(CommandTask), 'CommandTask', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetUpdateRequest), 'GetUpdateRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetUpdateResponse), 'GetUpdateResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(DownloadTask), 'DownloadTask', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetClientVerRequest), 'GetClientVerRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetClientVerResponse), 'GetClientVerResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(ClientVersion), 'ClientVersion', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UploadRequest), 'UploadRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UploadResponse), 'UploadResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetCurTimeRequest), 'GetCurTimeRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetCurTimeResponse), 'GetCurTimeResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(QueryDataRequest), 'QueryDataRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(QueryDataResponse), 'QueryDataResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UploadShopSaleDataRequest), 'UploadShopSaleDataRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UploadShopSaleDataResponse), 'UploadShopSaleDataResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetBillListRequest), 'GetBillListRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetBillListResponse), 'GetBillListResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(PrintBillInfo), 'PrintBillInfo', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(DownloadBillRequest), 'DownloadBillRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(DownloadBillResponse), 'DownloadBillResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(PrintBillData), 'PrintBillData', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(UploadDataRequest), 'UploadDataRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(ProcExceptDataRequest), 'ProcExceptDataRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(ProcExceptDataResponse), 'ProcExceptDataResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetLabelDataRequest), 'GetLabelDataRequest', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(GetLabelDataResponse), 'GetLabelDataResponse', [trioqualifiedElement]);
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(ArrayOfCommandTask), 'ArrayOfCommandTask');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(ArrayOfCommandTask)].RegisterExternalPropertyName(sARRAY_ITEM, 'CommandTask');
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(ArrayOfDownloadTask), 'ArrayOfDownloadTask');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(ArrayOfDownloadTask)].RegisterExternalPropertyName(sARRAY_ITEM, 'DownloadTask');
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(ArrayOfPrintBillInfo), 'ArrayOfPrintBillInfo');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(ArrayOfPrintBillInfo)].RegisterExternalPropertyName(sARRAY_ITEM, 'PrintBillInfo');
  typeRegistryInstance.Register(sNAME_SPACE, TypeInfo(ArrayOfPrintBillData), 'ArrayOfPrintBillData');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(ArrayOfPrintBillData)].RegisterExternalPropertyName(sARRAY_ITEM, 'PrintBillData');

end.






















