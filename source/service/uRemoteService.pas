unit uRemoteService;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, fpJson, JsonParser;

type

  TResponseResult = (
    Success
    , ParamError
    , TerminalNotExist
    , SysUnregister
    , IdentiyException
    , AuthCodeError
    , Failure
    , SysException
    );

  TBaseResponse = class(TPersistent)
  private
    FResponseCode: TResponseResult;
    FResponseMsg: UnicodeString;
  published
    property ResponseCode: TResponseResult read FResponseCode write FResponseCode;
    property ResponseMsg: UnicodeString read FResponseMsg write FResponseMsg;
  end;


  TCurTimeResponse = class(TBaseResponse)
  private
    FCurrentTime: UnicodeString;
  published
    property CurrentTime: UnicodeString read FCurrentTime write FCurrentTime;
  end;

  TRegisterResponse = class(TBaseResponse)
  private
    FShopCode: UnicodeString;
    FCompanyCode: UnicodeString;
    FRegisterCode: UnicodeString;
    FMUUID: UnicodeString;
  published
    property ShopCode: UnicodeString read FShopCode write FShopCode;
    property CompanyCode: UnicodeString read FCompanyCode write FCompanyCode;
    property RegisterCode: UnicodeString read FRegisterCode write FRegisterCode;
    property MUUID: UnicodeString read FMUUID write FMUUID;
  end;

  TClientVersion = class(TBaseResponse)
  private
    FVerId: int64;
    FVerNo: UnicodeString;
    FVerDesc: UnicodeString;
    FIsMust: boolean;
    FUpdateFileUrl: UnicodeString;
  published
    property VerId: int64 read FVerId write FVerId;
    property VerNo: UnicodeString read FVerNo write FVerNo;
    property VerDesc: UnicodeString read FVerDesc write FVerDesc;
    property IsMust: boolean read FIsMust write FIsMust;
    property UpdateFileUrl: UnicodeString read FUpdateFileUrl write FUpdateFileUrl;
  end;

  TInitialClientFile = class
  private
    FDataType: integer;
    FFileUrl: UnicodeString;
  published
    property DataType: integer read FDataType write FDataType;
    property FileUrl: UnicodeString read FFileUrl write FFileUrl;
  end;

  { TInitialClientResponse }

  TInitialClientResponse = class(TBaseResponse)
  private
    FClientFile: TInitialClientFile;
  public
    constructor Create();
    destructor Destroy; override;
  published
    property ClientFile: TInitialClientFile read FClientFile write FClientFile;
  end;

  TLastInitialIdResponse = class(TBaseResponse)
  private
    FId: int64;
    FStep: UnicodeString;
  published
    property Id: int64 read FId write FId;
    property Step: UnicodeString read FStep write FStep;
  end;

  TUpdateTask = class(TObject)
  private
    FPId: int64;
    FFormatType: integer;
    FKeyId: int64;
    FExecSql: UnicodeString;
  published
    property PId: int64 read FPId write FPId;
    property FormatType: integer read FFormatType write FFormatType;
    property KeyId: int64 read FKeyId write FKeyId;
    property ExecSql: UnicodeString read FExecSql write FExecSql;
  end;

  TUpdateTaskList = class(TThreadList<TUpdateTask>);

  { TUpdateResponse }

  TUpdateResponse = class(TBaseResponse)
  private
    FIsHasTask: boolean;
    FTasks: TUpdateTaskList;
  public
    constructor Create();
    destructor Destroy; override;
  published
    property IsHasTask: boolean read FIsHasTask write FIsHasTask;
    property Tasks: TUpdateTaskList read FTasks write FTasks;
  end;

  TCommandTask = class(TObject)
  private
    FPid: int64;
    FCmdType: integer;
    FCmdInfo: UnicodeString;
    FCreateDate: TDatetime;
  published
    property Pid: int64 read FPid write FPid;
    property CmdType: integer read FCmdType write FCmdType;
    property CmdInfo: UnicodeString read FCmdInfo write FCmdInfo;
    property CreateDate: TDatetime read FCreateDate write FCreateDate;
  end;

  TCommandTaskList = class(TThreadList<TCommandTask>);

  { TCommandResponse }

  TCommandResponse = class(TBaseResponse)
  private
    FIsHasTask: boolean;
    FTasks: TCommandTaskList;
  public
    constructor Create();
    destructor Destroy; override;
  published
    property IsHasTask: boolean read FIsHasTask write FIsHasTask;
    property Tasks: TCommandTaskList read FTasks write FTasks;
  end;

  TUploadResponse = class(TBaseResponse)
  private
    FState: integer;
  published
    property State: integer read FState write FState;
  end;

  TUploadShopSaleDataResponse = class(TBaseResponse)
  private
    FState: integer;
    FMsg: UnicodeString;
  published
    property State: integer read FState write FState;
    property Msg: UnicodeString read FMsg write FMsg;
  end;

  TQueryDataResponse = class(TBaseResponse)
  private
    FDataState: integer;
    FExecSql: UnicodeString;
  published
    property DataState: integer read FDataState write FDataState;
    property ExecSql: UnicodeString read FExecSql write FExecSql;
  end;

  TLabelDataResponse = class(TBaseResponse)
  private
    FResponseValue: UnicodeString;
  published
    property ResponseValue: UnicodeString read FResponseValue write FResponseValue;
  end;

  //POS服务接口
  IRemoteService = interface(IInvokable)
    ['{C4B9ECDE-432E-4285-98BC-7CC6874DD30B}']
    function IsSrvConnection(): boolean;
    function GetCurrentTime(): TCurTimeResponse;
    function RegisterMachine(ShopCode, TermNo, TermID, License: string; iType: integer): TRegisterResponse;
    function GetClientVersion(verId: integer): TClientVersion;

    function InitialClient(TskId, iInitType, iInitStep: integer): TInitialClientResponse;
    function GetLastInitialId(): TLastInitialIdResponse;
    function GetUpdateTasks(): TUpdateResponse;
    function GetCommandTasks(): TCommandResponse;
    function UploadData(uSQL: string): TUploadResponse;
    function UploadShopSaleData(Uuids, uSQL: string): TUploadShopSaleDataResponse;
    function UnifyUploadData(IDs, uSQL: string; iDataType: integer): TUploadResponse;        //分类别上传数据

    function QueryData(DataType: integer; SearchKey: string): TQueryDataResponse;
    function GetPosPrintBillInfo(OrderNO, BeginDate, EndDate: string): TJSONObject;
    function GetPosPrintBillData(OrderNO: string): TJSONObject;
    function GetLabelData(iDataType, iGDGID, iPageSize, iPageIndex: integer; vSortCode: string): TLabelDataResponse;
    function GetMTicketInfo(Param: TStringList): TJSONObject;     //劵校验


    {
    //核销还需传入参数
    Param.Add('TicketNo=1061352154');  //劵号
    Param.Add('CustomerCode=0198');    //店号
    Param.Add(Format('%s=%s', ['TicketType', '1']));  劵类型 1，2，3= CardType_Goods,CardType_Money,CardType_Discount
    Param.Add('Products='+Param.Get('Products'));   //json   //提货劵的商品列表 json 格式
    Param.Add('FlowNo=X0000001');      //订单流水号
    Param.Add('PayAmt=6200');          //应付
    Param.Add('PreferentialAmt=0');    //优惠
    Param.Add('RealAmt=6200');         //实付
    }
    function DestoryMTicket(Param: TStringList; var rsStr: string): boolean;     //劵核销
    {
    参数
    list.Add('TicketNo=1211975711');        //劵号
    list.Add('CustomerCode=0198');          //店号
    list.Add('FlowNo=X0000002');            //原流水
    }
    function CancelMTicket(Param: TStringList; var rsStr: string): boolean; //劵冲正
    //function UploadInvStore(Param: TJSONObject): TJSONObject;
    //外卖单处理
    function TakeOutProcess(useMethod, url: string; Param: TStringList): TJSONObject;
  end;



implementation

{ TCommandResponse }

constructor TCommandResponse.Create();
begin
  inherited Create();
  FTasks := TCommandTaskList.Create;
end;

destructor TCommandResponse.Destroy;
begin
  if Assigned(FTasks) then
    FreeAndNil(FTasks);
  inherited Destroy;
end;

{ TUpdateResponse }

constructor TUpdateResponse.Create();
begin
  inherited Create();
  FTasks := TUpdateTaskList.Create;
end;

destructor TUpdateResponse.Destroy;
begin
  if Assigned(FTasks) then
    FreeAndNil(FTasks);
  inherited Destroy;
end;



constructor TInitialClientResponse.Create();
begin
  inherited Create();
  FClientFile := TInitialClientFile.Create;
end;

destructor TInitialClientResponse.Destroy;
begin
  if Assigned(FClientFile) then
    FreeAndNil(FClientFile);
  inherited Destroy;
end;


end.
