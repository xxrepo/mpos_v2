unit uRemoteService;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fpJson,
  JsonParser,
  SysUtils,
  PosService;

type


  //POS服务接口
  IRemoteService = interface(IInvokable)
    ['{495A824C-F16E-4D4E-8029-216325DD9743}']
    function IsSrvConnection(): boolean;
    function GetCurrentTime(): GetCurTimeResponse;
    function RegisterMachine(ShopCode, TermNo, TermID, License: string; iType: integer): RegisterResponse;
    function GetClientVersion(verId: integer): ClientVersion;
    function InitialClient(TskId, iInitType, iInitStep: integer): InitialClientResponse;
    function GetLastInitialId(): LastInitialIdResponse;
    function GetUpdateTasks(): GetUpdateResponse;
    function GetCommandTasks(): GetCommandResponse;
    function UploadData(uSQL: string): UploadResponse;
    function UploadShopSaleData(Uuids, uSQL: string): UploadShopSaleDataResponse;
    function UnifyUploadData(IDs, uSQL: string; iDataType: integer): UploadResponse;        //分类别上传数据
    function QueryData(DataType: integer; SearchKey: string): QueryDataResponse;
    function GetPosPrintBillInfo(OrderNO, BeginDate, EndDate: string): TJSONObject;
    function GetPosPrintBillData(OrderNO: string): TJSONObject;
    function GetLabelData(iDataType, iGDGID, iPageSize, iPageIndex: integer; vSortCode: string): GetLabelDataResponse;
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

end.
