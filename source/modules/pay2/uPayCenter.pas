unit uPayCenter;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  Generics.Collections,
  cm_interfaces, cm_messager, cm_parameter, cm_sysutils, uCashPay, cm_Plat,
  uPay, uPayPO, uPayDAO, uDAO,
  uPayRecordImpl, uPayAssignDAOImpl, uPayInfoImpl, uPayTypeImpl;

type

  { TPOSPayDeal }

  TPOSPayDeal = class(TCMMessageable, IPayCenter, IPayServiceRegister)
  private
    FServiceList: TInterfaceList;
    FTypeInfoList: TPayTypeInfoList;
    FRecordDAO: IPayRecordDAO;
    FAssignDAO: IPayAssignDAO;
    FInfoDAO: IPayInfoDAO;
    FTypeDAO: IPayTypeDAO;
    FPayBoard: IPayBoard;
  public
    constructor Create;
    destructor Destroy; override;
    //TODO 配置
  private //service
    function PaySelectsDefinitely(AAmount: currency; out thePayCode: string; out theAmount: currency): boolean;
    function GetPayService(const APayCode: string): IPayService;
    function GetPayedAmount(const AOrderUUID: string): currency;
    function CallPay(const APayCode, AAssignUUID: string; AAmount: currency; AResuestParameter: ICMConstantParameterDataList;
      out theAmount: currency): boolean;
    function CreatePayResponse(const APayUUID, ARemark: string; AAmount: currency; APayMsg: ICMConstantParameterDataList): IPayResponse;
  private //db
    function SavePayRecord(const APayUUID, AOrderUUID: string; AAmount: currency): boolean;
    function SavePayAssign(const AAssignUUID, APayUUID: string; AAmount: currency): boolean;
    function SavePayInfo(const AAssignUUID, AServiceUUID: string; AAmount: currency; AType: byte; ARemark: string): boolean;
  public //IPayCenter
    function Pay(APayRequest: IPayRequest): boolean; overload;
    function Pay(APayRequest: IPayRequest; out thePayResponse: IPayResponse): boolean; overload;
    function Pay(const APayCode: string; APayRequest: IPayRequest; out thePayResponse: IPayResponse): boolean; overload;
    function QueryPay(const APayUUID: string): IPayResponse;
    function QueryOrder(const AOrderUUID: string): TPayResponseList;
    function GetPayTypeList: TPayTypeInfoList;
  public //IPayServiceRegister
    function AddService(APayService: IPayService): boolean;
  end;


  { TPayResponse }

  TPayResponse = class(TCMBase, IPayResponse)
  private
    FPayUUID: string;
    FPayAmount: currency;
    FPayRemark: string;
    FPayMsg: ICMConstantParameterDataList;
  public
    constructor Create(const AUUID: string; AAmount: currency);
  public
    function GetPayUUID: string;
    function GetPayAmount: currency;
    function GetPayRemark: string;
    function GetPayMsg: ICMConstantParameterDataList;
  end;

  { TPayServiceRequest }

  TPayServiceRequest = class(TCMBase, IPayServiceRequest)
  private
    FPayUUID: string;
    FPayAmount: currency;
    FPayParameter: ICMConstantParameterDataList;
  public
    constructor Create(AAmount: currency);
  public
    function GetPayUUID: string;
    function GetPayAmount: currency;
    function GetPayParameter: ICMConstantParameterDataList;
  end;

implementation

uses uSystem;

{ TPayServiceRequest }

constructor TPayServiceRequest.Create(AAmount: currency);
begin
  inherited Create;
  FPayUUID := CreateGUIDStr;
  FPayAmount := AAmount;
  FPayParameter := nil;
end;

function TPayServiceRequest.GetPayUUID: string;
begin
  Result := FPayUUID;
end;

function TPayServiceRequest.GetPayAmount: currency;
begin
  Result := FPayAmount;
end;

function TPayServiceRequest.GetPayParameter: ICMConstantParameterDataList;
begin
  Result := FPayParameter;
end;

{ TPayResponse }

constructor TPayResponse.Create(const AUUID: string; AAmount: currency);
begin
  inherited Create;
  FPayUUID := AUUID;
  FPayAmount := AAmount;
  FPayRemark := '';
  FPayMsg := nil;
end;

function TPayResponse.GetPayUUID: string;
begin
  Result := FPayUUID;
end;

function TPayResponse.GetPayAmount: currency;
begin
  Result := FPayAmount;
end;

function TPayResponse.GetPayRemark: string;
begin
  Result := FPayRemark;
end;

function TPayResponse.GetPayMsg: ICMConstantParameterDataList;
begin
  Result := FPayMsg;
end;

{ TPOSPayDeal }

constructor TPOSPayDeal.Create;
begin
  inherited Create;
  FServiceList := TInterfaceList.Create;
  FTypeInfoList := TPayTypeInfoList.Create;

  TPOSDAOFactory.GetInstance.OutDAO(TPayRecordDAO, IPayRecordDAO, FRecordDAO);
  TPOSDAOFactory.GetInstance.OutDAO(TPayAssignDAO, IPayAssignDAO, FAssignDAO);
  TPOSDAOFactory.GetInstance.OutDAO(TPayInfoDAO, IPayInfoDAO, FInfoDAO);
  TPOSDAOFactory.GetInstance.OutDAO(TPayTypeDAO, IPayTypeDAO, FTypeDAO);
end;

destructor TPOSPayDeal.Destroy;
begin
  FServiceList.Free;
  FTypeInfoList.Free;
  FRecordDAO := nil;
  FAssignDAO := nil;
  FInfoDAO := nil;
  FTypeDAO := nil;
  FPayBoard := nil;
  inherited Destroy;
end;

//TODO
function TPOSPayDeal.PaySelectsDefinitely(AAmount: currency; out thePayCode: string; out theAmount: currency): boolean;
var
  s: string;
begin
  Result := False;
  Messager.Debug('PaySelectsDefinitely Begin 支付类型选定');
  //处理默认支付类型 TODO
  //thePayCode := 'CashPay';

  if InterfaceRegister.OutInterface(IPayBoard, FPayBoard) then
    Result := FPayBoard.SelectsDefinitely(AAmount, thePayCode, theAmount);
  Messager.Debug('PaySelectsDefinitely End 支付类型选定');
end;

function TPOSPayDeal.GetPayService(const APayCode: string): IPayService;
begin
  Result := nil;
  //TODO get pay service
  //test
  Messager.Debug('FServiceList.Count:%d', [FServiceList.Count]);

  if FServiceList.Count > 0 then
  begin
    Result := IPayService(FServiceList.Items[0]);
  end;
end;

//TODO
function TPOSPayDeal.GetPayedAmount(const AOrderUUID: string): currency;
begin
  Result := FRecordDAO.GetPayedAmountByOrderUUID(AOrderUUID);
end;

function TPOSPayDeal.Pay(APayRequest: IPayRequest): boolean;
var
  pr: IPayResponse;
begin
  Result := Pay(APayRequest, pr);
end;

function TPOSPayDeal.SavePayRecord(const APayUUID, AOrderUUID: string; AAmount: currency): boolean;
var
  payRec: TPayRecordPO;
begin
  Result := False;
  Messager.Debug('保存支付请求记录...');
  payRec := TPayRecordPO.Create;
  try
    payRec.PayUUID := APayUUID;
    payRec.OrderUUID := AOrderUUID;
    payRec.Amount := AAmount;
    payRec.Time_ := now();
    payRec.Remark := '';
    Result := FRecordDAO.savePayRecord(payRec);
  finally
    payrec.Free;
  end;
end;

function TPOSPayDeal.SavePayAssign(const AAssignUUID, APayUUID: string; AAmount: currency): boolean;
var
  payass: TPayAssignPO;
begin
  Result := False;
  Messager.Debug('保存支付业务分派记录...');
  payass := TPayAssignPO.Create;
  try
    payass.AssignUUID := AAssignUUID;
    payass.PayUUID := APayUUID;
    payass.Amount := AAmount;
    payass.Time_ := now();
    payass.Remark := '';
    Result := FAssignDAO.savePayAssign(payass);
  finally
    payass.Free;
  end;
end;

function TPOSPayDeal.SavePayInfo(const AAssignUUID, AServiceUUID: string; AAmount: currency; AType: byte; ARemark: string): boolean;
var
  payInfo: TPayInfoPO;
begin
  Result := False;
  Messager.Debug('保存支付业务结果信息记录...');
  payInfo := TPayInfoPO.Create;
  try
    payInfo.AssignUUID := AAssignUUID;
    payInfo.ServiceUUID := AServiceUUID;
    payInfo.Type_ := AType;
    payInfo.Amount := AAmount;
    payInfo.Time_ := now();
    payInfo.Remark := ARemark;
    Result := FInfoDAO.savePayInfo(payInfo);
  finally
    payInfo.Free;
  end;
end;

function TPOSPayDeal.CallPay(const APayCode, AAssignUUID: string; AAmount: currency; AResuestParameter: ICMConstantParameterDataList;
  out theAmount: currency): boolean;
var
  ps: IPayService;

  serviceResObj: TPayServiceRequest;
  serviceRes: IPayServiceRequest;
  serviceRspList: TPayServiceResponseList;

  i: integer;
  rsp: IPayServiceResponse;
  serviceRspListG: TList<IPayServiceResponse>;

  payInfo: TPayInfoPO;
begin
  Result := False;
  ps := GetPayService(APayCode);
  if not Assigned(ps) then
  begin
    Messager.Error('soso 2222');
    Exit;
  end;
  //准备支付业务请求信息
  serviceResObj := TPayServiceRequest.Create(AAmount);
  serviceResObj.FPayParameter := AResuestParameter;
  serviceRes := serviceResObj;
  if ps.ToPay(serviceRes, serviceRspList) then
  begin
    //save pay info
    Messager.Info('收到支付业务响应，开始记录支付信息...');
    serviceRspListG := serviceRspList.LockList;
    try
      for i := 0 to serviceRspListG.Count - 1 do
      begin
        rsp := serviceRspListG[i];

        if not SavePayInfo(AAssignUUID, rsp.GetServicePayUUID, rsp.GetPayAmount, rsp.GetPayType, rsp.GetPayRemark) then
        begin
          Exit;
        end;
        Messager.Debug('PayItem:%d -- %d, %.2f, %s', [i + 1, rsp.GetPayType, rsp.GetPayAmount, rsp.GetPayRemark]);

      end;
    finally
      serviceRspList.UnlockList;
    end;
    //响应
    Messager.Debug('call over 1');
    theAmount := serviceRspList.GetSumPayAmount;
    Result := True;
  end;
end;

function TPOSPayDeal.CreatePayResponse(const APayUUID, ARemark: string; AAmount: currency; APayMsg: ICMConstantParameterDataList): IPayResponse;
var
  payRspObj: TPayResponse;
begin
  Result := nil;
  Messager.Debug('CreatePayResponse()...');
  payRspObj := TPayResponse.Create(APayUUID, AAmount);
  payRspObj.FPayRemark := ARemark;
  payRspObj.FPayMsg := APayMsg;
  Result := payRspObj;
end;

function TPOSPayDeal.Pay(APayRequest: IPayRequest; out thePayResponse: IPayResponse): boolean;
var
  payUUID, assignUUID: string;
  payCode: string;
  payedAmount: currency;
  selPayAmount: currency;
  callAmount: currency;
  ps: IPayService;
begin
  Result := False;
  try
    payUUID := CreateGUIDStr;
    if not SavePayRecord(payUUID, APayRequest.GetOrderUUID, APayRequest.GetPayAmount) then
    begin
      //if Assigned(FPayBoard) or (InterfaceRegister.OutInterface(IPayBoard, FPayBoard)) then
      //  FPayBoard.SetPromptMessage(etError, '数据保存异常, 请联系管理员!');
      Messager.Error('保存支付记录失败。');
      Exit;
    end;

    //查询确认已支付金额
    payedAmount := GetPayedAmount(APayRequest.GetOrderUUID);
    if payedAmount >= APayRequest.GetPayAmount then
    begin
      thePayResponse := CreatePayResponse(payUUID, '已经支付了这么多了呀', payedAmount, nil);
      Messager.Debug('已支付完成, 不需要再支付');
      Result := True;
      Exit;
    end;

    while payedAmount < APayRequest.GetPayAmount do
    begin

      //支付方式选定
      if not PaySelectsDefinitely(APayRequest.GetPayAmount - payedAmount, payCode, selPayAmount) then
      begin
        //if Assigned(SaleBoard) or (InterfaceRegister.OutInterface(ISaleBoard, SaleBoard)) then
        //  FPayBoard.SetPromptMessage(etError, '数据保存异常, 请联系管理员!');

        Messager.Error('支付未完成情况下, 选择退出');
        thePayResponse := CreatePayResponse(payUUID, '已经支付了这么多了呀', payedAmount, nil);
        Exit;
      end;

      //根据选定支付方式, 获得支付服务
      ps := GetPayService(payCode);
      if not Assigned(ps) then
      begin
        Messager.Error('选定支付服务不存在, 不能进行支付!');
        AppSystem.GetMsgBox().ShowMessage('选定支付服务不存在, 不能进行支付!');
        thePayResponse := CreatePayResponse(payUUID, '已经支付了这么多了呀', payedAmount, nil);
        Exit;
      end;

      //创建并保存支付分派任务
      assignUUID := CreateGUIDStr;
      if not SavePayAssign(assignUUID, payUUID, selPayAmount) then
      begin
        Messager.Error('支付分派任务保存失败, 退出');
        thePayResponse := CreatePayResponse(payUUID, '已经支付了这么多了呀', payedAmount, nil);
        Exit;
      end;

      //呼叫支付服务处理业务
      if not CallPay(payCode, assignUUID, selPayAmount, APayRequest.GetPayParameter, callAmount) then
      begin
        Messager.Error('支付服务处理业务失败, 退出');
        thePayResponse := CreatePayResponse(payUUID, '已经支付了这么多了呀', payedAmount, nil);
        Exit;
      end;

      payedAmount := GetPayedAmount(APayRequest.GetOrderUUID);
    end;
    thePayResponse := CreatePayResponse(payUUID, '已经支付了这么多了呀', payedAmount, nil);
  except
    on e: Exception do
      Messager.Error('Pay: ', e);
  end;
end;

function TPOSPayDeal.Pay(const APayCode: string; APayRequest: IPayRequest; out thePayResponse: IPayResponse): boolean;
begin

end;

function TPOSPayDeal.QueryPay(const APayUUID: string): IPayResponse;
var
  PayedAmount: currency;
  PayResponse: TPayResponse;
begin
  Result := nil;

  try
    PayedAmount := FAssignDAO.GetPayedAmountByPayUUID(APayUUID);
    if PayedAmount > 0 then
    begin
      PayResponse := TPayResponse.Create(APayUUID, PayedAmount);
      PayResponse.FPayRemark := '';
      Result := IPayResponse(PayResponse);
    end;
  except
    on e: Exception do
      Messager.Error('QueryPay: ', e);
  end;
end;

function TPOSPayDeal.QueryOrder(const AOrderUUID: string): TPayResponseList;
var
  i: integer;
  PayResponse: TPayResponse;
  PayedAmount: currency;
  PayRecordList: TPayRecordPOList;
  PayRecordList_: TList<TPayRecordPO>;
begin
  Result := TPayResponseList.Create;
  try
    try
      PayRecordList := FRecordDAO.GetPayRecordList(AOrderUUID);
      PayRecordList_ := PayRecordList.LockList();
      for i := 0 to PayRecordList_.Count - 1 do
      begin
        PayedAmount := FAssignDAO.GetPayedAmountByPayUUID(PayRecordList_[i].PayUUID);
        if PayedAmount > 0 then
        begin
          PayResponse := TPayResponse.Create(PayRecordList_[i].PayUUID, PayedAmount);
          PayResponse.FPayRemark := '';
          //PayResponse.GetPayMsg.Get('').AsString;
          Result.Add(IPayResponse(PayResponse));
        end;
      end;
    except
      on e: Exception do
        Messager.Error('QueryOrder: ', e);
    end;
  finally
    PayRecordList.UnlockList;
  end;
end;

function TPOSPayDeal.GetPayTypeList: TPayTypeInfoList;
begin
  Result := FTypeInfoList;
end;

function TPOSPayDeal.AddService(APayService: IPayService): boolean;
begin
  Result := False;
  try
    if FServiceList.Add(APayService) >= 0 then
    begin
      Messager.Info('成功加入支付业务:%s。', [APayService.GetName]);
      Result := True;
    end;
  except
    on e: Exception do
      Messager.Error('QueryPay: ', e);
  end;
end;

end.












