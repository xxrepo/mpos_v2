unit uPayCenter;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DB,
  Generics.Collections,
  cm_interfaces, cm_messager, cm_parameter, cm_sysutils, uCashPay, cm_Plat,
  uPay, uPayPO, uPayDAO, uDAO,
  uPayRecordImpl, uPayAssignDAOImpl, uPayInfoImpl, uPayTypeImpl, uPayUtils;

type

  { TPOSPayDeal }

  TPOSPayDeal = class(TCMMessageable, IPayCenter, IPayServiceRegister, IPayBoardListener)
  private
    FServiceList: TInterfaceList;
    FTypeInfoList: TPayTypeInfoList;
    FRecordDAO: IPayRecordDAO;
    FAssignDAO: IPayAssignDAO;
    FInfoDAO: IPayInfoDAO;
    FPayBoard: IPayBoard;
    procedure LoadPayInfo;
  private
    FCurrOrderAmount: currency;
    FCurrPayUUID: string;
    FCurrOrderUUID: string;
    FPayVO: TPayView;
  public
    constructor Create;
    destructor Destroy; override;
    //TODO 配置
  private //service
    function GetPayService(const APayCode: string): IPayService;
    function GetPayedAmount(const AOrderUUID: string): currency;
    function CallPay(APayService: IPayService; const AAssignUUID: string; AAmount: currency;
      AResuestParameter: ICMConstantParameterDataList; out theAmount: currency): boolean;
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
  public //IPayBoardListener
    procedure Changed(e: IPayBoardEvent);
    procedure Closed(e: IPayBoardEvent);
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

const
  PayRootNode = 'pay';
  PayCode = 'Code';
  PayName = 'Name';

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

procedure TPOSPayDeal.LoadPayInfo;
var
  i: integer;
  PayTypeDAO: IPayTypeDAO;
  PayTypePOList: TPayTypePOList;
  PayTypePOList_: TList<TPayTypePO>;
begin
  if TPOSDAOFactory.GetInstance.OutDAO(TPayTypeDAO, IPayTypeDAO, PayTypeDAO) then
  begin
    PayTypePOList := PayTypeDAO.GetPayTypeList();
    try
      PayTypePOList_ := PayTypePOList.lockList();
      for i := 0 to PayTypePOList_.Count - 1 do
        FTypeInfoList.Add(TPayTypeInfo.Create(PayTypePOList_[i].Code, PayTypePOList_[i].Name));
    finally
      PayTypePOList.UnlockList;
    end;
  end;
end;

constructor TPOSPayDeal.Create;
begin
  inherited Create;
  FServiceList := TInterfaceList.Create;
  FTypeInfoList := TPayTypeInfoList.Create;
  FPayVO := TPayView.Create;

  TPOSDAOFactory.GetInstance.OutDAO(TPayRecordDAO, IPayRecordDAO, FRecordDAO);
  TPOSDAOFactory.GetInstance.OutDAO(TPayAssignDAO, IPayAssignDAO, FAssignDAO);
  TPOSDAOFactory.GetInstance.OutDAO(TPayInfoDAO, IPayInfoDAO, FInfoDAO);

  LoadPayInfo;
end;

destructor TPOSPayDeal.Destroy;
begin
  FServiceList.Free;
  FTypeInfoList.Free;
  FPayVO.Free;
  FRecordDAO := nil;
  FAssignDAO := nil;
  FInfoDAO := nil;
  FPayBoard := nil;
  inherited Destroy;
end;

function TPOSPayDeal.GetPayService(const APayCode: string): IPayService;
var
  i, j: integer;
  PayParmeter: ICMParameter;
begin
  Result := nil;
  try
    if AppSystem.GetParameter.Get(PayRootNode).IsNull then
    begin
      Messager.Debug('支付参数配置不存在');
      Exit;
    end;

    PayParmeter := AppSystem.GetParameter.Get(PayRootNode);
    if PayParmeter.ItemCount <= 0 then
    begin
      Messager.Debug('支付类型配置不存在');
      Exit;
    end;

    if FServiceList.Count <= 0 then
    begin
      Messager.Debug('支付服务配置不存在');
      Exit;
    end;

    //查找支付服务
    for i := 0 to PayParmeter.ItemCount - 1 do
      if not PayParmeter.GetItem(i).IsNull then
        if PayParmeter.GetItem(i).Get(PayCode).AsString = APayCode then
          for j := 0 to FServiceList.Count - 1 do
            if IPayService(FServiceList[j]).GetName = PayParmeter.GetItem(i).Get(PayName).AsString then
            begin
              Result := IPayService(FServiceList[j]);
              Messager.Debug('GetPayService: %s', [Result.GetName]);
            end;
  except
    on e: Exception do
      Messager.Error('GetPayService: ', e);
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

function TPOSPayDeal.CallPay(APayService: IPayService; const AAssignUUID: string; AAmount: currency;
  AResuestParameter: ICMConstantParameterDataList; out theAmount: currency): boolean;
var
  serviceResObj: TPayServiceRequest;
  serviceRes: IPayServiceRequest;
  serviceRspList: TPayServiceResponseList;

  i: integer;
  rsp: IPayServiceResponse;
  serviceRspListG: TList<IPayServiceResponse>;

  payInfo: TPayInfoPO;
begin
  Result := False;

  //准备支付业务请求信息
  serviceResObj := TPayServiceRequest.Create(AAmount);
  serviceResObj.FPayParameter := AResuestParameter;
  serviceRes := serviceResObj;
  if APayService.ToPay(serviceRes, serviceRspList) then
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
  Messager.Debug('bbbb');
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
  payedAmount: currency;
  ds: TDataSet;
  pi: TPaidInfo;
begin
  Result := False;
  try
    FCurrPayUUID := CreateGUIDStr;
    FPayVO.PaidInfoList.Clear;

    FCurrOrderAmount := APayRequest.GetOrderAmount;
    FPayVO.OrderAmount := FCurrOrderAmount;
    FCurrOrderUUID := APayRequest.GetOrderUUID;

    if not SavePayRecord(FCurrPayUUID, FCurrOrderUUID, FCurrOrderAmount) then
    begin
      Messager.Error('保存支付记录失败。');
      Exit;
    end;

    //查询确认已支付金额
    payedAmount := GetPayedAmount(APayRequest.GetOrderUUID);
    if payedAmount >= FCurrOrderAmount then
    begin
      thePayResponse := CreatePayResponse(FCurrPayUUID, '已经支付了这么多了呀', payedAmount, nil);
      Messager.Debug('已支付完成, 不需要再支付');
      Result := True;
      Exit;
    end;

    ds := FInfoDAO.GetPaidDataSet(FCurrOrderUUID);
    while not ds.EOF do
    begin
      pi := TPaidInfo.Create;
      pi.PayName := ds.FieldByName('serviceCode').AsString;
      pi.PayAmount := ds.FieldByName('amount').AsCurrency;
      pi.PayRemark := ds.FieldByName('remark').AsString;
      FPayVO.PaidInfoList.Add(pi);
      ds.Next;
    end;

    if InterfaceRegister.OutInterface(IPayBoard, FPayBoard) then
    begin
      FPayBoard.SetListener(Self);
      FPayBoard.SetPayView(FPayVO);
      FPayBoard.StartPay(FCurrPayUUID);

      payedAmount := GetPayedAmount(APayRequest.GetOrderUUID);
      Result := payedAmount >= APayRequest.GetOrderAmount;
    end;
    Messager.Debug('Pay End 支付类型选定');
    thePayResponse := CreatePayResponse(FCurrPayUUID, '已经支付了', payedAmount, nil);
  except
    on e: Exception do
      Messager.Error('Pay: ', e);
  end;
end;

function TPOSPayDeal.Pay(const APayCode: string; APayRequest: IPayRequest; out thePayResponse: IPayResponse): boolean;
begin
  Result := False;
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

procedure TPOSPayDeal.Changed(e: IPayBoardEvent);
var
  ps: IPayService;
  assignUUID: string;
  payedAmount: currency;
  callAmount: currency;
  pi: TPaidInfo;
begin
  Messager.Debug('Changed(%s)...', [e.GetPayCode]);

  if e.GetPayUUID <> FCurrPayUUID then
  begin
    Messager.Error('大大的异常');
    Exit;
  end;

  //根据选定支付方式, 获得支付服务
  ps := GetPayService(e.GetPayCode);
  if not Assigned(ps) then
  begin
    Messager.Error('选定支付服务不存在, 不能进行支付!');
    AppSystem.GetMsgBox().ShowMessage('选定支付服务不存在, 不能进行支付!');
    Exit;
  end;

  ////创建并保存支付分派任务
  assignUUID := CreateGUIDStr;
  //本次支付请求已支付金额
  payedAmount := FRecordDAO.GetPayedAmountByOrderUUID(FCurrOrderUUID);

  Messager.Info('FCurrOrderAmount:%.2f  payedAmount:%.2f', [FCurrOrderAmount, payedAmount]);
  if payedAmount >= FCurrOrderAmount then
  begin
    Messager.Info('快支付了之么多。');
    FPayBoard.StopPay;
    Exit;
  end;

  if not SavePayAssign(assignUUID, e.GetPayUUID, FCurrOrderAmount - payedAmount) then
  begin
    Messager.Error('支付分派任务保存失败, 退出');
    //thePayResponse := CreatePayResponse(payUUID, '已经支付了这么多了呀', payedAmount, nil);
    Exit;
  end;

  ////呼叫支付服务处理业务
  //if not CallPay(e.GetPayCode, assignUUID, selPayAmount, APayRequest.GetPayParameter, callAmount) then
  if not CallPay(ps, assignUUID, FCurrOrderAmount - payedAmount, nil, callAmount) then
  begin
    Messager.Error('支付服务处理业务失败, 退出');
    //thePayResponse := CreatePayResponse(payUUID, '已经支付了这么多了呀', payedAmount, nil);
    Exit;
  end;

  payedAmount := FRecordDAO.GetPayedAmountByOrderUUID(FCurrOrderUUID);
  Messager.Error('payedAmount: %.2f %.2f', [payedAmount, FCurrOrderAmount]);
  if payedAmount >= FCurrOrderAmount then
  begin
    Messager.Info('快111支付了之么多。');
    FPayBoard.StopPay;
    Exit;
  end;

  pi := TPaidInfo.Create;
  pi.PayName := 'xxx';
  pi.PayAmount := callAmount;
  pi.PayRemark := 'haha';
  FPayVO.PaidInfoList.Add(pi);
  FPayBoard.SetPayView(FPayVO);
  Messager.Debug('aaaaaaa');
end;

procedure TPOSPayDeal.Closed(e: IPayBoardEvent);
begin
  Messager.Debug('Closed (%s)...', [e.GetPayUUID]);
end;




//end;

end.












