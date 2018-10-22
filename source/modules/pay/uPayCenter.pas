unit uPayCenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_messager, cm_parameter, cm_sysutils, uCashPay,
  uPay;

type

  { TPOSPayDeal }

  TPOSPayDeal = class(TCMMessageable, IPayCenter, IPayServiceRegister)
  private
    FServiceList: TInterfaceList;
    FTypeInfoList: TPayTypeInfoList;
  public
    constructor Create;
    destructor Destroy; override;
    //TODO 配置
  private
    function GetPayCode: string;
    function GetPayService(const APayCode: string): IPayService;
  public //IPayCenter
    function Pay(APayRequest: IPayRequest): Boolean; overload;
    function Pay(APayRequest: IPayRequest; out thePayResponse: IPayResponse): Boolean; overload;
    function Pay(const APayCode: string; APayRequest: IPayRequest; out thePayResponse: IPayResponse): Boolean; overload;
    function QueryPay(const APayUUID: string): IPayResponse;
    function QueryOrder(const AOrderUUID: string): TPayResponseList;
    function GetPayTypeList: TPayTypeInfoList;
  public //IPayServiceRegister
    function AddService(APayService: IPayService): Boolean;
  end;


  { TPayResponse }

  TPayResponse = class(TCMBase, IPayResponse)
  private
    FPayUUID: string;
    FPayCode: string;
    FPayType: Byte;
    FPayAmount: Currency;
    FPayRemark: string;
    FPayMsg: ICMConstantParameterDataList;
  public
    constructor Create(const AUUID, ACode: string; AType: Byte; AAmount: Currency);
  public
    function GetPayUUID: string;
    function GetPayCode: string;
    function GetPayType: Byte;
    function GetPayAmount: Currency;
    function GetPayRemark: string;
    function GetPayMsg: ICMConstantParameterDataList;
  end;

  { TPayServiceRequest }

  TPayServiceRequest = class(TCMBase, IPayServiceRequest)
  private
    FPayUUID: string;
    FPayAmount: Currency;
    FPayParameter: ICMConstantParameterDataList;
  public
    constructor Create(AAmount: Currency);
  public
    function GetPayUUID: string;
    function GetPayAmount: Currency;
    function GetPayParameter: ICMConstantParameterDataList;
  end;

implementation

{ TPayServiceRequest }

constructor TPayServiceRequest.Create(AAmount: Currency);
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

function TPayServiceRequest.GetPayAmount: Currency;
begin
  Result := FPayAmount;
end;

function TPayServiceRequest.GetPayParameter: ICMConstantParameterDataList;
begin
  Result := FPayParameter;
end;

{ TPayResponse }

constructor TPayResponse.Create(const AUUID, ACode: string; AType: Byte; AAmount: Currency);
begin
  inherited Create;
  FPayUUID := AUUID;
  FPayCode := ACode;
  FPayAmount := AAmount;
  FPayRemark := '';
  FPayMsg := nil;
end;

function TPayResponse.GetPayUUID: string;
begin
  Result := FPayUUID;
end;

function TPayResponse.GetPayCode: string;
begin
  Result := FPayCode;
end;

function TPayResponse.GetPayType: Byte;
begin
  Result := FPayType;
end;

function TPayResponse.GetPayAmount: Currency;
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
end;

destructor TPOSPayDeal.Destroy;
begin
  FServiceList.Free;
  FTypeInfoList.Free;
  inherited Destroy;
end;

function TPOSPayDeal.GetPayCode: string;
begin
  Result := '';
  //TODO get pay code
  //test
  Result := 'CashPay';
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

function TPOSPayDeal.Pay(APayRequest: IPayRequest): Boolean;
var
  pr: IPayResponse;
begin
  Result := Pay(APayRequest, pr);
end;

function TPOSPayDeal.Pay(APayRequest: IPayRequest; out thePayResponse: IPayResponse): Boolean;
var
  payCode: string;
begin
  Result := False;
  payCode := GetPayCode;
  if payCode = '' then
    begin
      Messager.Error('soso 1111');
      Exit;
    end;
  Result := Pay(payCode, APayRequest, thePayResponse);
end;

function TPOSPayDeal.Pay(const APayCode: string; APayRequest: IPayRequest; out thePayResponse: IPayResponse): Boolean;
var
  ps: IPayService;
  serviceResObj: TPayServiceRequest;
  serviceRes: TPayServiceRequest;
  serviceRsp: IPayServiceResponse;
  payRspObj: TPayResponse;
begin
  Result := False;
  ps := GetPayService(APayCode);
  if not Assigned(ps) then
    begin
      Messager.Error('soso 2222');
      Exit;
    end;
  //准备支付业务请求信息
  serviceResObj := TPayServiceRequest.Create(APayRequest.GetPayAmount);
  serviceResObj.FPayParameter := APayRequest.GetPayParameter;
  serviceRes := serviceResObj;
  //
  if ps.ToPay(serviceRes, serviceRsp) then
    begin
      payRspObj := TPayResponse.Create(serviceRes.GetPayUUID, APayCode, serviceRsp.GetPayType, serviceRsp.GetPayAmount);
      payRspObj.FPayRemark := serviceRsp.GetPayRemark;
      payRspObj.FPayMsg := serviceRsp.GetPayMsg;
      thePayResponse := payRspObj;
      Result := True;
    end;
end;

function TPOSPayDeal.QueryPay(const APayUUID: string): IPayResponse;
begin
  Result := nil;
end;

function TPOSPayDeal.QueryOrder(const AOrderUUID: string): TPayResponseList;
begin
  Result := nil;
end;

function TPOSPayDeal.GetPayTypeList: TPayTypeInfoList;
begin
  Result := FTypeInfoList;
end;

function TPOSPayDeal.AddService(APayService: IPayService): Boolean;
begin
  Result := False;
  if FServiceList.Add(APayService) >= 0 then
    begin
      Messager.Info('成功加入支付业务:%s。', [APayService.GetName]);
      Result := True;
    end;
end;

end.

