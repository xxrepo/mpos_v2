unit uPayUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_parameter, cm_ParameterUtils,
  uPay;

type

  //----- 直接实现类（供需要时使用，不一定要用）--------------------------------------------------------

  { TGenericPayRequest
    // 方便构建支付请求参数
  }
  TGenericPayRequest = class(TCMBase, IPayRequest)
  private
    FOrderUUID: string;
    FPayAmount: currency;
    FPayParameter: ICMParameterDataList;
  public
    constructor Create(const AOrderUUID: string; APayAmount: currency);
    destructor Destroy; override;
    property PayParameter: ICMParameterDataList read FPayParameter;
  public
    function GetOrderUUID: string;
    function GetOrderAmount: currency;
    function GetPayParameter: ICMConstantParameterDataList;
  end;

  { TPayServiceResponse }

  TPayServiceResponse = class(TCMBase, IPayServiceResponse)
  private
    FServicePayUUID: string;
    FPayType: byte;
    FPayAmount: currency;
    FPayRemark: string;
    FPayMsg: ICMConstantParameterDataList;
  public
    constructor Create(const AServicePayUUID: string; AType: byte; AAmount: currency);
    property PayAmount: currency write FPayAmount;
    property PayRemark: string write FPayRemark;
    property PayMsg: ICMConstantParameterDataList write FPayMsg;
  public //IPayServiceResponse
    function GetServicePayUUID: string;
    function GetPayType: byte;
    function GetPayAmount: currency;
    function GetPayRemark: string;
    function GetPayMsg: ICMConstantParameterDataList;
  end;


  { TPayTypeInfo }

  TPayTypeInfo = class(TCMBase, IPayTypeInfo)
  private
    FPayCode: string;
    FPayName: string;
  public
    constructor Create(const APayCode: string; const APayName: string);
    destructor Destroy; override;
  public
    function GetPayCode: string;
    function GetPayName: string;
  end;


implementation

{ TPayTypeInfo }

constructor TPayTypeInfo.Create(const APayCode: string; const APayName: string);
begin
  FPayCode := APayCode;
  FPayName := APayName;
end;

destructor TPayTypeInfo.Destroy;
begin
  inherited Destroy;
end;

function TPayTypeInfo.GetPayCode: string;
begin
  Result := FPayCode;
end;

function TPayTypeInfo.GetPayName: string;
begin
  Result := FPayName;
end;

{ TPayServiceResponse }

constructor TPayServiceResponse.Create(const AServicePayUUID: string; AType: byte; AAmount: currency);
begin
  inherited Create;
  FServicePayUUID := AServicePayUUID;
  FPayType := AType;
  FPayAmount := AAmount;
  FPayRemark := '';
  FPayMsg := nil;
end;

function TPayServiceResponse.GetServicePayUUID: string;
begin
  Result := FServicePayUUID;
end;

function TPayServiceResponse.GetPayType: byte;
begin
  Result := FPayType;
end;

function TPayServiceResponse.GetPayAmount: currency;
begin
  Result := FPayAmount;
end;

function TPayServiceResponse.GetPayRemark: string;
begin
  Result := FPayRemark;
end;

function TPayServiceResponse.GetPayMsg: ICMConstantParameterDataList;
begin
  Result := FPayMsg;
end;

{ TGenericPayRequest }

constructor TGenericPayRequest.Create(const AOrderUUID: string; APayAmount: currency);
begin
  inherited Create;
  FOrderUUID := AOrderUUID;
  FPayAmount := APayAmount;
  FPayParameter := TCMParameterDataList.Create;
end;

destructor TGenericPayRequest.Destroy;
begin
  FPayParameter := nil;
  inherited Destroy;
end;

function TGenericPayRequest.GetOrderUUID: string;
begin
  Result := FOrderUUID;
end;

function TGenericPayRequest.GetOrderAmount: currency;
begin
  Result := FPayAmount;
end;

function TGenericPayRequest.GetPayParameter: ICMConstantParameterDataList;
begin
  Result := FPayParameter;
end;

end.

