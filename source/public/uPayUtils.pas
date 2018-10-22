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
    FPayAmount: Currency;
    FPayParameter: ICMParameterDataList;
  public
    constructor Create(const AOrderUUID: string; APayAmount: Currency);
    destructor Destroy; override;
    property PayParameter: ICMParameterDataList read FPayParameter;
  public
    function GetOrderUUID: string;
    function GetPayAmount: Currency;
    function GetPayParameter: ICMConstantParameterDataList;
  end;

  { TPayServiceResponse }

  TPayServiceResponse = class(TCMBase, IPayServiceResponse)
  private
    FServicePayUUID: string;
    FPayType: Byte;
    FPayAmount: Currency;
    FPayRemark: string;
    FPayMsg: ICMConstantParameterDataList;
  public
    constructor Create(const AServicePayUUID: string; AType: Byte; AAmount: Currency);
    property PayAmount: Currency write FPayAmount;
    property PayRemark: string write FPayRemark;
    property PayMsg: ICMConstantParameterDataList write FPayMsg;
  public //IPayServiceResponse
    function GetServicePayUUID: string;
    function GetPayType: Byte;
    function GetPayAmount: Currency;
    function GetPayRemark: string;
    function GetPayMsg: ICMConstantParameterDataList;
  end;

implementation

{ TPayServiceResponse }

constructor TPayServiceResponse.Create(const AServicePayUUID: string; AType: Byte; AAmount: Currency);
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

function TPayServiceResponse.GetPayType: Byte;
begin
  Result := FPayType;
end;

function TPayServiceResponse.GetPayAmount: Currency;
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

constructor TGenericPayRequest.Create(const AOrderUUID: string; APayAmount: Currency);
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

function TGenericPayRequest.GetPayAmount: Currency;
begin
  Result := FPayAmount;
end;

function TGenericPayRequest.GetPayParameter: ICMConstantParameterDataList;
begin
  Result := FPayParameter;
end;

end.

