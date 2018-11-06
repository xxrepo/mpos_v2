unit uPay;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  cm_interfaces, cm_parameter,
  uInterfaces;

type

  { IPayTypeInfo
    // 支付类型信息，由后台管理统一配置
  }
  IPayTypeInfo = interface(ICMBase)
    ['{43052FD7-F093-4350-A573-E265ED74190D}']
    function GetPayCode: string;
    function GetPayName: string;
  end;

  TPayTypeInfoList = TList<IPayTypeInfo>;

  { IPayRequest
    // 向支付中心发起支付的请求
  }
  IPayRequest = interface(ICMBase)
    ['{BEB23E41-5968-4EF0-9D13-6F66850DBB3D}']
    function GetOrderUUID: string;
    function GetOrderAmount: currency;  //单据金额
    function GetPayParameter: ICMConstantParameterDataList;
  end;

  { IPayResponse
    // 支付中心完成支付的响应
  }
  IPayResponse = interface(ICMBase)
    ['{6CF6CA56-9325-4F36-B1C9-55DFFC396C31}']
    function GetPayUUID: string;  //本次支付的唯一编码
    function GetPayAmount: currency;
    function GetPayRemark: string;   //备注信息
    function GetPayMsg: ICMConstantParameterDataList; //其他支付相关信息
  end;

  { TPayResponseList }

  TPayResponseList = class(TThreadList<IPayResponse>)
  public
    function GetSumPayAmount: currency;
  end;

  { IPayCenter
    // 供需要支付的地方调用
  }
  IPayCenter = interface(IUnknown)
    ['{F56BACB4-1D77-4C78-873D-981A971E6B2B}']
    function Pay(APayRequest: IPayRequest): boolean; overload;
    function Pay(APayRequest: IPayRequest; out thePayResponse: IPayResponse): boolean; overload;
    function Pay(const APayCode: string; APayRequest: IPayRequest; out thePayResponse: IPayResponse): boolean; overload;
    function QueryPay(const APayUUID: string): IPayResponse;
    function QueryOrder(const AOrderUUID: string): TPayResponseList;
    function GetPayTypeList: TPayTypeInfoList;
  end;

  //---- 支付业务 -------------------------------------------------------------------------

  { IPayServiceRequest
    // 向支付业务发起支付的请求
  }
  IPayServiceRequest = interface(ICMBase)
    ['{909583CD-F403-4CC4-AB42-719D71B0E786}']
    function GetPayUUID: string;
    function GetPayAmount: currency;
    function GetPayParameter: ICMConstantParameterDataList;
  end;

  { IPayServiceResponse
    // 支付业务完成支付的响应
  }
  IPayServiceResponse = interface(ICMBase)
    ['{E499FC74-D5C2-4772-B27D-B51A3DBF400A}']
    function GetServicePayUUID: string;
    function GetPayType: byte; //支付类型，不记帐应大于等于100
    function GetPayAmount: currency;
    function GetPayRemark: string;
    //function GetPayMsg: ICMConstantParameterDataList;
  end;

  { TPayServiceResponseList }

  TPayServiceResponseList = class(TThreadList<IPayServiceResponse>)
  public
    function GetSumPayAmount: currency;
    //TODO
    //function GetPayMsg: ICMConstantParameterDataList;
  end;

  { IPayService
    // 支付业务，由各支付提供者实现
  }
  IPayService = interface(ICMBase)
    ['{7273DCBD-BC5F-487A-8A5C-DFC48A6DDBD3}']
    function GetName: string;
    //因为 theResponseList 不是自动进行生命周期管理的接口，故返回 True 时务必要手动释放。
    function ToPay(ARequest: IPayServiceRequest; out theResponseList: TPayServiceResponseList): boolean;
    function ToQueryPay(const AServicePayUUID: string): IPayServiceResponse;
  end;

  { IPayServiceRegister
    // 用于支付业务的注入
  }
  IPayServiceRegister = interface(ICMBase)
    ['{18609CC1-85A0-42ED-8BB1-7C7939B93238}']
    function AddService(APayService: IPayService): boolean;
  end;

  IPayBoardEvent = interface(ICMEvent)
    ['{8C3567D8-F7C3-4CF4-B5E5-E74B0B67ED0D}']
    function GetPayUUID: string;
    function GetPayCode: string;
    function GetAmount: currency;
  end;

  IPayBoardListener = interface(ICMListener)
    ['{BD3DAB81-1209-47B3-AE93-D0ED8083D7BF}']
    procedure Changed(e: IPayBoardEvent);
    procedure Closed(e: IPayBoardEvent);
  end;

  TPaidInfo = class
  private
    FPayName: string;
    FPayAmount: currency;
    FPayRemark: string;
  public
    property PayName: string read FPayName write FPayName;
    property PayAmount: currency read FPayAmount write FPayAmount;
    property PayRemark: string read FPayRemark write FPayRemark;
  end;

  { TPayView }

  TPayView = class
  private
    FOrderAmount: currency;
    FPaidInfoList: TObjectList<TPaidInfo>;
  public
    constructor Create;
    destructor Destroy; override;
    property OrderAmount: currency read FOrderAmount write FOrderAmount;
    property PaidInfoList: TObjectList<TPaidInfo> read FPaidInfoList write FPaidInfoList;
  end;

  IPayBoard = interface(IPromptableBoard)
    ['{63838A40-14F5-4BD8-810A-47D32C0E3703}']
    procedure PromptMessage(AEventType: TEventType; const AMsg: string);
    procedure SetListener(AListener: IPayBoardListener);

    procedure SetPayView(AVO: TPayView);
    procedure StartPay(const APayUUID: string);
    procedure StopPay;
  end;



implementation

{ TPayView }

constructor TPayView.Create;
begin
  FPaidInfoList := TObjectList<TPaidInfo>.Create;
end;

destructor TPayView.Destroy;
begin
  FPaidInfoList.Free;
  inherited Destroy;
end;

{ TPayResponseList }

function TPayResponseList.GetSumPayAmount: currency;
var
  i: integer;
  list: TList<IPayResponse>;
begin
  Result := 0;
  list := LockList;
  try
    for i := 0 to list.Count - 1 do
    begin
      Result := Result + list[i].GetPayAmount;
    end;
  finally
    UnlockList;
  end;
end;

{ TPayServiceResponseList }

function TPayServiceResponseList.GetSumPayAmount: currency;
var
  i: integer;
  list: TList<IPayServiceResponse>;
begin
  Result := 0;
  list := LockList;
  try
    for i := 0 to list.Count - 1 do
    begin
      if IPayServiceResponse(list[i]).GetPayType < 100 then
        Result := Result + IPayServiceResponse(list[i]).GetPayAmount;
    end;
  finally
    UnlockList;
  end;
end;



end.

