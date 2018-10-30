unit uPay;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  cm_interfaces, cm_parameter;

type

  { IPayTypeInfo
    // 支付类型信息，由后台管理统一配置
  }
  IPayTypeInfo = interface(ICMBase)
    ['{66265E44-2B90-4AA9-894F-A9D35A756164}']
    function GetPayType: Byte;  //支付类型
    function GetPayName: string;
  end;

  TPayTypeInfoList = TList<IPayTypeInfo>;

  { IPayRequest
    // 向支付中心发起支付的请求
  }
  IPayRequest = interface(ICMBase)
    ['{4B93095D-F6DC-401B-8FC7-AF194E3D6D82}']
    function GetOrderUUID: string;
    function GetPayAmount: Currency;
    function GetPayParameter: ICMConstantParameterDataList;
  end;

  { IPayResponse
    // 支付中心完成支付的响应
  }
  IPayResponse = interface(ICMBase)
    ['{6CF6CA56-9325-4F36-B1C9-55DFFC396C31}']
    function GetPayUUID: string;  //本次支付的唯一编码
    function GetPayAmount: Currency;
    function GetPayRemark: string;   //备注信息
    function GetPayMsg: ICMConstantParameterDataList; //其他支付相关信息
  end;

  { TPayResponseList }

  TPayResponseList = class(TThreadList<IPayResponse>)
  public
    function GetSumPayAmount: Currency;
  end;

  { IPayCenter
    // 供需要支付的地方调用
  }
  IPayCenter = Interface(IUnknown)
    ['{F56BACB4-1D77-4C78-873D-981A971E6B2B}']
    function Pay(APayRequest: IPayRequest): Boolean; overload;
    function Pay(APayRequest: IPayRequest; out thePayResponse: IPayResponse): Boolean; overload;
    function Pay(const APayCode: string; APayRequest: IPayRequest; out thePayResponse: IPayResponse): Boolean; overload;
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
    function GetPayAmount: Currency;
    function GetPayParameter: ICMConstantParameterDataList;
  end;

  { IPayServiceResponse
    // 支付业务完成支付的响应
  }
  IPayServiceResponse = interface(ICMBase)
    ['{E499FC74-D5C2-4772-B27D-B51A3DBF400A}']
    function GetServicePayUUID: string;
    function GetPayType: Byte; //支付类型，不记帐应大于等于100
    function GetPayAmount: Currency;
    function GetPayRemark: string;
    //function GetPayMsg: ICMConstantParameterDataList;
  end;

  { TPayServiceResponseList }

  TPayServiceResponseList = class(TThreadList<IPayServiceResponse>)
  public
    function GetSumPayAmount: Currency;
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
    function ToPay(ARequest: IPayServiceRequest; out theResponseList: TPayServiceResponseList): Boolean;
    function ToQueryPay(const AServicePayUUID: string): IPayServiceResponse;
  end;

  { IPayServiceRegister
    // 用于支付业务的注入
  }
  IPayServiceRegister = interface(ICMBase)
    ['{9F1C1551-7F35-4F2E-AC82-11CE630BBDD6}']
    function AddService(APayService: IPayService): Boolean;
  end;



implementation

{ TPayResponseList }

function TPayResponseList.GetSumPayAmount: Currency;
var
  i: Integer;
  list: TList<IPayResponse>;
begin
  Result := 0;
  list := LockList;
  try
    for i:=0 to list.Count-1 do
      begin
        Result := Result + list[i].GetPayAmount;
      end;
  finally
    UnlockList;
  end;
end;

{ TPayServiceResponseList }

function TPayServiceResponseList.GetSumPayAmount: Currency;
var
  i: Integer;
  list: TList<IPayServiceResponse>;
begin
  Result := 0;
  list := LockList;
  try
    for i:=0 to list.Count-1 do
      begin
        if IPayServiceResponse(list[i]).GetPayType < 100 then
          Result := Result + IPayServiceResponse(list[i]).GetPayAmount;
      end;
  finally
    UnlockList;
  end;
end;



end.

