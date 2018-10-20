unit uRemoteService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_Parameter, cm_DOM;

type
  //服务响应结果类型
  TResponseResult = (Success, ParamError, TerminalNotExist, SysUnregister, IdentiyException,
    AuthCodeError, Failure, SysException);

  //POS远程服务接口
  IRemoteService = interface(ICMBase)
    ['{624C6698-A5E4-4B61-B1ED-AE1C024E413F}']

    procedure SetMachineParameterInterface(AParameter: ICMParameter);
    //返回服务器时间
    function GetCurrentTime(const AUrl: string; out ADatetime: TDatetime; out RetCode: TResponseResult; out RetMsg: string;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //注册设备
    function RegisterMachine(const AUrl: string; ShopCode, TermNo, TermID, License, MACAddress: string; iType: integer;
      out CompCode, AuthCode: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000;
      AReadTimeOut: integer = 4000): boolean;
    //获取版本详细信息
    function GetClientVersion(const AUrl: string; verId: integer; out VerNo: string; out VerDesc: string;
      out IsMust: boolean; out FileUrl: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000;
      AReadTimeOut: integer = 4000): boolean;
    //获取本设备最后初始化任务
    function GetLastInitialId(const AUrl: string; out InitID: integer; out InitStep: string; out RetCode: TResponseResult;
      out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取初始化任务文件
    function InitialClient(const AUrl: string; TskId, iInitType, iInitStep: integer; out FileType: integer;
      out FileUrl: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取待执行的任务
    function GetUpdateTasks(const AUrl: string; out Tasks: TCMDOMNode; out RetCode: TResponseResult; out RetMsg: string;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取待执行的指令
    function GetCommandTasks(const AUrl: string; out Tasks: TCMDOMNode; out RetCode: TResponseResult; out RetMsg: string;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //上传数据接口
    function UploadData(const AUrl: string; uSQL: string; out RetCode: TResponseResult; out RetMsg: string;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //上传销售数据接口
    function UploadShopSaleData(const AUrl: string; Uuids, uSQL: string; out RetCode: TResponseResult; out RetMsg: string;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //分类别上传数据接口
    function UnifyUploadData(const AUrl: string; IDs, uSQL: string; iDataType: integer; out RetCode: TResponseResult;
      out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //查询数据接口
    function QueryData(const AUrl: string; DataType: integer; SearchKey: string; out RetCode: TResponseResult;
      out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取单据打印清单
    function GetPosPrintBillInfo(const AUrl: string; OrderNO, BeginDate, EndDate: string; out Orders: TCMDOMNode;
      out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取单据打印内容
    function GetPosPrintBillData(const AUrl: string; OrderNO: string; out Orders: TCMDOMNode; out RetCode: TResponseResult;
      out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    //获取标签数据
    function GetLabelData(const AUrl: string; iDataType, iGDGID, iPageSize, iPageIndex: integer; vSortCode: string;
      out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;

  end;




implementation

end.
