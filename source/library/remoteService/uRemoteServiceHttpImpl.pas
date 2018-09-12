unit uRemoteServiceHttpImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_messager, FPJSon, IDHttp, LazUtf8, DateUtils, IdURI, cm_DOM, cm_XML, uRemoteService, TypInfo,
  cm_Parameter, Base64, IdAntiFreeze, Syncobjs;

type

  { THttpRemoteService }

  THttpRemoteService = class(TCMMessageableComponent, IRemoteService)
  private
    FCriticalSection: TRTLCriticalSection;
    FParameter: ICMParameter;
    //设置公共访问参数
    function HttpClientPost(const AURL, IntfMethod: string; const ARequestNode: TCMDOMNode; out ARetNode: TCMDOMNode;
      AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
    function GetRequestParameterNode(ANodeName: string = 'request'; IsCommonParameter: boolean = True): TCMDOMNode;
  public
    constructor Create(AOwner: TComponent); override;
  public //impl
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



constructor THttpRemoteService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //初始化临界区
  InitCriticalSection(FCriticalSection);
end;

procedure THttpRemoteService.SetMachineParameterInterface(AParameter: ICMParameter);
begin
  Self.FParameter := AParameter;
end;


function THttpRemoteService.GetCurrentTime(const AUrl: string; out ADatetime: TDatetime; out RetCode: TResponseResult;
  out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'GetCurrentTime';
var
  vRetCode, vDateTime: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  ADatetime := MinDatetime;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      vDateTime := retNode.FindChildNode('CurrentTime').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s  CurrentTime: %s', [vRetCode, RetMsg, vDatetime]);

      ADateTime := StrToDatetimeDef(vDateTime, ADateTime);
      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));

      Result := True;
    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(retNode) then
        FreeAndNil(retNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;


function THttpRemoteService.RegisterMachine(const AUrl: string; ShopCode, TermNo, TermID, License, MACAddress: string;
  iType: integer; out CompCode, AuthCode: string; out RetCode: TResponseResult; out RetMsg: string;
  AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'RegisterMachine';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode('request', False);
      TCMDOMNode.Create('ShopCode', ShopCode, reqNode);
      TCMDOMNode.Create('MUUID', TermID, reqNode);
      TCMDOMNode.Create('TerminalNo', TermNo, reqNode);
      TCMDOMNode.Create('MacAddr', MACAddress, reqNode);
      TCMDOMNode.Create('License', License, reqNode);
      TCMDOMNode.Create('RegisterType', IntToStr(iType), reqNode);

      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        CompCode := retNode.FindChildNode('CompanyCode').Text;
        AuthCode := retNode.FindChildNode('RegisterCode').Text;
        Messager.Info('Result: CompCode: %s  AuthCode: %s ', [CompCode, AuthCode]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(retNode) then
        FreeAndNil(retNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, [e.ClassName, e.Message]);
  end;
end;

function THttpRemoteService.GetClientVersion(const AUrl: string; verId: integer; out VerNo: string; out VerDesc: string;
  out IsMust: boolean; out FileUrl: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000;
  AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'GetClientVersion';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      TCMDOMNode.Create('verId', IntToStr(verId), reqNode);
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        IsMust := StrToBoolDef(retNode.FindChildNode('IsMust').Text, False);

        VerNo := retNode.FindChildNode('VerNo').Text;
        FileUrl := retNode.FindChildNode('UpdateFileUrl').Text;
        VerDesc := retNode.FindChildNode('VerDesc').Text;

        Messager.Info('Result: VerNo: %s  FileUrl: %s ', [VerNo, FileUrl]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(retNode) then
        FreeAndNil(retNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.GetLastInitialId(const AUrl: string; out InitID: integer; out InitStep: string;
  out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'GetLastInitialId';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        InitId := StrToIntDef(retNode.FindChildNode('Id').Text, -1);
        InitStep := retNode.FindChildNode('Step').Text;

        Messager.Info('Result: InitId: %d  InitStep: %s ', [InitId, InitStep]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(retNode) then
        FreeAndNil(retNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.InitialClient(const AUrl: string; TskId, iInitType, iInitStep: integer; out FileType: integer;
  out FileUrl: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'InitialClient';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      TCMDOMNode.Create('TskId', IntToStr(TskId), reqNode);
      TCMDOMNode.Create('iInitType', IntToStr(iInitType), reqNode);
      TCMDOMNode.Create('iInitStep', IntToStr(iInitStep), reqNode);
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        FileType := StrToIntDef(retNode.FindChildNode('DataType').Text, -1);
        FileUrl := retNode.FindChildNode('FileUrl').Text;

        Messager.Info('Result: FileType: %d  FileUrl: %s ', [FileType, FileUrl]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(retNode) then
        FreeAndNil(retNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.GetUpdateTasks(const AUrl: string; out Tasks: TCMDOMNode; out RetCode: TResponseResult;
  out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'GetUpdateTasks';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        Tasks := retNode;

        Messager.Info('Result: Tasks.ChildCount: %d ', [Tasks.ChildCount]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
      //if Assigned(retNode) then
      //  FreeAndNil(retNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.GetCommandTasks(const AUrl: string; out Tasks: TCMDOMNode; out RetCode: TResponseResult;
  out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'GetCommandTasks';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        Tasks := retNode;

        Messager.Info('Result: Tasks.ChildCount: %d ', [Tasks.ChildCount]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
      //if Assigned(retNode) then
      //  FreeAndNil(retNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.UploadData(const AUrl: string; uSQL: string; out RetCode: TResponseResult; out RetMsg: string;
  AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'UploadData';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      TCMDOMNode.Create('ExecSQL', Base64.EncodeStringBase64(uSQL), reqNode);
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        Messager.Debug('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
      if Assigned(retNode) then
        FreeAndNil(retNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.UploadShopSaleData(const AUrl: string; Uuids, uSQL: string; out RetCode: TResponseResult;
  out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'UploadShopSaleData';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      TCMDOMNode.Create('UUID', Uuids, reqNode);
      TCMDOMNode.Create('ExecSQL', Base64.EncodeStringBase64(uSQL), reqNode);
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        Messager.Debug('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
      if Assigned(retNode) then
        FreeAndNil(retNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.UnifyUploadData(const AUrl: string; IDs, uSQL: string; iDataType: integer;
  out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'UnifyUploadData';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      TCMDOMNode.Create('ID', IDs, reqNode);
      TCMDOMNode.Create('DataType', IntToStr(iDataType), reqNode);
      TCMDOMNode.Create('ExecSQL', Base64.EncodeStringBase64(uSQL), reqNode);
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        Messager.Debug('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
      if Assigned(retNode) then
        FreeAndNil(retNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.QueryData(const AUrl: string; DataType: integer; SearchKey: string; out RetCode: TResponseResult;
  out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'QueryData';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      TCMDOMNode.Create('SearchKey', SearchKey, reqNode);
      TCMDOMNode.Create('DataType', IntToStr(DataType), reqNode);
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        Messager.Debug('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
      if Assigned(retNode) then
        FreeAndNil(retNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.GetPosPrintBillInfo(const AUrl: string; OrderNO, BeginDate, EndDate: string;
  out Orders: TCMDOMNode; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'GetPosPrintBillInfo';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      TCMDOMNode.Create('OrderNO', OrderNO, reqNode);
      TCMDOMNode.Create('BeginDate', BeginDate, reqNode);
      TCMDOMNode.Create('EndDate', EndDate, reqNode);
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        Orders := retNode;
        Messager.Info('Result: Orders.ChildCount: %d ', [Orders.ChildCount]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
      if Assigned(retNode) then
        FreeAndNil(retNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.GetPosPrintBillData(const AUrl: string; OrderNO: string; out Orders: TCMDOMNode;
  out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
const
  IntfMethodName = 'GetPosPrintBillData';
var
  vRetCode: string;
  reqNode: TCMDOMNode = nil;
  retNode: TCMDOMNode = nil;
  respNode: TCMDOMNode = nil;
begin
  Result := False;
  RetCode := TResponseResult.Failure;
  try
    //设置访问参数
    try
      reqNode := Self.GetRequestParameterNode();
      TCMDOMNode.Create('OrderNO', OrderNO, reqNode);
      if not HttpClientPost(AUrl, IntfMethodName, reqNode, retNode, AConnectTimeOut, AReadTimeOut) then
      begin
        RetMsg := '服务请求失败, 请确认服务地址!';
        Messager.Error(IntfMethodName, RetMsg);
        Exit;
      end;

      ////返回值判断
      if not Assigned(retNode) then
      begin
        Messager.Error('Result: 不存在');
        Exit;
      end;

      //返回值代码
      RetMsg := retNode.FindChildNode('ResponseMsg').Text;
      vRetCode := retNode.FindChildNode('ResponseCode').Text;
      Messager.Info('Result: ResponseCode: %s  ResponseMsg: %s ', [vRetCode, RetMsg]);

      RetCode := TResponseResult(GetEnumValue(TypeInfo(TResponseResult), vRetCode));
      if (RetCode = TResponseResult.Success) then
      begin
        Orders := retNode;
        Messager.Info('Result: Orders.ChildCount: %d ', [Orders.ChildCount]);
        Result := True;
      end;

    finally
      if Assigned(reqNode) then
        FreeAndNil(reqNode);
      if Assigned(respNode) then
        FreeAndNil(respNode);
      //if Assigned(retNode) then
      //  FreeAndNil(retNode);
    end;
  except
    on e: Exception do
      Messager.Error(IntfMethodName, e);
  end;
end;

function THttpRemoteService.GetLabelData(const AUrl: string; iDataType, iGDGID, iPageSize, iPageIndex: integer;
  vSortCode: string; out RetCode: TResponseResult; out RetMsg: string; AConnectTimeOut: integer; AReadTimeOut: integer): boolean;

begin
  Result := False;
end;


function THttpRemoteService.HttpClientPost(const AURL, IntfMethod: string; const ARequestNode: TCMDOMNode;
  out ARetNode: TCMDOMNode; AConnectTimeOut: integer = 4000; AReadTimeOut: integer = 4000): boolean;
var
  URL: TIdURI = nil;
  MethodNode: TCMDOMNode = nil;
  RespNode: TCMDOMNode = nil;
  BodyNode: TCMDOMNode = nil;
  RootNode: TCMDOMNode = nil;
  HttpClient: TIDHttp = nil;
  ARetStream: TStringStream = nil;
  AReqStream: TStringStream = nil;
  ARespStream: TStringStream = nil;
  Streamer: TCMDOMNodeStreamer = nil;
  IdAntiFreeze: TIdAntiFreeze = nil;
begin
  Result := False;
  EnterCriticalSection(FCriticalSection); //进入临界区
  try
    try
      try
        Streamer := TCMDOMNodeStreamer.Create(nil);
        AReqStream := TStringStream.Create;
        ARetStream := TStringStream.Create;
        ARespStream := TStringStream.Create;
        IdAntiFreeze := TIdAntiFreeze.Create(nil);
        URL := TIdURI.Create(AURL);

        HttpClient := TIDHttp.Create;
        HttpClient.Request.AcceptLanguage := 'zh-cn';
        HttpClient.Request.ContentType := 'text/xml; charset=utf-8';
        HttpClient.Request.Host := URL.Host;
        HttpClient.ConnectTimeout := AConnectTimeOut;
        HttpClient.ReadTimeout := AReadTimeOut;

        RootNode := TCMDOMNode.Create('SOAP-ENV:Envelope');
        RootNode.SetAttribute('xmlns:xsd', 'http://www.w3.org/2001/XMLSchema');
        RootNode.SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
        RootNode.SetAttribute('xmlns:soap', 'http://schemas.xmlsoap.org/soap/envelope/');
        RootNode.SetAttribute('xmlns:SOAP-ENV', 'http://schemas.xmlsoap.org/soap/envelope/');
        BodyNode := TCMDOMNode.Create('SOAP-ENV:Body', RootNode);

        MethodNode := TCMDOMNode.Create(IntfMethod, BodyNode);
        MethodNode.SetAttribute('xmlns', 'http://www.meiyijia.com.cn/');
        MethodNode.AppendChild(ARequestNode);

        //参数对象转字符流准备发送
        Streamer.WriteXML(RootNode, AReqStream);
        Messager.Debug('Request: %s', [AReqStream.DataString]);

        //发送访问请求
        HttpClient.Post(URL.URI, AReqStream, ARespStream);
        Messager.Debug('Response: %s', [ARespStream.DataString]);

        ARespStream.Position := 0;
        Streamer.ReadXML(RespNode, ARespStream);

        if Assigned(RespNode) then
          if RespNode.HasChildNodes then
            if RespNode.FirstChild.HasChildNodes then
              if RespNode.FirstChild.FirstChild.HasChildNodes then
              begin
                Streamer.WriteXML(RespNode.FirstChild.FirstChild.FirstChild, ARetStream);
                ARetStream.Position := 0;
                Streamer.ReadXML(ARetNode, ARetStream);
                Messager.Debug('Result: %s', [ARetStream.DataString]);

                Result := True;
              end;
      finally
        if Assigned(URL) then
          FreeAndNil(URL);
        if Assigned(Streamer) then
          FreeAndNil(Streamer);
        if Assigned(ARetStream) then
          FreeAndNil(ARetStream);
        if Assigned(AReqStream) then
          FreeAndNil(AReqStream);
        if Assigned(ARespStream) then
          FreeAndNil(ARespStream);
        if Assigned(HttpClient) then
          FreeAndNil(HttpClient);
        if Assigned(RootNode) then
          FreeAndNil(RootNode);
        if Assigned(IdAntiFreeze) then
          FreeAndNil(IdAntiFreeze);
      end;
    except
      on e: Exception do
        Messager.Error('HttpClientPost: %s %s', [e.ClassName, e.Message]);
    end;
  finally
    //离开临界区
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function THttpRemoteService.GetRequestParameterNode(ANodeName: string = 'request'; IsCommonParameter: boolean = True): TCMDOMNode;
begin
  Result := TCMDOMNode.Create(ANodeName);
  if (IsCommonParameter) and Assigned(FParameter) then
  begin
    TCMDOMNode.Create('CompanyCode', FParameter.Get('compCode').AsString, Result);
    TCMDOMNode.Create('TerminalNo', FParameter.Get('termCode').AsString, Result);
    TCMDOMNode.Create('ShopCode', FParameter.Get('shopCode').AsString, Result);
    TCMDOMNode.Create('AuthCode', FParameter.Get('authCode').AsString, Result);
    TCMDOMNode.Create('MacAddr', FParameter.Get('MacAddress').AsString, Result);
  end;
end;

initialization

end.
