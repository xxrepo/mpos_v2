unit uSocketSOAPInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  typinfo,
  fpjson,
  laz2_DOM,
  laz2_XMLRead,
  //laz2_XMLWrite,
  laz2_xmlutils,
  //uBasiceInterface,
  uSocketInterface,
  uSocketHTTPRequest,
  //uExceptionDefine,
  //uParamConvert,
  uSocketPropRegister;

Type
  TSOAPResult=(R_UNKNOWN,R_FAILED,R_SUCCESS);

  IHttpRequestSetting=Interface
    ['{AFA6C5AF-8F18-464D-A992-1EA0A25556AC}']
    Function RunMode:Byte; //--0: 后台运行模式; 其它前台模式
    Function ObjectPropInterface:IObjectPropInterface;

    Procedure SetRunMode(AValue:Byte);
    Function SocketSetting:ISocketSetting;
    Procedure SetObjectPropInterface(AValue:IObjectPropInterface);
  end;


  { ISOAPResponse }

  ISOAPResponse=Interface
    ['{5565CDB7-7CE6-4E3B-8414-391EC8CC82AB}']
    Procedure SetSOAPResult(AValue:TSOAPResult);
    Function SOAPResult:TSOAPResult;
    Procedure SetSOAPResultDesc(AValue:String);
    Function SOAPResultDesc:String;
    Procedure SetResponse(AValue:IHttpResponse);
    Function Response:IHttpResponse;
    Procedure Assign(SrcSOAPResponse:ISOAPResponse);
  end;



  IAPIHttp=Interface
    ['{DE2B5FB2-A5BF-4B4D-97B9-8CC19F9735BA}']
    Function HttpMethod(IdNetHttp:INetHttp): ISOAPResponse;
    Function HttpGet(WebURL:String; HeaderList:TJsonObject; RequestParam:Pointer; Size:Integer; HttpRequestSetting:IHttpRequestSetting): ISOAPResponse;  overload;
    Function HttpPost(WebURL:String; HeaderList:TJsonObject;RequestParam:Pointer; Size:Integer; HttpRequestSetting:IHttpRequestSetting): ISOAPResponse;  overload;

    Function HttpGet(WebURL:String; HeaderList:TJsonObject; RequestParam:String; HttpRequestSetting:IHttpRequestSetting): ISOAPResponse;  overload;
    //Function HttpGet_Json(WebURL:String;HeaderList:TJsonObject; RequestParam:String; out ResponseJson:TJsonObject; HttpRequestSetting:IHttpRequestSetting):ISOAPResponse;
    //Function HttpGet_Object(WebURL:String;HeaderList:TJsonObject;RequestParam:String; out ResponseObj:TObject; ResponseTypeInfo:PTypeInfo;  HttpRequestSetting:IHttpRequestSetting):ISOAPResponse;

    Function HttpPost(WebURL:String; HeaderList:TJsonObject;RequestParam:String; HttpRequestSetting:IHttpRequestSetting): ISOAPResponse; overload;
    //Function HttpPost_Json(WebURL:String;HeaderList:TJsonObject;RequestParam:String; out ResponseJson:TJsonObject; HttpRequestSetting:IHttpRequestSetting):ISOAPResponse;
    //Function HttpPost_Object(WebURL:String;HeaderList:TJsonObject;RequestParam:String; out ResponseObj:TObject; ResponseTypeInfo:PTypeInfo; HttpRequestSetting:IHttpRequestSetting):ISOAPResponse;
    Function NewAPIHttp():IAPIHttp;
  end;


  ISOAPHttp=Interface(IAPIHttp)
    ['{19D2EFDE-69B3-4361-AEAE-4D0B8B687F41}']

    Function BMInvoke(WebURL:String; SoapMethodName:String; RequestObj:TObject; RequestTypeInfo:PTypeInfo; Out ResponseObj:TObject; ResponseTypeInfo:PTypeInfo;  HttpRequestSetting:IHttpRequestSetting): ISOAPResponse; overload;
    Function BMInvoke(WebURL:String; RequestObj:TObject; RequestTypeInfo:PTypeInfo; Out ResponseObj:TObject; ResponseTypeInfo:PTypeInfo;  HttpRequestSetting:IHttpRequestSetting): ISOAPResponse; overload;
    Function BMInvoke(WebURL:String; RequestObj:TObject; RequestTypeInfo:PTypeInfo; out ResponseJson:TJsonData; HttpRequestSetting:IHttpRequestSetting): ISOAPResponse;overload;
    Function BMInvoke(WebURL:String; RequestObj:TObject; RequestTypeInfo:PTypeInfo; SoapResponseNodeName:String; out ResponseJson:TJsonData;  HttpRequestSetting:IHttpRequestSetting): ISOAPResponse; overload;

    Function BMInvoke(WebURL:String; RequstJson:TJsonData; out Response:TJsonData;  HttpRequestSetting:IHttpRequestSetting): ISOAPResponse; overload;
    Function BMInvoke(WebURL:String; SoapMethodName:String; RequstJson:TJsonData; out ResponseJson:TJsonData;  HttpRequestSetting:IHttpRequestSetting): ISOAPResponse; overload;
    Function BMInvoke(WebURL:String;  SoapMethodName:String;RequstJson:TJsonData; Out ResponseObj:TObject; ResponseTypeInfo:PTypeInfo;  HttpRequestSetting:IHttpRequestSetting): ISOAPResponse; overload;
  end;

  { THttpRequstSetting }

  THttpRequstSetting=Class(TInterfacedObject, IHttpRequestSetting)
  Protected
    FRunMode:Byte;
    FSocketSetting:ISocketSetting;
    FDefaultPropInterface:IObjectPropInterface;
  Public
    Constructor Create();
    Function RunMode:Byte; //--0: 后台运行模式; 其它前台模式
    Procedure SetRunMode(AValue:Byte);
    Function  SocketSetting:ISocketSetting;

    Function ObjectPropInterface():IObjectPropInterface;
    Procedure SetObjectPropInterface(AValue:IObjectPropInterface);
  end;

  Function GetHttpRequestSetting():IHttpRequestSetting;

implementation

Function GetHttpRequestSetting():IHttpRequestSetting;
Begin
  Result:= THttpRequstSetting.Create();
end;

{ THttpRequstSetting }

constructor THttpRequstSetting.Create();
begin
  Inherited;
  FRunMode:=0;
  FSocketSetting:= Nil;
  FDefaultPropInterface:=Nil;
end;

function THttpRequstSetting.RunMode: Byte;
begin
  Result:= FRunMode;
end;

procedure THttpRequstSetting.SetRunMode(AValue: Byte);
begin
  if AValue<>FRunMode then
    FRunMode:= AValue;
end;

function THttpRequstSetting.SocketSetting: ISocketSetting;
begin
  if not Assigned(FSocketSetting) Then
    FSocketSetting:= TSocketSetting.Create();
  Result:=FSocketSetting;
end;

function THttpRequstSetting.ObjectPropInterface(): IObjectPropInterface;
begin
  Result:=FDefaultPropInterface;
end;

procedure THttpRequstSetting.SetObjectPropInterface(AValue: IObjectPropInterface);
begin
  FDefaultPropInterface:=AValue;
end;


end.

