unit uSocketHTTPRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  typinfo,
  fpjson,
  laz2_DOM,
  laz2_XMLRead,
  laz2_XMLWrite,
  laz2_xmlutils,
  uSocketBasiceInterface,
  //uHeaderInterface,
  uSocketInterface
  //uURLInterface,
  //uRequestParamInterface,

  //uHttpSocket,

  //uExceptionDefine,
  //uSocketSOAPParamConvert
  ;


Type

  IURL=Interface
    ['{BA976291-54D6-4103-B9FF-C9583421CEC9}']
    Procedure SetURLAddr(URL:String);
    Function WebRegionAddr:String;  //--域名地址
    Function Host:String;  //--  Host地址
    Function Port:Word;
    Function HttpType:String;
    Function WebPageAddr:String;  //--网页地址
    Function URLParam:String; //--网页请求参数
    Function IsValidIP():Boolean;
  end;


  { IHeaderWrite }
  IHeaderWrite=Interface(IBasicParams)
  ['{91745433-344F-4026-9961-D1DFEBA944CF}']
    Procedure SetHost(AValue:String);
    Procedure SetContentType(AValue:String);
    Procedure SetCharset(AValue:String);
    Procedure SetConnection(AValue:String);
    Procedure SetContentLength(AValue:Integer);
    Procedure SetContentLanguage(AValue:String);
    Procedure SetContentEncoding(AValue:String);

    Procedure SetAccept(AValue:String);
    Procedure SetAcceptCharSet(AValue:String);
    Procedure SetAcceptEncoding(AValue:String);
    Procedure SetAcceptLanguage(AValue:String);

    Procedure SetReferer(AValue:String);
    Procedure SetUserAgent(AValue:String);
    Procedure AssignHeader(AValue:TJsonObject);

    Function AsHeader():String ;  //--转换成请求报文头格式
  end;

  {IHeaderReadWrite}
  IHeaderReadWrite=Interface(IHeaderWrite)
  ['{9EEA5CD5-FD85-42E2-91BE-F502FEAA1B76}']
    Procedure SetHeader(HeaderValue:String);
    Function GetStatusCode:Integer;
    Function GetStatusDesc:String;
    Function GetHost():String;
    Function GetContentType:String;
    Function GetContentLength:Integer;
    Function GetDate:String;
    Function ToHeader():String ; //--打印报文头内容
  end;

  {HTTP Request Interfaces}
  //TParamType= (P_STRING,P_JSON,P_XML);

  {
  IBaseParamTypeInterface=Interface(IBasicParams)
    ['{8ECDFC85-5E08-4094-A94A-4CC0D7B820AF}']
    Procedure SetParamType(Value:TParamType);
    Function ParamType():TParamType;
  end;

  IBaseXMLParamInterface=Interface
    ['{90761328-E730-4D1B-BE9F-2A76271F5DE0}']
    Procedure SetEncode(AValue:String);
    Procedure SetVersion(Version:String);
    Procedure AddNode(NodeName,ParentNodeName:String); overload;
    Procedure AddNode(NodeName,ParentNodeName:String;AValue:String);  overload;
    Procedure AddAttribute(NodeName:String; AttributeName,AttributeValue:String);
  end;

  IXMLParamInterface=Interface(IBaseXMLParamInterface)
    ['{DC623469-E826-4D7C-9DC7-EB046326EE17}']
    Procedure Clear();
    Function AsParamStr:String;
  end;

  IJsonParamInterface=Interface
    ['{0641593C-8120-4B85-AAF5-196364CB8972}']
    Procedure SetJson(Json:TJsonData);
    Function AsParamStr:String;
  end;

  IStringParamInterface=Interface
    ['{4224A042-8EB0-4855-99E2-E427F01AB9AA}']
    Procedure SetParamStr(AValue:String);
    Function AsParamStr:String;
  end;
  }

  IBaseParamInterface=Interface
    ['{EF943E2D-B861-4CC0-8E43-D54A1026E70F}']
    Procedure SetStrParam(AValue:String);
    Procedure SetXMLParam(AValue:TDomNode);
    Procedure SetJsonParam(AValue:TJsonData);
    Function ToRequestContent():String;
  end;

  IBasicHTTPRequst=Interface
  ['{EDEA2E6D-7C1B-4EA0-8AC6-78673C096B6A}']
    Procedure SetHttpMethodName(AValue:String);
    Function GetHttpMethodName():String;
    Procedure SetHttpVersion(Version:String);
    Function GetHttpVersion():String;
    Function Header:IHeaderReadWrite;
    Function RequestParam:IBaseParamInterface;
  end;

  IHttpRequest=Interface
  ['{BFB3CB57-2F28-4005-A3C4-EADE64BD7448}']
    Function Request:IBasicHTTPRequst;
    Function GetHttpRequestContent():String; //--生成请求参数
  end;


  IHttpResponse=Interface
    ['{2974F175-D32A-4798-A2F6-D7DA4053ED9D}']
    Function Header:IHeaderReadWrite;
    Function ResponseStream:TStringStream;
    Function ResponseContent:String;  //--响应内容
    Function Split(ResponseData:String):integer; //--分解响应内容
  end;


  INetHttp=Interface(IBasic)
    ['{C4D64589-4B81-49F8-AC38-B4317117A0B8}']
    Function URL:IURL;
    Function Request:IHttpRequest;
    Function GetRequestContent():String;
    Function GetResponse:IHttpResponse;
    Function HttpSocket:IHttpSocket;
    Function NewNetHttp():INetHttp;
  end;


implementation


end.

