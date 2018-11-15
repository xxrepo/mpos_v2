unit uSocketSOAPParamConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,strutils,
  typinfo,
  fpjson,

  laz2_DOM,
  laz2_XMLRead,
  laz2_XMLWrite,
  laz2_xmlutils,

  //uSocketBasiceInterface,
  //uExceptionDefine,
  uSocketPropRegister,
  cm_plat
  //uSocketArrayofObject
  ;

Type

  IParamConvert=Interface
    ['{5DC7E40F-EE74-4CDE-81F0-667C1A631AB1}']
    Function StrToXML(XMLStr:String):TXMLDocument;
    Function XMLToSrt(XMLDoc:TXMLDocument):String;

    Function FindXMLNode(XMLDoc:TDomNode; FindNodeName:String):TDomNode;

    Function JsonToXMLDoc(Json:TJSONData; RootNodeName:String):TXMLDocument;
    Procedure JsonToSoapXML(Json:TJSONData; RootNode:TDomNode; FindSoapNodeName:String);
    Function XMLToJson(XMLNode:TDomNode; StartFormNodeName:String):TJsonObject;

    Function JsonToObject(Json:TJSONData; Instance:TObject; PropInfo:PTypeInfo; DefaultObjectPropRegisty:IObjectPropInterface):Boolean;
    Function XMLToObject(XMLNode:TDomNode; StartFormNodeName:String; Instance:TObject; PropInfo:PTypeInfo; DefaultObjectPropRegisty:IObjectPropInterface):Boolean;

    Function ObjectToJson(Instance:TObject; PropInfo:PTypeInfo; DefaultObjectPropRegisty:IObjectPropInterface):TJsonObject;
    Function ObjectToXML( Instance:TObject; PropInfo:PTypeInfo; RootNodeName:String; DefaultObjectPropRegisty:IObjectPropInterface):TDomNode;
  end;

  ISOAPRequestConvert=Interface
    ['{73CEF01D-BD40-4034-8FF6-B3FCF71C24A0}']
    Function ObjectToJson(Instance:TObject; PropInfo:PTypeInfo; SoapMethodName:String):TJSONData; //-- 增加XML的属性
    Function ObjectToXML(Instance:TObject; PropInfo:PTypeInfo; SoapMethodName:String):TDomNode; //-- 增加XML的属性

    Function JsonToXML(Json:TJSONData;SoapMethodName:String):TDomNode;

    Procedure SetDefaultObjectPropRegisty(AValue:IObjectPropInterface);
    Function DefaultObjectPropRegisty:IObjectPropInterface;

    Function New():ISOAPRequestConvert;
  end;

  Function GetDefaultParamConvert():IParamConvert;
  Function GetDefaultSoapRequestConvert():ISOAPRequestConvert;

  Procedure SetDefaultParamConvert(AValue:IParamConvert);
  Procedure SetDefaultSoapRequestConvert(AValue:ISOAPRequestConvert);

  //Procedure NodeToJson(node: TDomNode; Json:TJSONData);
implementation
Var
    DefaultParamConvert: IParamConvert;
    DefaultSoapRequestConvert:ISOAPRequestConvert;
{ TParamConvert }


//Result:=ARegister.PutInterface(IParamConvert,TParamConvert.Create())>=1;
// if Not Result then exit;
//
// Result:=ARegister.PutInterface(ISOAPRequestConvert,TSOAPRequestConvert.Create())>=1;

Function GetDefaultParamConvert():IParamConvert;
Begin
  Result:=Nil;
  if DefaultParamConvert=nil then
    if not cm_plat.InterfaceRegister.OutInterface(IParamConvert,DefaultParamConvert) Then
      exit;
  Result:= DefaultParamConvert;
end;

Function GetDefaultSoapRequestConvert():ISOAPRequestConvert;
Begin
  Result:=Nil;
  if DefaultSoapRequestConvert=nil then
    if not cm_plat.InterfaceRegister.OutInterface(ISOAPRequestConvert,DefaultSoapRequestConvert) Then
      exit;
  Result:= DefaultSoapRequestConvert;
end;


Procedure SetDefaultParamConvert(AValue:IParamConvert);
Begin
  DefaultParamConvert:= AValue;
end;

Procedure SetDefaultSoapRequestConvert(AValue:ISOAPRequestConvert);
Begin
  DefaultSoapRequestConvert:=AValue;
end;




Initialization
  DefaultParamConvert:=Nil;
  DefaultSoapRequestConvert:=nil;






end.

