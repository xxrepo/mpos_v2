{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_XML

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_XML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_DOM,
  laz2_DOM,
  laz2_XMLRead,
  laz2_XMLWrite,
  laz2_xmlutils,
  LazUTF8Classes,
  LazFileUtils;

type

  ECMXMLException = class(Exception);

  { TCMDOMNodeStreamer }

  TCMDOMNodeStreamer = class(TComponent)
  protected const
    FTextAttributesName: string = 'thenodetext';
  protected
    function Dom2CM(const ADomNode: TDOMNode; var ACMNode: TCMDOMNode): Boolean; virtual;
    function CM2DOM(const ACMNode: TCMDOMNode; var ADOMNode: TDOMNode): Boolean; virtual;
  public
    function ReadXML(out AMCNode: TCMDOMNode; AStream: TStream): Boolean; overload;
    function ReadXML(out AMCNode: TCMDOMNode; const AFileName: string): Boolean; overload;
    function WriteXML(const AMCNode: TCMDOMNode; AStream: TStream): Boolean; overload;
    function WriteXML(const AMCNode: TCMDOMNode; const AFileName: string): Boolean; overload;
    function NodeToStr(const AMCNode: TCMDOMNode): string;
    function StrToNode(const AMCNStr: string): TCMDOMNode;
  end;

implementation

{TCMDOMNodeStreamer}

function TCMDOMNodeStreamer.Dom2CM(const ADomNode: TDOMNode; var ACMNode: TCMDOMNode): Boolean;
var
  i: Integer;
  vNode: TDOMNode;
  tempNode: TCMDOMNode;
  attributeNode: TDOMNode;
  textStr: string;
begin
  Result := False;
  if Assigned(ADomNode) and (ADomNode.NodeName <> '#text') and (ADomNode.NodeName <> '#comment') then
    begin
      if Assigned(ACMNode) then
        tempNode := TCMDOMNode.Create(ADomNode.NodeName, ACMNode)
      else
        begin
          ACMNode := TCMDOMNode.Create(ADomNode.NodeName);
          tempNode := ACMNode;
        end;
      //Text
      if Assigned(ADomNode.Attributes) and Assigned(ADomNode.Attributes.GetNamedItem(FTextAttributesName)) then
        begin  //有子元素则值存于属性中
          textStr := ADomNode.Attributes.GetNamedItem(FTextAttributesName).NodeValue;
        end
      else if Assigned(ADomNode.FirstChild) then
        begin
          textStr := ADomNode.FirstChild.NodeValue;
        end
      else
        begin
          textStr := '';
        end;
      tempNode.Text := textStr;
      //属性
      if Assigned(ADomNode.Attributes) then
      for i:=0 to ADomNode.Attributes.Length-1 do
        begin
          attributeNode := ADomNode.Attributes[i];
          tempNode.Attributes.Add(attributeNode.NodeName + tempNode.Attributes.NameValueSeparator + attributeNode.NodeValue);
        end;
      //孩子们
      vNode := ADomNode.FirstChild;
      while Assigned(vNode) do
        begin
          if (vNode.NodeName <> '#text') and (vNode.NodeName <> '#comment') then
            begin
              if not Dom2CM(vNode, tempNode) then
                Exit;
            end;
          vNode := vNode.NextSibling;
        end;

      Result := True;
    end;
end;

function TCMDOMNodeStreamer.CM2DOM(const ACMNode: TCMDOMNode; var ADOMNode: TDOMNode): Boolean;
var
  theNode, textNode: TDOMNode;
  i: Integer;
  pName, pValue: string;
  theDoc: TDOMDocument;
begin
  Result := False;
  if Assigned(ACMNode) and Assigned(ADOMNode) then
    begin
      if ADOMNode is TDOMDocument then
        begin
          theDoc :=  TDOMDocument(ADOMNode);
          theNode := theDoc.CreateElement(ACMNode.Name);
        end
      else if Assigned(ADOMNode.OwnerDocument) then
        begin
          theDoc := ADOMNode.OwnerDocument;
          theNode := theDoc.CreateElement(ACMNode.Name);
        end
      else
        begin
          raise ECMXMLException.CreateFmt('DOMNode %s 没有TDOMDocument.', [ADOMNode.NodeName]);
          Exit;
        end;
      ADOMNode.AppendChild(theNode);
      //
      if ACMNode.HasChildNodes then
        begin
          if ACMNode.Text <> '' then
            TDOMElement(theNode).SetAttribute(FTextAttributesName, ACMNode.Text);
        end
      else
        begin
          textNode := theDoc.CreateTextNode(ACMNode.Text);
          theNode.Appendchild(textNode);
        end;
      //属性
      for i := 0 to ACMNode.Attributes.Count - 1 do
        begin
          pName := ACMNode.Attributes.Names[i];
          if not IsXmlName(pName) then
            begin
              raise ECMXMLException.CreateFmt('元素 %s 的属性名 %s 不符合规范.', [ACMNode.Name, pName]);
              Exit;
            end;
          pValue := ACMNode.Attributes.ValueFromIndex[i];
          TDOMElement(theNode).SetAttribute(pName, pValue);
        end;
      //孩子们
      for i:=0 to ACMNode.ChildNodes.Count-1 do
        begin
          if not CM2DOM(ACMNode.ChildNodes[i], theNode) then
            Exit;
        end;

      Result := True;
    end;
end;

function TCMDOMNodeStreamer.ReadXML(out AMCNode: TCMDOMNode; AStream: TStream): Boolean;
var
  Doc: TXMLDocument;
  rootNode: TDOMNode;
begin
  Result := False;
  try
    Doc := TXMLDocument.Create;
    try
      ReadXMLFile(Doc, AStream);
      rootNode := Doc.DocumentElement;
      AMCNode := nil;
      if not Dom2CM(rootNode, AMCNode) then
        Exit;
      Result := Assigned(AMCNode);
    finally
      if Assigned(Doc) then
        Doc.Free;
    end;
  except
    on e: Exception do
      begin
        raise ECMXMLException.CreateFmt('ReadXML(): %s %s.', [e.ClassName, e.Message]);
      end;
  end;
end;

function TCMDOMNodeStreamer.ReadXML(out AMCNode: TCMDOMNode; const AFileName: string): Boolean;
var
  FileStream: TStream;
begin
  Result := False;
  if not FileExistsUTF8(AFileName) then
    begin
      raise ECMXMLException.CreateFmt('XML文件 %s 不存在！', [AFileName]);
      Exit;
    end;
  FileStream := TFileStreamUTF8.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    Result := ReadXML(AMCNode, FileStream);
  finally
    FileStream.Free;
  end;
end;

function TCMDOMNodeStreamer.WriteXML(const AMCNode: TCMDOMNode; AStream: TStream): Boolean;
var
  Doc: TXMLDocument;
begin
  Result := False;
  try
    Doc := TXMLDocument.Create;
    try
      if not CM2DOM(AMCNode, TDOMNode(Doc)) then
        Exit;
      WriteXMLFile(Doc, AStream, []);
      Result := Assigned(AStream);
    finally
      if Assigned(Doc) then
        Doc.Free;
    end;
  except
    on e: Exception do
      begin
        raise ECMXMLException.CreateFmt('WriteXML(): %s %s.', [e.ClassName, e.Message]);
      end;
  end;
end;

function TCMDOMNodeStreamer.WriteXML(const AMCNode: TCMDOMNode; const AFileName: string): Boolean;
var
  fs: TFileStreamUTF8;
begin
  Result := False;
  fs := TFileStreamUTF8.Create(AFileName, fmCreate);
  try
    Result := WriteXML(AMCNode, fs);
  finally
    fs.Free;
  end;
end;

function TCMDOMNodeStreamer.NodeToStr(const AMCNode: TCMDOMNode): string;
var
  m: TStringStream;
begin
  Result := '';
  m := TStringStream.Create('');
  try
    if Self.WriteXML(AMCNode, m) then
      begin
        Result := m.DataString;
      end;
  finally
    m.Free;
  end;
end;

function TCMDOMNodeStreamer.StrToNode(const AMCNStr: string): TCMDOMNode;
var
  m: TStringStream;
begin
  Result := nil;
  m := TStringStream.Create(AMCNStr);
  try
    Self.ReadXML(Result, m);
  finally
    m.Free;
  end;
end;



end.

