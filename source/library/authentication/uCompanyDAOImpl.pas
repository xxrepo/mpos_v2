unit uCompanyDAOImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults, Forms,
  uCompany, Dialogs, LazFileUtils,
  cm_DOM, cm_XML, cm_dialogs, cm_messager,
  cm_parameter, cm_ParameterUtils;

const
  CompanyFile: string = 'config/company.xml';
  CompanysNode: string = 'Companys';

type
  TCompanyFileDAO = class(TCMMessageable, ICompanyDAO)
  private
    FCompanyFilePath: string;
  public
  published
    property CompanyFilePath: string read FCompanyFilePath write FCompanyFilePath;
  public
    function FindByCode(const ACode: string): TCompany;
    function GetList(): TCompanyList;
  end;

implementation

function TCompanyFileDAO.FindByCode(const ACode: string): TCompany;
var
  CompanyList: TObjectList<TCompany>;
  Company: TCompany;
  i: integer;
begin
  Result := nil;

  CompanyList := GetList();
  try
    if CompanyList.Count = 0 then
      Exit;

    for i := 0 to CompanyList.Count - 1 do
      if (CompanyList[i].Code = ACode) then
      begin
        Company := TCompany.Create;
        Company.Code := CompanyList[i].Code;
        Company.Name := CompanyList[i].Name;
        Company.AbbrName := CompanyList[i].AbbrName;
        Company.AreaConfig := CompanyList[i].AreaConfig;
        Result := Company;
      end;
  finally
    CompanyList.Free;
  end;

end;

function TCompanyFileDAO.GetList(): TCompanyList;
var
  Company: TCompany;
  ns: TCMDOMNodeStreamer;
  node: TCMDOMNode;
  cnode: TCMDOMNode;
  i: integer;
  fn: string;
  paramLoad: TCMParameter;
  p: ICMParameter;
begin
  Result := TObjectList<TCompany>.Create;
  ns := TCMDOMNodeStreamer.Create(nil);
  Messager.Info('开始初始化参数工具...');
  try
    Messager.Info('开始加载默认配置参数...');
    if not FileExistsUTF8(CompanyFile) then
    begin
      Messager.Info('配置文件:' + CompanyFile + '不存在.');
      //AppSystem.GetMsgBox.ShowMessage('默认配置文件:' + CompanyFile + '不存在.');
      Exit;
    end;

    if ns.ReadXML(node, CompanyFile) then
    begin
      for i := 0 to node.ChildCount - 1 do
      begin
        cnode := node.ChildNodes.Items[i];
        Company := TCompany.Create;
        Company.Code := cnode.GetAttribute('Name');
        Company.Name := cnode.GetAttribute('CompName');
        Company.AbbrName := cnode.GetAttribute('AbbrName');
        Company.AreaConfig := cnode.GetAttribute('AreaConfig');
        Result.Add(Company);
      end;
      node.Free;
    end;
  finally
    ns.Free;
  end;

end;


end.















