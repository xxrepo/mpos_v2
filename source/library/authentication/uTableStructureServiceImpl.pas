unit uTableStructureServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  uDAO, cm_Messager, cm_DB,
  uTableStructureService;

type

  { TTableStructureService }

  TTableStructureService = class(TCMMessageable, ITableStructureService)
    function AddTableField(ATableName, AFieldName, AFieldType: string; sta: ICMStatement = nil): boolean; //数字库增加一个字段
    function CheckTableFieldName(ATableName, AFieldName: string; sta: ICMStatement = nil): boolean;       //检查数据库字段是否存在
    function CheckTableRowCount(ATableName: string; sta: ICMStatement = nil): integer;                    //表有多少行
    function CheckTableisExistsAndCreate(ATableName, ASQL: string; sta: ICMStatement = nil): boolean;     //检查表是否存在并新建一个表
  end;

const
  TableInfoSQLFmt = 'PRAGMA TABLE_INFO([%s])';
  TableAddFieldSQLFmt = 'ALTER TABLE %s ADD %s %s ;';
  CheckTableExistsSQLFmt = 'SELECT name FROM sqlite_master WHERE type=''table'' AND name=%s COLLATE NOCASE ;';
  CheckTableRowCountSQLFmt = 'SELECT Count(*) FROM %s ;';

implementation

uses
  StrUtils;

{ TTableStructureService }

function TTableStructureService.CheckTableFieldName(ATableName, AFieldName: string; sta: ICMStatement = nil): boolean;
var
  ds: TDataSet;
begin
  Result := False;
  if (sta = nil) then
    Exit;

  try
    ds := sta.Query(format(TableInfoSQLFmt, [ATableName]));
    if not ds.IsEmpty then
    begin
      ds.First;
      while not ds.EOF do
      begin
        if SameText(Trim(ds.FieldByName('name').AsString), Trim(AFieldName)) then
        begin
          Messager.Debug('CheckTableFieldName 表 ' + ATableName + ' 存在字段名:' + AFieldName);
          Result := True;
          Exit;
        end;
        ds.Next;
      end;
    end;
    Messager.Debug('CheckTableFieldName 表 ' + ATableName + ' 不存在字段名:' + AFieldName);
  finally
    ds.Free;
  end;
end;

function TTableStructureService.CheckTableRowCount(ATableName: string; sta: ICMStatement): integer;
var
  ds: TDataSet;
  tSql: string;
  tmpCnt: integer;
begin
  Result := -1;
  try
    ds := sta.Query(Format(CheckTableRowCountSQLFmt, [ATableName]));
    if not ds.IsEmpty then
      Result := ds.Fields[0].AsInteger;
  finally
    ds.Free;
  end;

end;

function TTableStructureService.AddTableField(ATableName, AFieldName, AFieldType: string; sta: ICMStatement = nil): boolean;
var
  tSql: string;
begin
  Result := False;
  if (sta = nil) then
    Exit;

  //如果不存在字段，则增加这个字段
  if not Self.CheckTableFieldName(ATableName, AFieldName) then
  begin
    try
      sta.Execute(format(TableAddFieldSQLFmt, [ATableName, AFieldName, AFieldType]));
      Result := True;
    except
      on e: Exception do
        Messager.Error('AddTableField: ', e);
    end;
  end;
end;

function TTableStructureService.CheckTableisExistsAndCreate(ATableName, ASQL: string; sta: ICMStatement): boolean;
var
  ds: TDataSet;
  tSql: string;
begin
  Result := False;
  try
    ds := sta.Query(Format(CheckTableExistsSQLFmt, [QuotedStr(ATableName)]));
    if ds.IsEmpty then
    begin
      Messager.Debug('CheckTableisExistsAndCreate 没有找到表:' + ATableName + ',开始执行建表语句。');
      if sta.Execute(ASql) then
      begin
        Messager.Debug('psf.CheckTableisExistsAndCreate 建表:' + ATableName + '成功');
        Exit;
      end
      else
        Messager.Error('psf.CheckTableisExistsAndCreate 建表:' + ATableName + '失败');
      Exit;
    end;
    Messager.Debug('psf.CheckTableisExistsAndCreate 找到表:' + ATableName + ',无须建表');
  finally
    ds.Free;
  end;

end;

end.
