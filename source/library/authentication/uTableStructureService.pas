unit uTableStructureService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_DB, cm_interfaces;

type
  ITableStructureService = interface(ICMBase)
    ['{0DE7BC86-3BAE-4145-AC42-2B9D9E7E4259}']
    function AddTableField(ATableName, AFieldName, AFieldType: string; sta: ICMStatement = nil): boolean; //数字库增加一个字段
    function CheckTableFieldName(ATableName, AFieldName: string; sta: ICMStatement = nil): boolean;       //检查数据库字段是否存在
    function CheckTableRowCount(ATableName: string; sta: ICMStatement = nil): integer;                    //表有多少行
    function CheckTableisExistsAndCreate(ATableName, ASQL: string; sta: ICMStatement = nil): boolean;     //检查表是否存在并新建一个表

  end;

implementation

end.
