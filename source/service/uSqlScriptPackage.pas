unit uSqlScriptPackage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces;

type
                         //初始化     数据包      sql语句
  enmuSQLScriptType = (sstNone,sstdbinit,sstsqlpackage,sstsqlscript);

  ISQLScriptPackage = interface(ICMBase)
    ['{13AB6C1D-B611-4A87-BA70-053A2026A2F2}']
    //任务初始化
    function init:boolean;
    //设置任务类型
    function setSQLScriptType(sst:enmuSQLScriptType):boolean;
    //增加一个文件包
    function addFilePackage(fileName:string):boolean;
  end;

implementation

end.

