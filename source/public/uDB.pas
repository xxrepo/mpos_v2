unit uDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db,
  cm_DB;

type

  IPOSStatement = interface(ICMSQLiteStatement)
    ['{16515A32-FDDD-4047-B0A1-EE0992C8B55F}']
    function Query(const ASQLStr: string; out ex: Exception; ADataSetOwner: TComponent=nil): TDataSet; overload;
    //function QueryBySqliteDataSet(const ASQLStr: string; out ex: Exception; ADataSetOwner: TComponent=nil): TDataSet; overload;
    function Execute(const ASQLStr: string; out ex: Exception): Boolean; overload;
    function ExecuteBySQLQuery(const ASQLStr: string; out ex: Exception): Boolean; overload;
    function ExecuteBySqliteConnection(const ASQLStr: string; out ex: Exception): Boolean; overload;
  end;

  IPOSDBExtensionMethod = interface
    ['{1A2565F3-92E9-49C9-95B7-B15217A6DF20}']
    //内存数据库相关////////////////////////
    function CreateMemDb: boolean;    //建一个空内存库
    function DestoryMemDb: boolean;   //销毁一个内存库
    function AttachDb: boolean;
    function MemDbExecSql(vSql: PChar): boolean;   //内存库执行一条语句
    function MemDbExecSql(vSqlList: TStringList): boolean;  //内存库批量执行语句
    function MemDbExecSqlOnLock(vSql: PChar; boResetConn: boolean): boolean;
    //内存库在锁中执行语句 （和实体表交换数据时专用）
    function MemDbExecSqlOnLock(vSqlList: TStringList; boResetConn: boolean): boolean;
    //内存库在锁中执行语句 （和实体表交换数据时专用）
    ////////////////////////////////////////
  end;


implementation

end.

