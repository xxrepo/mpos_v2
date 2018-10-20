{
    This file is part of the CM Software Development Kit.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_DB

    This is not a complete unit,
    Here is the copy part of the CMSDK, for demo testing


 **********************************************************************}

unit cm_DB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DB, SQLDB,
  syncobjs,
  cm_interfaces;

type

  ECMDBError = class(EDatabaseError);

  ICMStatement = interface;

  //与特定数据库的连接（会话）。在连接上下文中执行 SQL 语句并返回结果。
  ICMConnection = interface(ICMBase)
    ['{DC60350E-C885-47B8-AF62-1626A0539330}']
    procedure Close;
    function IsClosed: Boolean;
    function CreateStatement: ICMStatement;
    procedure SetAutoCommit(ABool: Boolean);
    function GetAutoCommit: Boolean;
    procedure Commit;
    procedure Rollback;
  end;

  //用于执行静态 SQL 语句并返回它所生成的结果。
  ICMStatement = interface(ICMBase)
    ['{AF973674-24A4-441D-930B-6601742EDE5A}']
    procedure Close;
    function IsClosed: Boolean;
    function GetConnection: ICMConnection;
    function Execute(const ASQLStr: string): Boolean;
    function Query(const ASQLStr: string; ADataSetOwner: TComponent=nil): TDataSet;
    procedure SetQueryTimeout(AInt: Integer);
    function GetQueryTimeout: Integer;
  end;

  //SQLite是基于文件锁的，必要时可能需要其CriticalSection
  ICMSQLiteConnection = interface(ICMConnection)
    ['{1FDB2508-84ED-4F59-8A38-F38D048AA64E}']
    function GetCriticalSection: TCriticalSection;
  end;

  ICMSQLiteStatement = interface(ICMStatement)
    ['{38547CE3-B501-4F85-BDC4-D4C7C32911BA}']
    //function QueryBySqliteDataSet(const ASQLStr: string; ADataSetOwner: TComponent=nil): TDataSet;
    function ExecuteBySQLQuery(const ASQLStr: string): Boolean;
    function ExecuteBySqliteConnection(const ASQLStr: string): Boolean;
    function GetCriticalSection: TCriticalSection;
  end;

const
  DBEventTypeNames: array[TDBEventType] of string = ('detCustom', 'detPrepare', 'detExecute', 'detFetch',
                                                     'detCommit', 'detRollBack', 'detParamValue', 'detActualSQL');

implementation



end.

