unit uDBInitDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uDAO;

type
  IDBInitDAO = interface(IPOSDAO)
    ['{15832B36-DC71-4F80-9374-EDAE89652DB3}']
    //function submitUnCompliteTask:boolean;               //提交数据库中末完成的所有待上传任务
    //function backupDataBase:boolean;                     //备份当前数据库
    ////内存数据库相关////////////////////////
    //function CreateMemDb: boolean;    //建一个空内存库
    //function DestoryMemDb: boolean;   //销毁一个内存库
    //function AttachDb: boolean;
    //function MemDbExecSql(vSql: PChar): boolean;   //内存库执行一条语句
    //function MemDbExecSql(vSqlList: TStringList): boolean;  //内存库批量执行语句
    //function MemDbExecSqlOnLock(vSql: PChar; boResetConn: boolean): boolean;
    ////内存库在锁中执行语句 （和实体表交换数据时专用）
    //function MemDbExecSqlOnLock(vSqlList: TStringList; boResetConn: boolean): boolean;
    //
  end;


implementation

end.

