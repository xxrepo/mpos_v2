unit uDBInitDAOImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  uDBInitDAO, uDAO, uDB, uDBUtils, uApp;

type

  { TDBInitDAO }

  TDBInitDAO = class(TPOSDAO, IDBInitDAO)
  public
    //function dbFileExists:boolean;  //数据库文件是否存在
    //function backupDataBase:boolean;
    //function submitUnCompliteTask:boolean;               //提交数据库中末完成的所有待上传任务
    ////内存数据库相关////////////////////////
    //function CreateMemDb: boolean;    //建一个空内存库
    //function DestoryMemDb: boolean;   //销毁一个内存库
    //function AttachDb: boolean;
    //function MemDbExecSql(vSql: PChar): boolean;   //内存库执行一条语句
    //function MemDbExecSql(vSqlList: TStringList): boolean;  //内存库批量执行语句
    //function MemDbExecSqlOnLock(vSql: PChar; boResetConn: boolean): boolean;
    ////内存库在锁中执行语句 （和实体表交换数据时专用）
    //function MemDbExecSqlOnLock(vSqlList: TStringList; boResetConn: boolean): boolean;
  end;

  { TDBinitBasicDAO }

  TDBinitBasicDAO = class
  private
    POSDBExtensionMethod:IPOSDBExtensionMethod;
    DBInitDAO: IDBInitDAO;
  public
    constructor Create;
    destructor Destory;
    function GetDBHelper: TPOSDBHelper;
    function dbFileExists:boolean;  //数据库文件是否存在
    function backupDataBase:boolean;
    function submitUnCompliteTask:boolean;   //提交数据库中末完成的所有待上传任务
    //实体数据库相关
    function Execute(const ASQL: string; out ex: Exception): Boolean;
    function Execute(const ASQL: string): Boolean;
    function Query(const ASQL: string): TDataSet;
    function Query(const ASQL: string; out ex: Exception): TDataSet;
    //内存数据库相关////////////////////////
    function CreateMemDb: boolean;    //建一个空内存库
    function DestoryMemDb: boolean;   //销毁一个内存库
    function AttachDb: boolean;
    function MemDbExecSql(vSql: string): boolean;   //内存库执行一条语句
    function MemDbExecSql(vSqlList: TStringList): boolean;  //内存库批量执行语句
    function MemDbExecSqlOnLock(vSql: string; boResetConn: boolean): boolean;
    //内存库在锁中执行语句 （和实体表交换数据时专用）
    function MemDbExecSqlOnLock(vSqlList: TStringList; boResetConn: boolean): boolean;
  end;

implementation



{ TDBinitBasicDAO }

constructor TDBinitBasicDAO.Create;
begin
  TPOSDAOFactory.GetInstance.OutDAO(TDBInitDAO,IDBInitDAO,DBInitDAO);
  InterfaceRegister.OutInterface(IPOSDBExtensionMethod,POSDBExtensionMethod);
end;

destructor TDBinitBasicDAO.Destory;
begin
  if Assigned(DBInitDAO) then FreeAndNil(DBInitDAO);
end;

function TDBinitBasicDAO.GetDBHelper: TPOSDBHelper;
begin
  Result:= DBInitDAO.GetDBHelper;
end;

function TDBinitBasicDAO.dbFileExists: boolean;
begin
  Result := False;
end;

function TDBinitBasicDAO.backupDataBase: boolean;
begin
  Result := True;
end;

function TDBinitBasicDAO.submitUnCompliteTask: boolean;
begin
  Result := True;
end;

function TDBinitBasicDAO.Execute(const ASQL: string; out ex: Exception
  ): Boolean;
begin
  Result :=GetDBHelper.Execute(ASQL,Ex);
end;

function TDBinitBasicDAO.Execute(const ASQL: string): Boolean;
begin
  Result := GetDBHelper.Execute(ASQL);
end;

function TDBinitBasicDAO.Query(const ASQL: string): TDataSet;
begin
  result := GetDBHelper.Query(ASQL);
end;

function TDBinitBasicDAO.Query(const ASQL: string; out ex: Exception): TDataSet;
begin
  result :=GetDBHelper.Query(ASQL,ex);
end;

function TDBinitBasicDAO.CreateMemDb: boolean;
begin
  Result := False;
  if Assigned(POSDBExtensionMethod) then
    result := POSDBExtensionMethod.CreateMemDb;
end;

function TDBinitBasicDAO.DestoryMemDb: boolean;
begin
  Result := False;
  if Assigned(POSDBExtensionMethod) then
    result := POSDBExtensionMethod.DestoryMemDb;
end;

function TDBinitBasicDAO.AttachDb: boolean;
begin
  Result := False;
  if Assigned(POSDBExtensionMethod) then
    result := POSDBExtensionMethod.AttachDb;
end;

function TDBinitBasicDAO.MemDbExecSql(vSql: string): boolean;
begin
  Result := False;
  if Assigned(POSDBExtensionMethod) then
    result := POSDBExtensionMethod.MemDbExecSql(PChar(vSql));
end;

function TDBinitBasicDAO.MemDbExecSql(vSqlList: TStringList): boolean;
begin
  Result := False;
  if Assigned(POSDBExtensionMethod) then
    result := POSDBExtensionMethod.MemDbExecSql(vSqlList);
end;

function TDBinitBasicDAO.MemDbExecSqlOnLock(vSql: string; boResetConn: boolean
  ): boolean;
begin
  Result := False;
  if Assigned(POSDBExtensionMethod) then
    result := POSDBExtensionMethod.MemDbExecSqlOnLock(PChar(vSql),boResetConn);
end;

function TDBinitBasicDAO.MemDbExecSqlOnLock(vSqlList: TStringList;
  boResetConn: boolean): boolean;
begin
  Result := False;
  if Assigned(POSDBExtensionMethod) then
    result := POSDBExtensionMethod.MemDbExecSqlOnLock(vSqlList,boResetConn);
end;

end.

