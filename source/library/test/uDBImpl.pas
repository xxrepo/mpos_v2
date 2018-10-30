unit uDBImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, syncobjs,
  cm_interfaces, cm_messager, cm_sysutils, cm_SqliteDB,
  uDB,
  sqlite3, Sqlite3Conn;

type

  { TPOSSQLMessageFormatter }

  TPOSSQLMessageFormatter = class(TCMBase, ICMMessageFormatter)
  public
    function Format(ARecord: ICMMessageRecord): string;
  end;

  TPOSDBExtensionMethodProxy = class;

  { TPOSStatement }

  TPOSStatement = class(TCMSQLiteStatement, IPOSStatement, IPOSDBExtensionMethod)
  private
    function SelDoExecute(sel: Word; const AMsgTitle, ASQLStr: string; out ex: Exception): Boolean;
  public //IPOSStatement
    function Query(const ASQLStr: string; out ex: Exception; ADataSetOwner: TComponent=nil): TDataSet; overload;
    //function QueryBySqliteDataSet(const ASQLStr: string; out ex: Exception; ADataSetOwner: TComponent=nil): TDataSet; overload;
    function Execute(const ASQLStr: string; out ex: Exception): Boolean; overload;
    function ExecuteBySQLQuery(const ASQLStr: string; out ex: Exception): Boolean; overload;
    function ExecuteBySqliteConnection(const ASQLStr: string; out ex: Exception): Boolean; overload;
  private
    FExtensionMethodProxy: TPOSDBExtensionMethodProxy;
    procedure DoCheckProxy;
  public //IPOSDBExtensionMethod
    function CreateMemDb: boolean;    //建一个空内存库
    function DestoryMemDb: boolean;   //销毁一个内存库
    function AttachDb: boolean;
    function MemDbExecSql(vSql: PChar): boolean;   //内存库执行一条语句
    function MemDbExecSql(vSqlList: TStringList): boolean;  //内存库批量执行语句
    function MemDbExecSqlOnLock(vSql: PChar; boResetConn: boolean): boolean;
    function MemDbExecSqlOnLock(vSqlList: TStringList; boResetConn: boolean): boolean;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TPOSDBExtensionMethodProxy
    // 这里的实现是复制过来的
  }
  TPOSDBExtensionMethodProxy = class(TCMMessageableComponent, IPOSDBExtensionMethod)
  private
    //内存库
    MemDb: psqlite3;
    FDBFileName: string;
    FSQLConn: TSQLite3Connection;
    //线程锁
    ThreadLock: TCriticalSection;
  public
    constructor Create(AOwner: TComponent; const ADBFileName: string; ASQLiteConn: TSQLite3Connection; ACriticalSection: TCriticalSection); reintroduce;
  public
    function CreateMemDb: boolean;    //建一个空内存库
    function DestoryMemDb: boolean;   //销毁一个内存库
    function AttachDb: boolean;
    function MemDbExecSql(vSql: PChar): boolean;   //内存库执行一条语句
    function MemDbExecSql(vSqlList: TStringList): boolean;  //内存库批量执行语句
    function MemDbExecSqlOnLock(vSql: PChar; boResetConn: boolean): boolean;
    function MemDbExecSqlOnLock(vSqlList: TStringList; boResetConn: boolean): boolean;
  end;

implementation

{ TPOSSQLMessageFormatter }

function TPOSSQLMessageFormatter.Format(ARecord: ICMMessageRecord): string;
var
  ensql: string;
  mr: ICMDBMessageRecord;
  function EncodeSQL(const AStr: string): string;
  begin
    //Result := EncodeHexString(AStr);
    Result := AStr;
  end;
begin
  ensql := '';
  if Supports(ARecord, ICMDBMessageRecord, mr) then
    begin
      ensql := EncodeSQL(mr.GetSQL);
      ensql := SysUtils.Format(LineEnding + '[%s]', [ensql]);
    end;
  if ARecord.GetTitle = '' then
    Result := ARecord.GetSource + ':' + ARecord.GetMessage + ensql
  else
    Result := ARecord.GetSource + ':' + ARecord.GetTitle + ' ' + ARecord.GetMessage + ensql;
end;

{ TPOSStatement }

constructor TPOSStatement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtensionMethodProxy := nil;
end;

function TPOSStatement.Query(const ASQLStr: string; out ex: Exception; ADataSetOwner: TComponent): TDataSet;
var
  msgTitle: string;
begin
  Result := nil;
  Self.GetCriticalSection.Enter;
  try
    try
      msgTitle := GetExecuteMsgTitle('[out ex]查询SQL');
      Result := DoQuery(msgTitle, ASQLStr, ADataSetOwner);
    except
      on e1: EDatabaseError do
        begin
          Messager.Error(msgTitle + '失败。', e1);
          if Assigned(Result) then
            Result.Close;
          ex := EDatabaseError.Create(e1.Message);
        end;
      on e2: Exception do
        begin
          Messager.Error(msgTitle + '出错:。', e2);
          if Assigned(Result) then
            Result.Close;
          ex := Exception(e2.ClassType.Create);
          ex.Message := e2.Message;
        end;
    end;
  finally
    Self.GetCriticalSection.Leave;
  end;
end;

//function TPOSStatement.QueryBySqliteDataSet(const ASQLStr: string; out ex: Exception; ADataSetOwner: TComponent): TDataSet;
//var
//  msgTitle: string;
//begin
//  Result := nil;
//  Self.GetCriticalSection.Enter;
//  try
//    try
//      msgTitle := GetExecuteMsgTitle('[SqliteDataSet/out ex]查询SQL');
//      Result := DoQueryBySqliteDataSet(msgTitle, ASQLStr, ADataSetOwner);
//    except
//      on e1: EDatabaseError do
//        begin
//          Messager.Error(msgTitle + '失败。', e1);
//          if Assigned(Result) then
//            Result.Close;
//          ex := EDatabaseError.Create(e1.Message);
//        end;
//      on e2: Exception do
//        begin
//          Messager.Error(msgTitle + '出错:。', e2);
//          if Assigned(Result) then
//            Result.Close;
//          ex := Exception(e2.ClassType.Create);
//          ex.Message := e2.Message;
//        end;
//    end;
//  finally
//    Self.GetCriticalSection.Leave;
//  end;
//end;

//private
function TPOSStatement.SelDoExecute(sel: Word; const AMsgTitle, ASQLStr: string; out ex: Exception): Boolean;
begin
  Result := False;
  ex := nil;
  Self.GetCriticalSection.Enter;
  try
    try
      case sel of
      1: Result := DoExecute(AMsgTitle, ASQLStr);
      2: Result := DoExecuteBySQLQuery(AMsgTitle, ASQLStr);
      3: Result := DoExecuteBySqliteConnection(AMsgTitle, ASQLStr);
      end;
    except
      on e1: EDatabaseError do
        begin
          Result := False;
          Messager.Error(AMsgTitle + '失败。', e1);
          Self.Connection.SQLTran.Rollback;
          ex := EDatabaseError.Create(e1.Message);
        end;
      on e2: Exception do
        begin
          Result := False;
          Messager.Error(AMsgTitle + '出错:。', e2);
          Self.Connection.SQLTran.Rollback;
          ex := Exception(e2.ClassType.Create);
          ex.Message := e2.Message;
        end;
    end;
  finally
    Self.GetCriticalSection.Leave;
  end;
end;

function TPOSStatement.Execute(const ASQLStr: string; out ex: Exception): Boolean;
begin
  Result := SelDoExecute(1, GetExecuteMsgTitle('[out ex]执行SQL'), ASQLStr, ex);
end;

function TPOSStatement.ExecuteBySQLQuery(const ASQLStr: string; out ex: Exception): Boolean;
begin
  Result := SelDoExecute(2, GetExecuteMsgTitle('[SQLQuery/out ex]执行SQL'), ASQLStr, ex);
end;

function TPOSStatement.ExecuteBySqliteConnection(const ASQLStr: string; out ex: Exception): Boolean;
begin
  Result := SelDoExecute(3, GetExecuteMsgTitle('[SqliteConnection/out ex]执行SQL'), ASQLStr, ex);
end;

(*****************************************************************************************************)

procedure TPOSStatement.DoCheckProxy;
begin
  if not Assigned(FExtensionMethodProxy) then
    begin
      Messager.Debug('初始化Sqlite3结果:' + IntToStr(InitialiseSQLite));
      FExtensionMethodProxy := TPOSDBExtensionMethodProxy.Create(Self, Connection.DatabaseName, Connection.SQLiteConn, Self.GetCriticalSection);
    end;
end;

function TPOSStatement.CreateMemDb: Boolean;
begin
  Result := False;
  DoCheckProxy;
  Messager.Debug('CreateMemDb()...');
  Result := FExtensionMethodProxy.CreateMemDb;
  Messager.Debug('end CreateMemDb().');
end;

function TPOSStatement.DestoryMemDb: Boolean;
begin
  Messager.Debug('DestoryMemDb()...');
  Result := FExtensionMethodProxy.DestoryMemDb;
  Messager.Debug('end DestoryMemDb().');
end;

function TPOSStatement.AttachDb: Boolean;
begin
  DoCheckProxy;
  Messager.Debug('AttachDb()...');
  Result := FExtensionMethodProxy.AttachDb;
  Messager.Debug('end AttachDb().');
end;

function TPOSStatement.MemDbExecSql(vSql: PChar): Boolean;
begin
  DoCheckProxy;
  Messager.Debug('MemDbExecSql(PChar)...');
  Result := FExtensionMethodProxy.MemDbExecSql(vSql);
  Messager.Debug('end MemDbExecSql(PChar).');
end;

function TPOSStatement.MemDbExecSql(vSqlList: TStringList): Boolean;
begin
  DoCheckProxy;
  Messager.Debug('MemDbExecSql(TStringList)...');
  Result := FExtensionMethodProxy.MemDbExecSql(vSqlList);
  Messager.Debug('end MemDbExecSql(TStringList).');
end;

function TPOSStatement.MemDbExecSqlOnLock(vSql: PChar; boResetConn: Boolean): Boolean;
begin
  DoCheckProxy;
  Messager.Debug('MemDbExecSqlOnLock(PChar)...');
  Result := FExtensionMethodProxy.MemDbExecSqlOnLock(vSql, boResetConn);
  Messager.Debug('end MemDbExecSqlOnLock(PChar).');
end;

function TPOSStatement.MemDbExecSqlOnLock(vSqlList: TStringList; boResetConn: Boolean): Boolean;
begin
  DoCheckProxy;
  Messager.Debug('MemDbExecSqlOnLock(TStringList)...');
  Result := FExtensionMethodProxy.MemDbExecSqlOnLock(vSqlList, boResetConn);
  Messager.Debug('end MemDbExecSqlOnLock(TStringList).');
end;

{TPOSDBExtensionMethodProxy}

constructor TPOSDBExtensionMethodProxy.Create(AOwner: TComponent; const ADBFileName: string; ASQLiteConn: TSQLite3Connection; ACriticalSection: TCriticalSection);
begin
  inherited Create(AOwner);
  MemDb := nil;
  FDBFileName := ADBFileName;
  FSQLConn := ASQLiteConn;
  ThreadLock := ACriticalSection;
end;

//内存数据库相关////////////////////////
function TPOSDBExtensionMethodProxy.CreateMemDb: Boolean;    //建一个空内存库（包含attack一个实体库）
var
  ermsg: PChar;
  strl: string;
begin
  Result := False;
  try
    ermsg := nil;
    //建一个内存库
    if sqlite3_open(':memory:', @MemDb) = SQLITE_OK then
    begin
      if ermsg <> nil then
      begin
        strl := strpas(ermsg);
        Messager.Error('错误 建立内存数据库出错:' + strl);
        sqlite3_free(ermsg);
        Exit;
      end;
      Result := True;
      Messager.Info('建立内存数据库完成');
    end; //建库end
  except
    on e: Exception do
      Messager.Error('异常 建立内存数据库异常:' + e.Message);
  end;
end;

function TPOSDBExtensionMethodProxy.DestoryMemDb: Boolean;   //销毁一个内存库
begin
  Result := False;
  if MemDb = nil then
  begin
    Result := True;
    Exit;
  end;
  try
    if sqlite3_close(MemDb) = SQLITE_OK then
    begin
      Result := True;
      Messager.Info('销毁内存数据库成功');
    end;
  except
    on e: Exception do
      Messager.Error('异常 销毁内存数据库异常:' + e.Message);
  end;
end;

function TPOSDBExtensionMethodProxy.AttachDb: Boolean;
var
  ermsg: PChar;
  strl: string;
  tSql: string;
begin
  Result := False;
  //开始附加实体数据库
  try
    tSql := 'attach ' + QuotedStr(FDBFileName) + ' as posdb ;';
    if sqlite3_exec(MemDb, PChar(tSql), nil, nil, @ermsg) = SQLITE_OK then
    begin
      if ermsg <> nil then
      begin
        strl := strpas(ermsg);
        Messager.Error('错误 附加实体数据库失败:' + strl);
        sqlite3_free(ermsg);
        Exit;
      end;
      Result := True;
      Messager.Info('附加实体数据库完成');
    end;  //附加end

  except
    on e: Exception do
      Messager.Error('异常 附加实体数据库异常:' + e.Message);
  end;
end;

function TPOSDBExtensionMethodProxy.MemDbExecSql(vSql: PChar): Boolean;   //内存库执行一条语句
var
  ermsg: PChar;
  strl: string;
begin
  Result := False;
  try
    ermsg := nil;
    //开始事务
    if sqlite3_exec(memdb, PChar('begin transaction'), nil, nil, @ermsg) = SQLITE_OK then
    begin
      if ermsg <> nil then
      begin
        strl := strpas(ermsg);
        Messager.Error('错误 内存数据库 begin事务开始错误:' + strl);
        sqlite3_free(ermsg);
        Exit;
      end;
    end
    else
    begin
      Messager.Error('错误 内存数据库 begin事务失败');
      Exit;
    end;
    //开始执行语句
    if sqlite3_exec(MemDb, vSql, nil, nil, @ermsg) = SQLITE_OK then
    begin
      if ermsg <> nil then
      begin
        strl := strpas(ermsg);
        Messager.Error('错误 内存数据库 执行语句错误:' + strl);
        sqlite3_free(ermsg);
        if sqlite3_exec(MemDb, PChar('rollback transaction'), nil, nil, @ermsg) = SQLITE_OK then
          Messager.Info('内存数据库回滚成功');
        if ermsg <> nil then
          sqlite3_free(ermsg);
        Messager.Error('内存数据库执行Sql失败:' + StrPas(vSql));
        Exit;
      end;
    end
    else
    begin
      Messager.Error('错误 内存数据库 执行语句失败:' + StrPas(vSql));
      Exit;
    end;
    //事务提交
    if sqlite3_exec(memdb, PChar('commit transaction'), nil, nil, @ermsg) = SQLITE_OK then
    begin
      if ermsg <> nil then
      begin
        strl := strpas(ermsg);
        Messager.Error('错误 内存数据库 commit事务提交错误:' + strl);
        sqlite3_free(ermsg);
        if sqlite3_exec(memdb, PChar('rollback transaction'), nil, nil, @ermsg) = SQLITE_OK then
          Messager.Info('内存数据库回滚成功');
        if ermsg <> nil then
          sqlite3_free(ermsg);
        Exit;
      end;
      Result := True;
      //FLog.Log(0,pchar('执行语句成功'));
    end
    else
    begin
      Messager.Error('错误 内存数据库 commit事务提交失败');
      Exit;
    end;

  except
    on e: Exception do
      Messager.Error('异常 内存数据库 MemDbExecSql-->' + e.Message);
  end;
end;

function TPOSDBExtensionMethodProxy.MemDbExecSql(vSqlList: TStringList): Boolean;  //内存库批量执行语句
begin
  Result := MemDbExecSql(PChar(vSqlList.Text));
end;

function TPOSDBExtensionMethodProxy.MemDbExecSqlOnLock(vSql: PChar; boResetConn: Boolean): Boolean;
  //内存库在锁中执行语句 （和实体表交换数据时专用）
begin
  Result := False;
  ThreadLock.Enter;
  try
    if boResetConn then
    begin
      try
        FSQLConn.Close();
      except
        on e: Exception do
        begin
          Messager.Info('异常 断开实体库失败-->' + e.Message);
          FSQLConn.Connected := True;   //发生异常的情况下，确保数据库要重新连接
          Exit;
        end;
      end;
      Messager.Info('断开实体库连接成功');
    end;
    try
      Result := MemDbExecSql(vSql);
    except
    end;
    if not FSQLConn.Connected then
    begin
      FSQLConn.Connected := True;
      Messager.Info('实体库重新连接成功');
    end;
  finally
    //确保数据库要重新连接
    try
      if not FSQLConn.Connected then
        FSQLConn.Connected := True;
    except
    end;
    ThreadLock.Leave;
  end;
end;

function TPOSDBExtensionMethodProxy.MemDbExecSqlOnLock(vSqlList: TStringList; boResetConn: Boolean): Boolean;
  //内存库在锁中执行语句 （和实体表交换数据时专用）
begin
  Result := MemDbExecSqlOnLock(PChar(vSqlList.Text), boResetConn);
end;





end.

