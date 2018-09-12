{
    This file is part of the CM Software Development Kit.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_SqliteDB

    This is not a complete unit,
    Here is the copy part of the CMSDK, for demo testing


 **********************************************************************}

unit cm_SqliteDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Sqlite3Conn, sqlite3dyn, //Sqlite3DS,
  db, sqldb, syncobjs,
  FileUtil, LazFileUtils,
  cm_sysutils, cm_messager,
  cm_DB;

type

  { ICMDBMessageRecord }

  ICMDBMessageRecord = interface(ICMMessageRecord)
     ['{2BA0D4A3-FF61-474E-93AD-A38D1986BBD5}']
     function GetSQL: string;
  end;

  { TCMDBMessageRecord }

  TCMDBMessageRecord = class(TCMMessageRecord, ICMDBMessageRecord, ICMMessageRecord)
  private
    FSQL: string;
  protected
    property SQL: string read FSQL write FSQL;
  public
    function GetSQL: string;
  end;

  { TSQLite3ConnectionEx }

  TSQLite3ConnectionEx = class(TSQLite3Connection)
  public
    procedure ExecSQLEx(const ASQL: string);
  end;

  TCMSQLiteStatement = class;

  TCMSQLiteStatementClass = class of TCMSQLiteStatement;

  { TCMSQLiteConnection }

  TCMSQLiteConnection = class(TCMMessageableComponent, ICMSQLiteConnection, ICMConnection)
  private
    FIsSet: Boolean;
  private
    FSQLiteConn: TSQLite3ConnectionEx;
    FSQLTran: TSQLTransaction;
    FCriticalSection: TCriticalSection;
    FStatementClass: TCMSQLiteStatementClass;
    //
    FDatabaseName: string;
    FAutoCommit: Boolean;
    FConnectionLog: Boolean;
    function getSQLiteConn: TSQLite3Connection;
    procedure setDatabaseName(const ADatabaseName: string);
    function getDefaultLibrary: string;
    procedure setDefaultLibrary(const ALibraryName: string);
    function reset: Boolean;
    procedure connLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SQLiteConn: TSQLite3Connection read getSQLiteConn;
    property SQLTran: TSQLTransaction read FSQLTran;
    property DatabaseName: string read FDatabaseName write setDatabaseName;
    property DefaultLibrary: string read getDefaultLibrary write setDefaultLibrary;
    property ConnectionLog: Boolean read FConnectionLog write FConnectionLog;
    procedure SetStatementClass(AClass: TCMSQLiteStatementClass);
  public
    procedure Close;
    function IsClosed: Boolean;
    function CreateStatement: ICMStatement;
    procedure SetAutoCommit(ABool: Boolean);
    function GetAutoCommit: Boolean;
    procedure Commit;
    procedure Rollback;
    function GetCriticalSection: TCriticalSection;
  public
    property AutoCommit: Boolean read FAutoCommit write SetAutoCommit;
  end;

  { TCMSQLiteStatement }

  TCMSQLiteStatement = class(TCMMessageableComponent, ICMSQLiteStatement, ICMStatement)
  private
    FCMSQLiteConnection: TCMSQLiteConnection;
    FScript: TSQLScript;
    FQuery: TSQLQuery;
    FRaiseOnExcute: Boolean;
    procedure setConnection(AValue: TCMSQLiteConnection);
    function SelDoExecute(sel: Word; const AMsgTitle, ASQLStr: string): Boolean;
  protected
    procedure DoSQLMessage(Et: TEventType; const ATitle, AMsg, ASQL: string); virtual;
  protected
    FExecuteCount: Integer;
    function CheckConnectionStatus: Boolean;
    function GetExecuteMsgTitle(const AHead: string): string;
    function DoQuery(const AMsgTitle, ASQLStr: string; ADataSetOwner: TComponent): TDataSet;
    //function DoQueryBySqliteDataSet(const AMsgTitle, ASQLStr: string; ADataSetOwner: TComponent): TDataSet;
    function DoExecute(const AMsgTitle, ASQLStr: string): Boolean;
    function DoExecuteBySQLQuery(const AMsgTitle, ASQLStr: string): Boolean;
    function DoExecuteBySqliteConnection(const AMsgTitle, ASQLStr: string): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Connection: TCMSQLiteConnection read FCMSQLiteConnection write setConnection;
    property RaiseOnExcute: Boolean read FRaiseOnExcute write FRaiseOnExcute;
  public //ICMStatement
    procedure Close;
    function IsClosed: Boolean;
    function GetConnection: ICMConnection;
    function Execute(const ASQLStr: string): Boolean;
    function Query(const ASQLStr: string; ADataSetOwner: TComponent=nil): TDataSet;
    procedure SetQueryTimeout(AInt: Integer);
    function GetQueryTimeout: Integer;
  public //ICMSQLiteStatement
    //function QueryBySqliteDataSet(const ASQLStr: string; ADataSetOwner: TComponent=nil): TDataSet;
    function ExecuteBySQLQuery(const ASQLStr: string): Boolean;
    function ExecuteBySqliteConnection(const ASQLStr: string): Boolean;
    function GetCriticalSection: TCriticalSection;
  end;

const
  CMSQLiteDefaultDBName: string = 'temp.db';

implementation

{ TCMDBMessageRecord }

function TCMDBMessageRecord.GetSQL: string;
begin
  Result := FSQL;
end;

{ TCMSQLiteConnection }

constructor TCMSQLiteConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsSet := False;
  FSQLiteConn := TSQLite3ConnectionEx.Create(Self);
  FSQLTran := TSQLTransaction.Create(Self);
  FCriticalSection := TCriticalSection.Create;
  FStatementClass := TCMSQLiteStatement;
  FDatabaseName := CMSQLiteDefaultDBName;
  FAutoCommit := True;
  //
  FSQLiteConn.OnLog := @connLog;
  FConnectionLog := False;
end;

destructor TCMSQLiteConnection.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TCMSQLiteConnection.SetStatementClass(AClass: TCMSQLiteStatementClass);
begin
  FStatementClass := AClass;
end;

procedure TCMSQLiteConnection.connLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: string);
begin
  //由于编译器问题，在程序关闭时写日志有可能报错(相关对象释放顺序和存在性判断问题)，所以建议仅在调试时开启此功能
  if FConnectionLog then
    Messager.Debug('Connection log:[' + DBEventTypeNames[EventType] + '] ' + Msg);
end;

function TCMSQLiteConnection.reset: Boolean;
begin
  Result := False;
  FCriticalSection.Enter;
  try
    try
      FIsSet := True;
      FSQLiteConn.Close;
      FSQLiteConn.CharSet := string('UTF8');
      FSQLiteConn.DatabaseName := FDatabaseName;
      FSQLiteConn.UserName := '';
      FSQLiteConn.Password := '';
      FSQLiteConn.Transaction := FSQLTran;
      try
        FSQLiteConn.Open;
      except
        on e1: Exception do
          begin
            Messager.Error('创建数据库出错。', e1);
            Exit;
          end;
      end;
      //
      Result := FSQLiteConn.Connected;
    except
      on e3: Exception do
        begin
          Messager.Error('初始化时出错。', e3);
          Exit;
        end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TCMSQLiteConnection.setDatabaseName(const ADatabaseName: string);
begin
  if (ADatabaseName = '') then
    begin
      Messager.Error('设置的DatabaseName不能为空。');
      Exit;
    end;
  try
    try
      FDatabaseName := RepairFileUTF8Path(ADatabaseName);
    finally
      if not FileExists(FDatabaseName) then
        begin
          if ExtractFileDir(FDatabaseName) <> '' then
            if not DirectoryExistsUTF8(ExtractFileDir(FDatabaseName)) then
              ForceDirectoriesUTF8(ExtractFileDir(FDatabaseName));
        end;
    end;
  finally
    reset;
  end;
end;

function TCMSQLiteConnection.getSQLiteConn: TSQLite3Connection;
begin
  Result := FSQLiteConn;
end;

function TCMSQLiteConnection.getDefaultLibrary: string;
begin
  Result := sqlite3dyn.SQLiteDefaultLibrary;
end;

procedure TCMSQLiteConnection.setDefaultLibrary(const ALibraryName: string);
begin
  if (ALibraryName = '') then
    begin
      Messager.Error('设置的SQLiteLibrary不能为空。');
      Exit;
    end;
  try
    sqlite3dyn.SQLiteDefaultLibrary := RepairLibraryFileExt(RepairFilePath(ALibraryName), True);
  finally
    reset;
  end;
end;

procedure TCMSQLiteConnection.Close;
begin
  FSQLiteConn.Close();
end;

function TCMSQLiteConnection.IsClosed: Boolean;
begin
  try
    if not FIsSet then
      Self.reset;
  finally
    Result := not FSQLiteConn.Connected;
  end;
end;

function TCMSQLiteConnection.CreateStatement: ICMStatement;
var
  statement: TCMSQLiteStatement;
begin
  Result := nil;
  if not FIsSet then
    Self.reset;
  statement := FStatementClass.Create(Self, nil);   //不使用默认的信息处理器
  statement.Connection := Self;
  Result := statement;
  //作用 Connection 的信息处理器和记录等级
  statement.Messager.FromMessageHandlers(Messager);
  statement.Messager.SetLevel(Self.Messager.GetLevel);
end;

procedure TCMSQLiteConnection.SetAutoCommit(ABool: Boolean);
begin
  FCriticalSection.Enter;
  try
    FAutoCommit := ABool;
  finally
    FCriticalSection.Leave;
  end;
end;

function TCMSQLiteConnection.GetAutoCommit: Boolean;
begin
  Result := FAutoCommit;
end;

procedure TCMSQLiteConnection.Commit;
begin
  FSQLTran.Commit;
end;

procedure TCMSQLiteConnection.Rollback;
begin
  FSQLTran.Rollback;
end;

function TCMSQLiteConnection.GetCriticalSection: TCriticalSection;
begin
  Messager.Debug('获取CriticalSection...');
  Result := FCriticalSection;
end;

{ TCMSQLiteStatement }

constructor TCMSQLiteStatement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCMSQLiteConnection := nil;
  FScript := nil;
  FQuery := nil;
  FRaiseOnExcute := False;
  FExecuteCount := 0;
end;

destructor TCMSQLiteStatement.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TCMSQLiteStatement.setConnection(AValue: TCMSQLiteConnection);
begin
  if Assigned(FCMSQLiteConnection) then
    Exit;
  FCMSQLiteConnection := AValue;
  FScript := TSQLScript.Create(Self);
  FScript.DataBase := FCMSQLiteConnection.FSQLiteConn;
  FScript.Transaction := FCMSQLiteConnection.FSQLTran;
end;

procedure TCMSQLiteStatement.Close;
begin
  if Assigned(FScript) then
    FreeAndNil(FScript);
end;

function TCMSQLiteStatement.IsClosed: Boolean;
begin
  Result := not Assigned(FScript);
end;

function TCMSQLiteStatement.GetConnection: ICMConnection;
begin
  Result := FCMSQLiteConnection;
end;

procedure TCMSQLiteStatement.SetQueryTimeout(AInt: Integer);
begin
  //
end;

function TCMSQLiteStatement.GetQueryTimeout: Integer;
begin
  Result := 0;
end;

//private
function TCMSQLiteStatement.CheckConnectionStatus: Boolean;
var
  msg: string;
begin
  Result := False;
  if not Assigned(FCMSQLiteConnection) then
    begin
      msg := '未指定连接Connection.';
      Messager.Error('CheckConnectionStatus() return false.', msg);
      if FRaiseOnExcute then
        raise ECMDBError.Create(msg);
      Exit;
    end
  else if FCMSQLiteConnection.IsClosed then
    begin
      msg := 'Connection处于关闭状态.';
      Messager.Error('CheckConnectionStatus() return false.', msg);
      if FRaiseOnExcute then
        raise ECMDBError.Create(msg);
      Exit;
    end;
  if Self.IsClosed then
    begin
      msg := 'Statement处于关闭状态.';
      Messager.Error('CheckConnectionStatus() return false.', msg);
      if FRaiseOnExcute then
        raise ECMDBError.Create(msg);
      Exit;
    end;
  Result := True;
end;

function TCMSQLiteStatement.GetExecuteMsgTitle(const AHead: string): string;
begin
  Result := AHead;
  try
    FExecuteCount := FExecuteCount + 1;
    Result := Format(AHead + '(%d)', [FExecuteCount]);
  except
  end;
end;

function TCMSQLiteStatement.DoQuery(const AMsgTitle, ASQLStr: string; ADataSetOwner: TComponent): TDataSet;
var
  rQuery: TSQLQuery;
begin
  Result := nil;
  DoSQLMessage(etCustom, AMsgTitle, '开始...', ASQLStr);
  //
  rQuery := TSQLQuery.Create(ADataSetOwner);
  Result := rQuery;
  rQuery.Close;
  rQuery.DataBase := FCMSQLiteConnection.FSQLiteConn;
  rQuery.Transaction := FCMSQLiteConnection.FSQLTran;
  //
  rQuery.SQL.Clear;
  rQuery.SQL.Add(ASQLStr);
  rQuery.Prepare;
  rQuery.Open;
  try
    Messager.Message(etCustom, Format(AMsgTitle + ' 数据集%s。', [BoolToStr(rQuery.IsEmpty, '为空', '存在')]), '');
  except
  end;
end;

function TCMSQLiteStatement.Query(const ASQLStr: string; ADataSetOwner: TComponent): TDataSet;
var
  msgTitle: string;
begin
  Result := nil;
  if not CheckConnectionStatus then
    Exit;
  FCMSQLiteConnection.FCriticalSection.Enter;
  try
    try
      msgTitle := GetExecuteMsgTitle('查询SQL');
      Result := DoQuery(msgTitle, ASQLStr, ADataSetOwner);
    except
      on e1: EDatabaseError do
        begin
          Messager.Error(msgTitle + '失败。', e1);
          if Assigned(Result) then
            Result.Close;
          if FRaiseOnExcute then
            raise e1;
        end;
      on e2: Exception do
        begin
          Messager.Error(msgTitle + '出错:。', e2);
          if Assigned(Result) then
            Result.Close;
          if FRaiseOnExcute then
            raise e2;
        end;
    end;
  finally
    FCMSQLiteConnection.FCriticalSection.Leave;
  end;
end;

//function TCMSQLiteStatement.DoQueryBySqliteDataSet(const AMsgTitle, ASQLStr: string; ADataSetOwner: TComponent): TDataSet;
//var
//  rDS: TSqlite3Dataset;
//begin
//  Result := nil;
//  DoSQLMessage(etCustom, AMsgTitle, '开始...', ASQLStr);
//  //
//  rDS := TSqlite3Dataset.Create(Self);
//  Result := rDS;
//  rDS.Close;
//  rDS.FileName := FCMSQLiteConnection.FSQLiteConn.DatabaseName;
//  rDS.SQL := ASQLStr;
//  rDS.Open;
//  try
//    Messager.Message(etCustom, Format(AMsgTitle + ' 数据集%s。', [BoolToStr(rDS.IsEmpty, '为空', '存在')]), '');
//  except
//  end;
//end;

//function TCMSQLiteStatement.QueryBySqliteDataSet(const ASQLStr: string; ADataSetOwner: TComponent): TDataSet;
//var
//  msgTitle: string;
//begin
//  Result := nil;
//  if not CheckConnectionStatus then
//    Exit;
//  FCMSQLiteConnection.FCriticalSection.Enter;
//  try
//    try
//      msgTitle := GetExecuteMsgTitle('[SqliteDataSet]查询SQL');
//      Result := DoQueryBySqliteDataSet(msgTitle, ASQLStr, ADataSetOwner);
//    except
//      on e1: EDatabaseError do
//        begin
//          Messager.Error(msgTitle + '失败。', e1);
//          if Assigned(Result) then
//            Result.Close;
//          if FRaiseOnExcute then
//            raise e1;
//        end;
//      on e2: Exception do
//        begin
//          Messager.Error(msgTitle + '出错:。', e2);
//          if Assigned(Result) then
//            Result.Close;
//          if FRaiseOnExcute then
//            raise e2;
//        end;
//    end;
//  finally
//    FCMSQLiteConnection.FCriticalSection.Leave;
//  end;
//end;

function TCMSQLiteStatement.DoExecute(const AMsgTitle, ASQLStr: string): Boolean;
begin
  Result := False;
  DoSQLMessage(etCustom, AMsgTitle, '开始...', ASQLStr);
  //
  FCMSQLiteConnection.FSQLTran.Active := True;
  FScript.Script.Clear;
  FScript.Script.Text := ASQLStr;
  FScript.Execute;
  if FCMSQLiteConnection.AutoCommit then
    FCMSQLiteConnection.FSQLTran.Commit;
  Result := True;
  try
    Messager.Message(etCustom, AMsgTitle + ' 执行结束。', '');
  except
  end;
end;

function TCMSQLiteStatement.DoExecuteBySQLQuery(const AMsgTitle, ASQLStr: string): Boolean;
begin
  Result := False;
  DoSQLMessage(etCustom, AMsgTitle, '开始...', ASQLStr);
  //
  if not Assigned(FQuery) then
    begin
      FQuery := TSQLQuery.Create(Self);
      FQuery.DataBase := FCMSQLiteConnection.FSQLiteConn;
      FQuery.Transaction := FCMSQLiteConnection.FSQLTran;
    end;
  FCMSQLiteConnection.FSQLTran.Active := True;
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Text := ASQLStr;
  FQuery.ExecSQL;
  if FCMSQLiteConnection.AutoCommit then
    FCMSQLiteConnection.FSQLTran.Commit;
  Result := True;
  try
    Messager.Message(etCustom, AMsgTitle + ':执行结束。', '');
  except
  end;
end;

function TCMSQLiteStatement.DoExecuteBySqliteConnection(const AMsgTitle, ASQLStr: string): Boolean;
begin
  Result := False;
  DoSQLMessage(etCustom, AMsgTitle, '开始...', ASQLStr);
  //
  FCMSQLiteConnection.FSQLiteConn.ExecSQLEx(ASQLStr);
  Result := True;
  try
    Messager.Message(etCustom, AMsgTitle + ':执行结束。', '');
  except
  end;
end;

//private
function TCMSQLiteStatement.SelDoExecute(sel: Word; const AMsgTitle, ASQLStr: string): Boolean;
begin
  Result := False;
  if not CheckConnectionStatus then
    Exit;
  FCMSQLiteConnection.FCriticalSection.Enter;
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
          FCMSQLiteConnection.FSQLTran.Rollback;
          if FRaiseOnExcute then
            raise e1;
        end;
      on e2: Exception do
        begin
          Result := False;
          Messager.Error(AMsgTitle + '出错:。', e2);
          FCMSQLiteConnection.FSQLTran.Rollback;
          if FRaiseOnExcute then
            raise e2;
        end;
    end;
  finally
    FCMSQLiteConnection.FCriticalSection.Leave;
  end;
end;

procedure TCMSQLiteStatement.DoSQLMessage(Et: TEventType; const ATitle, AMsg, ASQL: string);
var
  rec: TCMDBMessageRecord;
begin
  rec := TCMDBMessageRecord.Create;
  rec.EventType := Et;
  rec.Title := ATitle;
  rec.Message := AMsg;
  rec.SQL := ASQL;
  Messager.DoMessage(rec);
end;

function TCMSQLiteStatement.Execute(const ASQLStr: string): Boolean;
var
  msgTitle: string;
begin
  Result := False;
  msgTitle := GetExecuteMsgTitle('执行SQL');
  Result := SelDoExecute(1, msgTitle, ASQLStr);
end;

function TCMSQLiteStatement.ExecuteBySQLQuery(const ASQLStr: string): Boolean;
var
  msgTitle: string;
begin
  Result := False;
  msgTitle := GetExecuteMsgTitle('[SQLQuery]执行SQL');
  Result := SelDoExecute(2, msgTitle, ASQLStr);
end;

function TCMSQLiteStatement.ExecuteBySqliteConnection(const ASQLStr: string): Boolean;
var
  msgTitle: string;
begin
  Result := False;
  msgTitle := GetExecuteMsgTitle('[SqliteConnection]执行SQL');
  Result := SelDoExecute(3, msgTitle, ASQLStr);
end;

function TCMSQLiteStatement.GetCriticalSection: TCriticalSection;
begin
  Result := FCMSQLiteConnection.FCriticalSection;
end;


{TSQLite3ConnectionEx}

procedure TSQLite3ConnectionEx.ExecSQLEx(const ASQL: string);
begin
  Self.execsql(ASQL);
end;



end.

