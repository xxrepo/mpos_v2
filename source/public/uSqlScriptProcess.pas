{
  SQL语句处理单元
  主要处理sql脚本的解析，分解等
}
unit uSqlScriptProcess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbUnzper, AbArcTyp, StrUtils,
  cm_messager;

type

  //记录表名和表中的数量量
  RFileAndTableSCount = Record
    FFileName : string;    //解析出的文件名
    FFilePath : string;    //文件所在的路径(含文件名)
    FTableName : string;   //文件中的表
    FTableSourceCount: integer;   //下发的数量
  end;

  //记录一个小单元的sql语句
  TSQLRecord = record
    FSQLType : string;  //记录语句的类型 例:create,insert,drop...
    //FTableName:string;  //如果是create table 记录的就是表名
    FSQLStr : string;   //具体的Sql语句
  end;
  PTSQLRecord = ^TSQLRecord;

  { TSQLProcessTask }

  TSQLProcessTask = class(TCMMessageablePersistent)
  private
    FPackFileList : TStringList;   //文件压缩包列表,里面含文件名的完整路径
    FExtractPath : string;      //解压的路径
    FSQLRunFileName : string;   //文件总列表的文件名  sqlrun.txt
    FScriptDelimiter : string;  //脚本的分隔符
    FSQLProcessedList :TList;  //已处理语句
    FLastErrorInfo: string;
  public
    FTableInfos : array of RFileAndTableSCount;  //记录操作了哪些文件，文件里有哪个表，下发了多少数据
  published
    property SQLRunFileName: string read FSQLRunFileName write FSQLRunFileName;
    property ScriptDelimiter: string read FScriptDelimiter write FScriptDelimiter;
    property PackFileList: TStringList read FPackFileList write FPackFileList;
    property ExtractPath: string read FExtractPath write FExtractPath;
    property SQLProcessedList: TList read FSQLProcessedList write FSQLProcessedList;
    property LastErrorInfo: string read FLastErrorInfo write FLastErrorInfo;
  public
    constructor Create;
    destructor Destroy;override;
    //初始化准备工作
    function init:boolean;
    //增加一个压缩包
    function addPackage(fileName:string):boolean;
    //增加一个文件名
    function addTables(fileName,filePath,tableName:string;tsc:integer):boolean;
    //设置文件名对应的表名
    function setTablesTableName(fileName,tableName:string):boolean;
    //取出对应表名的相关信息
    function getTables(tableName:string):RFileAndTableSCount;

    //文本编码处理
    function GetStrForUTF8File(FileName: string): string;
    //解压缩文件包
    function unPackFile:boolean;
    //加载待执行sql文件列表
    function loadSqlRunFileList:boolean;
    //解析
    function parseSql(fileName:string;sqlList:TStringList;maxScriptCount:integer=500):boolean;
    //解析待处理文件列表中的所有文件
    function parseSqlScript:boolean;
  end;



implementation

{ TSQLProcessTask }

constructor TSQLProcessTask.Create;
begin
  inherited Create;
  SetLength(FTableInfos,0);
  FSQLRunFileName :='SqlRun.txt';
  FPackFileList := TStringList.Create;
  FSQLProcessedList := TList.Create;
  FExtractPath := '.'+PathDelim+'temp'+PathDelim;
  FLastErrorInfo := '';
  Messager.Debug('Create');
end;

destructor TSQLProcessTask.Destroy;
begin
  if Assigned(FPackFileList) then FreeAndNil(FPackFileList);
  if Assigned(FSQLProcessedList) then FreeAndNil(FSQLProcessedList);
  inherited Destroy;
end;

function TSQLProcessTask.init: boolean;
begin
  Result := False;
  FPackFileList.Clear;
  FExtractPath :='.'+PathDelim+'temp'+PathDelim;
  FSQLRunFileName :='SqlRun.txt'; //默认
  FScriptDelimiter :='[-ENDOFSCRIPT-]';  //脚本的分隔符,默认值
  FSQLProcessedList.Clear;  //已处理语句
  SetLength(FTableInfos,0);
end;

function TSQLProcessTask.addPackage(fileName: string): boolean;
begin
  Result := True;
  if PackFileList.IndexOf(fileName)>=0 then Exit;
  PackFileList.Add(fileName);
  Messager.Debug('数据解析模块增加一个压缩包:'+fileName);
end;


function TSQLProcessTask.addTables(fileName, filePath, tableName: string; tsc: integer): boolean;
var i : integer;
begin
  Result := False;
  for i := 0 to length(FTableInfos) -1 do
  begin
    if FTableInfos[i].FFileName = fileName then
    begin
      Messager.Error('addTables 已经存在文件名:'+fileName);
      Exit;
    end;
  end;
  setLength(FTableInfos,length(FTableInfos)+1);
  FTableInfos[length(FTableInfos)-1].FFileName:=fileName;
  FTableInfos[length(FTableInfos)-1].FFilePath:=filePath;
  FTableInfos[length(FTableInfos)-1].FTableName:=tableName;
  FTableInfos[length(FTableInfos)-1].FTableSourceCount:=tsc;
  Result := True;
  Messager.Debug('addTables 新增一个文件名成功:'+fileName);

end;

function TSQLProcessTask.setTablesTableName(fileName,tableName: string): boolean;
var i : integer;
begin
  Result := False;
  for i := 0 to length(FTableInfos) -1 do
  begin
    if FTableInfos[i].FFileName = fileName then
    begin
      FTableInfos[i].FTableName:=tableName;
      Result := True;
      Messager.Debug('setTablesTableName 更新名件名对应表名成功:'+fileName);
      Exit;
    end;
  end;
  Messager.Error('setTablesTableName 末找到文件相关信息:'+fileName);
end;

function TSQLProcessTask.getTables(tableName: string): RFileAndTableSCount;
var i : integer;
begin
  result.FFileName:='';
  result.FTableName:='';
  result.FTableSourceCount:=0;
  for i := 0 to length(FTableInfos) -1 do
  begin
    if FTableInfos[i].FTableName = tableName then
    begin
      Result.FFileName:= FTableInfos[i].FFileName;
      Result.FTableName:= FTableInfos[i].FTableName;
      Result.FTableSourceCount:= FTableInfos[i].FTableSourceCount;
      Exit;
    end;
  end;

end;

function TSQLProcessTask.GetStrForUTF8File(FileName: string): string;
var
  MemStream: TMemoryStream;
  str, HeaderStr: string;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;
  MemStream := TMemoryStream.Create;
  try
    MemStream.LoadFromFile(FileName);

    SetLength(HeaderStr, 3);
    MemStream.Read(HeaderStr[1], 3);
    if HeaderStr = #$EF#$BB#$BF then
    begin
      SetLength(str, MemStream.Size - 3);
      MemStream.Read(str[1], MemStream.Size - 3);
    end;
    Result := str;
  finally
    MemStream.Free;
  end;
end;

function TSQLProcessTask.unPackFile: boolean;
var
  Unzip: TAbUnZipper;
  i : integer;
begin
  Result := False;
  if PackFileList.Count <= 0 then
  begin
    Messager.Debug('unPackFile 没有需要解压缩的文件。');
    Exit;
  end;
  Unzip := TAbUnZipper.Create(nil);
  try
    for i := 0 to FPackFileList.Count-1 do
    begin
      if not FileExists(Trim(FPackFileList.Strings[i])) then
      begin
        Messager.Debug(FPackFileList.Strings[i]+' 文件不存在。 unPackFile');
        Exit;
      end;
      Unzip.ExtractOptions := Unzip.ExtractOptions + [eoRestorePath];
      Unzip.FileName := FPackFileList.Strings[i];
      Unzip.BaseDirectory := FExtractPath;
      Unzip.ExtractFiles('*.*');
    end;
    Result := True;
    Messager.Debug('unPackFile 解压文件列表完成。');
  finally
    Unzip.Free;
  end;
end;

function TSQLProcessTask.loadSqlRunFileList: boolean;
var i,j : integer;
    packfileName : string;  //压缩包名
    packfilePath : string;  //当前sqlrun.txt所在的路径
    runFile : string;       //完整的sqlrun.txt路径
    loadSqlRun : TStringList;  //用来加载sqlrun.txt
    sqlRunLineStr : string; //记录从sqlrun.txt中记录的每一行文本 例如:sp_Pos_DownloadSysParams_001.sql,182
    filename : string;      //sqlrun.txt中记录的文件名  例如:sp_Pos_DownloadSysParams_001.sql
    fCount : integer;       //sqlrun.txt中文件名对应的下发数量 例如:182
begin
  Result := False;
  if PackFileList.Count <=0 then
  begin
    Messager.Error('loadSqlRunFileList 准备加载sqlrun.txt,需要处理的文件列表为空');
    Exit;
  end;
  for i := 0 to PackFileList.Count-1 do
  begin
    //取出压缩包的文件名
    packfileName := ExtractFileName(PackFileList.Strings[i]);
    packfilePath := FExtractPath + LeftStr(packfileName, Length(packfileName) - 4) + PathDelim;
    //组成一个完整的runfile.txt
    runFile := packfilePath + FSQLRunFileName;
    Messager.Debug('loadSqlRunFileList 开始加载:'+runFile);
    if not FileExists(runFile) then
    begin
      Messager.Debug(runFile+' 文件不存在。');
      Exit;
    end;
    //解析文件列表，存放文件名和下发的数量
    loadSqlRun := TStringList.Create;
    try
      try
        loadSqlRun.Clear;
        loadsqlrun.LoadFromFile(runFile);
        //loadSqlRun.Text := (GetStrForUTF8File(runFile));
        //Messager.Debug('TSQLProcessTask.loadSqlRunFileList 加载完成:'+runFile);
        //Messager.Debug('TSQLProcessTask.loadSqlRunFileList sqlrun数据条数:'+inttostr(loadSqlRun.Count));
        //开始分解出文件名和记录数量
        for j := 0 to loadSqlRun.Count-1 do
        begin
          sqlRunLineStr := Trim(loadSqlRun.Strings[j]);
          Messager.Debug(sqlRunLineStr);
          filename := Copy(sqlRunLineStr, 1, system.Pos(',', sqlRunLineStr) - 1);
          fCount := StrToInt(Trim(Copy(sqlRunLineStr, system.Pos(',', sqlRunLineStr) + 1, length(sqlRunLineStr))));
          if not addTables(filename,packfilePath,'',fCount) then
            Exit;
        end;
      except on e: Exception do
        begin
          Messager.Error('loadSqlRunFileList发生异常:'+e.Message);
          Exit;
        end;
      end;
    finally
      if Assigned(loadSqlRun) then FreeAndNil(loadSqlRun);
    end;
  end;
  Result := True;
  Messager.Debug('加载sqlrun.txt成功完成。');

end;

function TSQLProcessTask.parseSql(fileName:string;sqlList:TStringList;maxScriptCount:integer=500): boolean;
var i,sCount : integer;   //循环变量，计数器
    State: integer;  //操作状态 1 delete  2 insert  3 update  4 create  5 drop
    tmpSqlLine,tmpSqlLine_bak : string; //从列表中取出的一行待解析语句
    HeadOfScript : string;  //语句的头
    HeadSql : string;  //语句的头，用来组装完成语句使用的，例: create table...delete from...等
    runSql : string; //组成的完整的待执行语句
    tableName : string; //解析出来的具体的表名

    bEndInsert : boolean;//标识一条插入语句的结束

    psqlr : PTSQLRecord; //用来记录已经处理好的sql语句
begin
  result := False;
  //相关变量初始////////////////////
  sCount := 0;
  tmpSqlLine := '';
  HeadOfScript := '';
  HeadSql := '';
  runSql := '';
  ///////////////////////////////////
  for i := 0 to sqlList.Count-1 do
  begin
    tmpSqlLine_bak := '';
    tmpSqlLine := Trim(sqlList.Strings[i]);
    if tmpSqlLine = '' then Continue;
    HeadOfScript := UpperCase(copy(tmpSqlLine, 1, 4));  //取每行语句的头4个字符，判断当前的操作状态
    case HeadOfScript of
      'DELE':
      begin
        State := 1;
        //DELETE FROM tbposPricebdis_temp[-ENDOFSCRIPT-]
        //删除的语句基本也就一句，要记录下表名
        tmpSqlLine := UpperCase(tmpSqlLine);
        tableName := Copy(tmpSqlLine, 1, system.Pos('[', tmpSqlLine) - 1);  //DELETE FROM tbposPricebdis_temp
        tableName := Trim(Copy(tableName, system.Pos('FROM', tableName) + 5, Length(tableName))); //tbposPricebdis_temp
        if fileName<>'' then
          setTablesTableName(fileName,tableName);
        tableName := '';
        ///////////////////////////////////////////
        tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
        HeadSql := tmpSqlLine;
        runSql := runSql + HeadSql + chr(13);

        //inc(sCount);
      end;
      'INSE':
      begin
        State := 2;
        tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
        HeadSql := tmpSqlLine;
        //记录插入数据的表名
        tableName := copy(tmpSqlLine, 1, system.Pos('(', tmpSqlLine) - 1);  //INSERT INTO tbShopGDBind_temp(
        tableName := Trim(copy(tableName, system.Pos('INTO', tableName) + 5, Length(tableName)));
        //文件名对应的表名要记录下来
        if fileName<>'' then
          setTablesTableName(fileName,tableName);
        bEndInsert := True;
      end;
      'UPDA':
      begin
        State := 3;
        tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
        HeadSql := tmpSqlLine;
        runSql := runSql + HeadSql + chr(13);
        //inc(sCount);
      end;
      'CREA':
      begin
        State := 4;
        tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
        HeadSql := tmpSqlLine;
        runSql := runSql + HeadSql + chr(13);
        {
        {
        当前不是一个建一个表的情况下，表名不用保存，这个是为了
        不要在内存库建视图，索引这些东西
        还有一种情况，建表是多行的语句，如果下面的字段正好有
        create开头的时候，例createtime   这种情况下，解析的时候
        也会进入到这里，如果不标识的话，就会把tableName清掉，这样的
        话，内存库就无法建这个表，会有问题
        }
        if not bBeginCreateTable then tableName :='';
        //判断当前是否是一个建表语句的开始
        HeadSql := UpperCase(HeadSql);
        if system.Pos('CREATE TABLE',HeadSql)>0 then
        begin
          bBeginCreateTable := True;
          tableName := copy(HeadSql, 1, system.Pos(']', HeadSql) );  //INSERT INTO tbShopGDBind_temp(
          tableName := Trim(copy(tableName, system.Pos('[', tableName) , Length(tableName)));
        end;
        }
        //inc(sCount);
      end;
      'DROP':
      begin
        State := 5;
        tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
        HeadSql := tmpSqlLine;
        runSql := runSql + HeadSql + chr(13);
        //inc(sCount);
      end
      else
      begin    //当不是头语句，是数据语句的操作
        case State of      //1 delete  2 insert  3 update  4 create  5 drop
          1:
            begin
              tmpSqlLine_bak := tmpSqlLine;
              tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
              runSql := runSql + tmpSqlLine + chr(13);
              if system.Pos('[-ENDOFSCRIPT-]',tmpSqlLine_bak) >= 1 then
              begin
                new(psqlr);
                psqlr^.FSQLType:='DELETE';
                psqlr^.FSQLStr:= runSql;
                SQLProcessedList.Add(psqlr);
                //Messager.Debug('DELETE:'+runSql);
                runSql := '';
              end;
            end;
          2:
            begin
              //如果上一个语句已经结束，那这个语句的开始就要加上插入头语句  HeadSql
              if bEndInsert then
                runSql := runSql + HeadSql + chr(13);
              //当前一个语句是否已经结束
              if system.Pos('[-ENDOFSCRIPT-]',tmpSqlLine)>=0 then begin
                bEndInsert := True;
                inc(sCount);  //计数
              end else
                bEndInsert := False;

              tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
              runSql := runSql + tmpSqlLine + chr(13);

              //暂停全部还是500条数据一执行，这样快
              //一个完整的insert语句已经完成，并已经达到500条数据
              if (bEndInsert= True) and (sCount>=maxScriptCount) then
              begin
                new(psqlr);
                psqlr^.FSQLType:='INSERT';
                //psqlr^.FTableName:= tableName;
                psqlr^.FSQLStr:= runSql;
                SQLProcessedList.Add(psqlr);
                sCount := 0;
                //Messager.Debug('INSERT:'+runSql);
                runSql := '';
              end;
            end;
          3:
            begin
              tmpSqlLine_bak := tmpSqlLine;
              tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
              runSql := runSql + tmpSqlLine + chr(13);
              if system.Pos('[-ENDOFSCRIPT-]',tmpSqlLine_bak) >= 1 then
              begin
                new(psqlr);
                psqlr^.FSQLType:='UPDATE';
                psqlr^.FSQLStr:= runSql;
                SQLProcessedList.Add(psqlr);
                //Messager.Debug('UPDATE:'+runSql);
                runSql := '';
              end;
            end;
          4:
            begin
              tmpSqlLine_bak := tmpSqlLine;
              tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
              runSql := runSql + tmpSqlLine + chr(13);
              if system.Pos('[-ENDOFSCRIPT-]',tmpSqlLine_bak) >= 1 then
              begin
                //bBeginCreateTable := False; //无论如何，有结束符了，建表的状态肯定是结束的
                new(psqlr);
                psqlr^.FSQLType:='CREATE';
                //psqlr^.FTableName:= tableName;
                psqlr^.FSQLStr:= runSql;
                SQLProcessedList.Add(psqlr);
                //Messager.Debug('CREATE:'+runSql);
                runSql := '';
              end;
            end;
          5:
            begin
              tmpSqlLine_bak := tmpSqlLine;
              tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
              runSql := runSql + tmpSqlLine + chr(13);
              if system.Pos('[-ENDOFSCRIPT-]',tmpSqlLine_bak) >= 1 then
              begin
                new(psqlr);
                psqlr^.FSQLType:='DROP';
                psqlr^.FSQLStr:= runSql;
                SQLProcessedList.Add(psqlr);
                //Messager.Debug('DROP:'+runSql);
                runSql := '';
              end;
            end
          else
          begin
            tmpSqlLine := ReplaceStr(tmpSqlLine, '[-ENDOFSCRIPT-]', ';');
            runSql := runSql + tmpSqlLine + chr(13);
          end;
        end;    //case
      end;

    end;  //case

  end;   //for
  //解析完成以后再次检查是否否所有数据已经保存
  //到这里来了，应该就只剩下插入语句 了，因为其它语句已经有一句保存一句，都搞 完了
  if Trim(runSql) <> '' then
  begin
    new(psqlr);
    psqlr^.FSQLType:='INSERT';
    psqlr^.FSQLStr:= runSql;
    SQLProcessedList.Add(psqlr);
    //Messager.Debug('INSERT:'+runSql);
    runSql := '';
  end;
  Result := True;
end;

function TSQLProcessTask.parseSqlScript: boolean;
var i,j : integer;
    SQLPending : TStringList;  //用来加载每一个待处理文件中的原始数据
begin
  Result := False;
  if not unPackFile then Exit;
  if not loadSqlRunFileList then Exit;
  SQLPending := TStringList.Create;
  try
    for i := 0 to Length(FTableInfos)-1 do
    begin
      SQLPending.Clear;
      SQLPending.Text:= GetStrForUTF8File(FTableInfos[i].FFilePath+FTableInfos[i].FFileName);
      if not parseSql(FTableInfos[i].FFileName,SQLPending) then
      begin
        Messager.Error('parseSqlScript 解析脚本文件失败:'+FTableInfos[i].FFileName);
        Exit;
      end else
        Messager.Error('parseSqlScript 解析脚本文件成功:'+FTableInfos[i].FFileName);
    end;
    //所有的文件解析完成以后，需要完整的统计所有表的表名和相应的下发的数据量(也就是去重，因为一个表可能分成多个文件下发)
    for i := 0 to Length(FTableInfos)-1 do
    begin
      if FTableInfos[i].FTableName<>'' then
      begin
        if i <> Length(FTableInfos)-1 then
          for j := i+1 to Length(FTableInfos)-1 do
          begin
            if FTableInfos[j].FTableName = FTableInfos[i].FTableName then
            begin
              FTableInfos[i].FTableSourceCount:= FTableInfos[i].FTableSourceCount+FTableInfos[j].FTableSourceCount;
              FTableInfos[j].FTableSourceCount:=0;
              FTableInfos[j].FTableName:='';
              FTableInfos[j].FFileName:='';
              FTableInfos[j].FFilePath:='';
            end;
          end;

      end;
    end;

    //Messager.Debug('输出测试');
    //for i := 0 to length(FTableInfos)-1 do
    //begin
    //  Messager.Debug('==================================');
    //  Messager.Debug('FFileName='+FTableInfos[i].FFileName);
    //  Messager.Debug('FFilePath='+FTableInfos[i].FFilePath);
    //  Messager.Debug('FTableName='+FTableInfos[i].FTableName);
    //  Messager.Debug('FTableSourceCount='+inttostr(FTableInfos[i].FTableSourceCount));
    //  Messager.Debug('==================================');
    //end;

    Result := True;
  finally
    if Assigned(SQLPending) then FreeAndNil(SQLPending);
  end;
end;

end.

