{
数据库初始化模块
初始化流程：
1.如果数据库存在，检查是否有末提交的上传数据任务并提交完毕
2.如果数据库存在，则备份当前的数据库
3.请求最新的数据库初始化任务信息
4.执行数据库初始化脚本
5.检查数据的完整性
6.完毕
}
unit uDBInitializeImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Controls, StrUtils, uDBInitialize,
  cm_messager, uFormService, cm_LCL, uDAO, uApp, uDBInitDAOImpl,
  uSqlScriptProcess, uRemoteService, uDownFileService,
  PosService, cm_db, sqlite3, uDBInitForm;

type
  //初始化数据库工具类

  { TDBInitialize }

  TDBInitialize = class(TCMMessageable,IDBInitialize)
  public
    constructor Create;
    destructor Destroy;
    function DBInitialize: boolean; //初始化数据库

    function checkDbFileExists:boolean; //检查数据库文件是否存在
    function submitUploadTask:boolean; //检查并提交末上传的数据任务
    function backupDataBase:boolean;//备份当前数据库
    function checkTableSourceCount(tableName:string;cCount:integer):boolean;
    function getLastInitInfoFromServer:boolean;//获取最新的初始化任务信息
    function executeSqlScript:boolean;    //执行解析出来的数据库脚本
  private
    DBInitDAO : TDBinitBasicDAO;//IDBinitDAO;  //数据库操作类
    SQLProcessTask:TSQLProcessTask;  //脚本处理类
    FormDbInit : TDBInitForm;
    function DBFormCreate: boolean;
    procedure printLog(s:string);
  end;

implementation

{TDBInitialize}

constructor TDBInitialize.Create;
begin
  inherited Create;
  //TPOSDAOFactory.GetInstance.OutDAO(TDBInitDAO,IDBInitDAO,DBInitDAO);
  DBInitDAO := TDBinitBasicDAO.Create;
  SQLProcessTask:=TSQLProcessTask.Create;
end;

destructor TDBInitialize.Destroy;
begin
  if Assigned(DBInitDAO) then FreeAndNil(DBInitDAO);
  if Assigned(SQLProcessTask) then FreeAndNil(SQLProcessTask);
end;

function TDBInitialize.DBInitialize: boolean;
begin
  Result := False;
  DBFormCreate;
  //Messager.Debug('这个一个数据库初始化的工具信息输出测试');
  if getLastInitInfoFromServer then
    if executeSqlScript then Result := True;
end;

function TDBInitialize.checkDbFileExists: boolean;
begin
  //Result := DBDAO.dbFileExists;
  result := False;
end;

function TDBInitialize.submitUploadTask: boolean;
begin
  Result := True;
end;

function TDBInitialize.backupDataBase: boolean;
begin
  Result := True;
end;

function TDBInitialize.checkTableSourceCount(tableName: string; cCount: integer
  ): boolean;
var cint : integer;
begin
  Result := False;
  cint := DBInitDAO.GetDBHelper.GetInteger('select count(*) from '+tableName+';');
  Messager.Debug('校验表'+tableName+'数据，共下发'+IntToStr(cCount)+'条数据。'+ chr(13) +
                 '数据库中共有数据:'+IntToStr(cint)+'条。');

  if cInt = cCount then
  begin
    Messager.Debug(tableName+'校验通过 PASS');
    Result := True;
  end else Messager.Debug(tableName+'校验未通过 NO PASS');


end;

function TDBInitialize.getLastInitInfoFromServer: boolean;
var i: integer;
    FileDownService: IFileDownService;
    RemoteService: IRemoteService;
    //远程服务返回的结果
    lir: LastInitialIdResponse;
    icr: InitialClientResponse;
    //使用的变量
    iInitTaskID: integer;//初始化任务的ID
    vStep: TStringList;  //初始化的步骤
    fdt:TFileDownTask;   //一个下载任务
    downFileName: string; //需要下载的包名
begin
  Result := False;
  lir := nil;
  icr := nil;
  vStep := TStringList.Create;
  try
    if InterfaceRegister.OutInterface(IRemoteService, RemoteService) then   //远程服务
    begin
      //1.取最新的任务id和下载的步骤//////////////////
      lir := RemoteService.GetLastInitialId();
      if lir.ResponseCode = ResponseResult.Success then
      begin
        iInitTaskID := lir.Id;
        vStep.CommaText := lir.Step;
        Messager.Debug(Format('获取初始化任务......【ID: %s】', [IntToStr(iInitTaskID)]));
        printLog(Format('获取初始化任务......【ID: %s】', [IntToStr(iInitTaskID)]));
      end else
      begin
        Messager.Error('GetLastInitialId失败，初始化失败');
        Exit;
      end;
      /////////////////////////////////////////////////////////
      //引出文件下载服务
      if not InterfaceRegister.OutInterface(IFileDownService, FileDownService) then
      begin
        Messager.Error('终端初始化任务......【失败】,OutInterface(IFileDownService, FileDownService)');
        Exit;
      end;
      //2.分步下载数据包文件/////////////////////////////////////
      for i := 0 to vStep.Count-1 do
      begin
        Messager.Debug('下载初始化脚本......【开始】');
        printLog('下载初始化脚本......【开始】');
        icr := RemoteService.InitialClient(iInitTaskID,1,StrToInt(vStep.Strings[i]));
        if not (icr.ResponseCode = ResponseResult.Success)  then
        begin
          Messager.Error('下载初始化脚本......【失败】');
          Exit;
        end;
        //取下需要下载的文件包名
        downFileName := Trim(icr.ClientFile.FileUrl);
        if downFileName = '' then
        begin
          Messager.Error('下载初始化脚本......【失败】');
          Exit;
        end;
        downFileName := './' + ReplaceStr(downFileName, '//', '/');
        //ZipFile := Trim(Trim(SPI.FilePath.TempPath) + Trim(ExtractFileName(fileName)));
        try
          fdt:=TFileDownTask.Create(IntToStr(iInitTaskID));
          fdt.DownFileName:=downFileName;
          fdt.SaveFileName:=SQLProcessTask.ExtractPath+ExtractFileName(downFileName);
          if not FileDownService.addTask(fdt) then
          begin
            Messager.Error('下载初始化脚本......【失败】,新增一个下载任务失败:'+downFileName);
            Exit;
          end;
          while not (fdt.TaskStatus=sFinished) do
          begin
            printLog('正在下载文件中...');
            sleep(300);
            Application.ProcessMessages;
            //等待文件下载完成。这里可以做进度条
          end;
          if not FileExists(fdt.SaveFileName) then
          begin
            Messager.Error('下载初始化脚本......【失败】,文件下载失败，文件不存在:'+downFileName);
            Exit;
          end;
          SQLProcessTask.PackFileList.Add(fdt.SaveFileName);
        finally
          if Assigned(fdt) then FreeAndNil(fdt);
        end;
      end; //文件下载完成
      /////////////////////////////////////////////////////////////
      //3.开始解析数据包//////////////////////////////////////////
      if not SQLProcessTask.parseSqlScript then Exit;
      ////////////////////////////////////////////////////////////////
      Result := True;
    end else
    begin
      Messager.Error('终端初始化任务......【失败】,OutInterface(IRemoteService, RemoteService)')
    end;
  finally
    if Assigned(vStep) then FreeAndNil(vStep);
    if Assigned(lir) then FreeAndNil(lir);
    if Assigned(icr) then FreeAndNil(icr);
  end;
end;

function TDBInitialize.executeSqlScript: boolean;
var i: integer;
    psr:PTSQLRecord;
    tSql : string;
begin
  Result := False;
  if SQLProcessTask.SQLProcessedList.Count <= 0 then
  begin
    Messager.Error('待执行的sql脚本为空。');
    Exit;
  end;
  if not DBInitDAO.CreateMemDb then
  begin
    Messager.Error('内存数据库建立失败.');
    Exit;
  end;
  Messager.Debug('开始执行解析出来的SQL脚本');
  for i := 0 to SQLProcessTask.SQLProcessedList.Count-1 do
  begin
    psr := PTSQLRecord(SQLProcessTask.SQLProcessedList.Items[i]);
    if psr^.FSQLType='CREATE' then
    begin
      //Messager.Debug('建表:'+psr^.FSQLType+',tablename='+psr^.FTableName+',sql='+psr^.FSQLStr);

      //如果是建表的语句，那么实体和内存库都是要执行的
      if not DBInitDAO.GetDBHelper.Execute(psr^.FSQLStr) then
      begin
        Messager.Error('实体数据库执行语句失败:'+psr^.FSQLStr);
        Exit;
      end;
      if not DBInitDAO.MemDbExecSql(psr^.FSQLStr) then
      begin
        Messager.Error('内存数据库执行语句失败:'+psr^.FSQLStr);
        Exit;
      end;
    end else begin
      if (psr^.FSQLStr<>'') then
      begin
        if not DBInitDAO.MemDbExecSql(psr^.FSQLStr) then
        begin
          Messager.Error('内存数据库执行语句失败:'+psr^.FSQLStr);
          Exit;
        end;
      end;
    end;
  end;
  //所有数据执行完成以后，开始把内存库中的数据拉到实体库中
  //1.内存库附加实体库
  if not DBInitDAO.AttachDb then
  begin
    Messager.Error('内存库附加实体库失败。');
    Exit;
  end;
  //2.生成拉数据的sql
  tSql := '';
  for i := 0 to Length(SQLProcessTask.FTableInfos)-1 do
  begin
    if SQLProcessTask.FTableInfos[i].FTableName <> '' then
    begin
      tSql := tSql + 'Insert into posdb.'+ SQLProcessTask.FTableInfos[i].FTableName +
              ' select * from ' + SQLProcessTask.FTableInfos[i].FTableName + ' ;'+ chr(13);
    end;
  end;
  Messager.Debug('内存库即将执行数据转移语句:'+chr(13)+tSql);
  if not DBInitDAO.MemDbExecSqlOnLock(tSql,True) then
  begin
    Messager.Error('内存库转移数据失败。');
    Exit;
  end;
  //3.销毁内存数据库
  if DBInitDAO.DestoryMemDb then Messager.Debug('内存数据库销毁成功') else Messager.Error('内存数据库销毁失败。');
  //4.校验数据完整性
  for i := 0 to Length(SQLProcessTask.FTableInfos)-1 do
  begin
    if SQLProcessTask.FTableInfos[i].FTableName <> '' then
    begin
      if not checkTableSourceCount(SQLProcessTask.FTableInfos[i].FTableName,
        SQLProcessTask.FTableInfos[i].FTableSourceCount) then
      begin
        Exit;
      end;
    end;
  end;

  Result := True;
  Messager.Debug('数据库初始化完成。');
end;

function TDBInitialize.DBFormCreate: boolean;
var
  prw: ICMLCLPropertyReaderWriter;
  lclg: ICMLCLGenerator;
  lclm: ICMLCLManager;
  mt : TMethod;
begin
  Result := False;
  if InterfaceRegister.OutInterface(ICMLCLPropertyReaderWriter, prw) then
    if InterfaceRegister.OutInterface(ICMLCLGenerator, lclg) then
      if InterfaceRegister.OutInterface(ICMLCLManager, lclm) then
        begin
          FormDbInit := TDBInitForm(lclg.NewComponent('TServiceForm', nil));
          lclm.GetMainLCLGlobalSet.SetRequireDerivedFormResource(False);
          try
            prw.SetStrProp(FormDbInit.PanelTop,'Caption','数据库初始化');
            FormDbInit.MemoLog := TMemo(lclg.NewComponent('TMemo',FormDbInit));
            FormDbInit.MemoLog.Parent := FormDbInit.PanelClient;
            FormDbInit.MemoLog.Align:=alClient;

            FormDbInit.Btn_Cancel := TButton(lclg.NewComponent('TButton',FormDbInit));
            FormDbInit.Btn_Cancel.Parent := FormDbInit.PanelBottom;
            prw.SetStrProp(FormDbInit.Btn_Cancel,'Caption','取消');
            prw.SetOrdProp(FormDbInit.Btn_Cancel,'Top',10);
            prw.SetOrdProp(FormDbInit.Btn_Cancel,'Left',FormDbInit.Width-FormDbInit.Btn_Cancel.Width-50);

            mt.Code:=@TDBInitForm.CancelClick;
            mt.Data:=Pointer(TDBInitForm);
            prw.SetMethodProp(FormDbInit.Btn_Cancel,'OnClick',mt);
            //FormDbInit.PanelBottom.Caption:='this is bottom test';
            //MemoLog := TMemo.Create(FormDbInit);
            //MemoLog.ParentWindow := FormDbInit.PanelClient.Handle;
            //MemoLog.Align:= alClient;
            //MemoLog.Visible:= True;
            //MemoLog.Lines.Add('test info');
            //prw.SetStrProp(FormDbInit, 'Caption', '数据库初始化');
            //Messager.Debug('left:'+inttostr(prw.GetInt64Prop(FormDbInit,'Left')));
            //prw.SetInt64Prop(FormDbInit, 'Left', 0);
            //prw.SetInt64Prop(FormDbInit, 'Top', 0);
            //prw.SetInt64Prop(FormDbInit, 'Width', 1024);
            //prw.SetInt64Prop(FormDbInit, 'Height', 768);

            //Result := True;
          except
            on e: Exception do
              Messager.Error('DBFormCreate出现异常:'+e.Message);
          end;
          //FormDbInit.ShowModal;
          FormDbInit.Show;
        end;

end;

procedure TDBInitialize.printLog(s: string);
begin
  if Assigned(FormDbInit) then FormDbInit.printLog(s);
end;

end.

