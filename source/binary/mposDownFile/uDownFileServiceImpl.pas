unit uDownFileServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, DateUtils, ftpsend, httpsend, blcksock,
  cm_messager, uDownFileService;

type


  { TFileDownTool }

  TFileDownTool = class(TCMMessageable)
  private
    FTask:TFileDownTask;
    FHttpClient : THttpSend;
  public
    function getTask:TFileDownTask;
    function setTask(Task:TFileDownTask):boolean;
    function ftpdownfile:boolean;
    function GetSizeFromHeader(Header: string): integer;
    procedure myHttpStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
    function httpdownfile:boolean;
    function downFile:boolean;
  end;


  { TFileDownService }

  TFileDownService = class(TCMMessageable,IFileDownService)
  private
    FTaskList : TStringList;
    FThreadLock : TCriticalSection;
    FFileDownThreadList : TStringList;    //文件下载线程列表，默认有1个线程，如果一个线程超时，则得新再起一个线程
  public
    constructor Create;
    function getTaskList:TStringList;
    function checkTaskInList(Task:TFileDownTask):boolean;
    //新增一个下载任务
    function addTask(Task:TFileDownTask):boolean;
    function delTask(Task:TFileDownTask):boolean;
    //取任务信息
    function getTaskInfo(TaskId:string;out Task:TFileDownTask):boolean;
    //检查线程
    function checkThreadList:boolean;
    //取出一个待执行的任务
    function getWaitTask:TFileDownTask;
  end;

    { TFileDownThread }

  TFileDownThread = class(TThread)
  private
    FLastRunTime: TDateTime;
    FdownClient: TFileDownTool;
    Ffds: TFileDownService;
  public
    constructor Create(Task:TFileDownTask);
    destructor Destroy;
    function setFileDownService(fds: TFileDownService): boolean;
    function getLastRunTime:TDateTime;
    procedure execute;override;
  end;

implementation


{ TFileDownTool }

function TFileDownTool.getTask: TFileDownTask;
begin
  Result := FTask;
end;

function TFileDownTool.setTask(Task: TFileDownTask): boolean;
begin
  FTask := Task;
  Messager.Debug('setTask:'+task.DownFileName);
  Result := True;
end;

function TFileDownTool.ftpdownfile: boolean;
var ftpclient: TFTPSend;
begin
  Messager.Debug('开始FTP下载文件:'+FTask.DownFileName);
  Result := False;
  ftpclient := TFTPSend.Create;
  //ftp://ftpuser:myj@123456@mposftp.myj.com.cn:6002
  ftpclient.UserName:='ftpuser';
  ftpclient.Password:='myj@123456';
  ftpclient.TargetHost:='mposftp.myj.com.cn';
  ftpclient.TargetPort:='6002';
  try
    if not ftpclient.Login then
    begin
      //登录失败
      FTask.ErrorInfo:='ftp登录失败。';
      FTask.TaskStatus:= sPauseing;
      Messager.Debug('FTP登录失败。');
      Exit;
    end else
      Messager.Debug('FTP登录成功。');
    try
      //取出ftp上待下载文件的大小
      FTask.DownFileFileSize:= ftpclient.FileSize(FTask.DownFileName);
      if FTask.DownFileFileSize <=0 then
      begin
        FTask.ErrorInfo:='服务器上不存在此文件';
        Messager.Debug('待下载的文件不存在:'+FTask.DownFileName);
        FTask.TaskStatus:= sFinished;  //标示下载完成。
        Result := True;  //文件不存在，也要让任务完成，不要重复执行下载，就当这个任务失败了。
        Exit;
      end;
      Messager.Debug('待下载的文件大小:'+IntToStr(FTask.DownFileFileSize));
      //目标文件
      ftpclient.DirectFileName:= FTask.SaveFileName;
      ftpclient.DirectFile:= True;
      try
        FTask.TaskStatus:= sWorking;
        if ftpclient.RetrieveFile(FTask.DownFileName,True) then //true为支持断点
        begin
          Result := True;
          FTask.TaskStatus:= sFinished;
          Messager.Debug('文件下载成功');
        end else
        begin
          FTask.ErrorInfo:='文件下载失败,稍后重试';
          Messager.Debug('文件下载失败,稍后重试');
        end;
      except on e: Exception do
        Messager.Debug('下载异常:'+e.Message);
      end;

    finally
      if ftpclient.Logout then
        Messager.Debug('断开ftp成功')
      else
        Messager.Debug('断开ftp失败');
    end;
  finally
    if Assigned(ftpclient) then FreeAndNil(ftpclient);
  end;
end;

function TFileDownTool.GetSizeFromHeader(Header: string): integer;
var
  item: TStringList;
begin
  //the download size is contained in the header (e.g.: Content-Length: 3737722)
  Result := -1;

  if system.Pos('Content-Length:', Header) <> 0 then
  begin
    item := TStringList.Create();
    item.Delimiter := ':';
    item.StrictDelimiter := True;
    item.DelimitedText := Header;
    if item.Count = 2 then
    begin
      Result := StrToInt(Trim(item[1]));
    end;
  end;
end;

procedure TFileDownTool.myHttpStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
var
  currentHeader: string;
  i: integer;
begin
  //try to get filesize from headers
  if (FTask.DownFileFileSize = -1) then
  begin
    for i := 0 to FHttpClient.Headers.Count - 1 do
    begin
      currentHeader := FHttpClient.Headers[i];
      FTask.DownFileFileSize := GetSizeFromHeader(currentHeader);
      if FTask.DownFileFileSize <> -1 then
      begin
        Messager.Debug('HTTP方式读取到需下载文件大小为:' + IntToStr(FTask.DownFileFileSize));
        break;
      end;
    end;
  end;
end;

function TFileDownTool.httpdownfile: boolean;
begin
  Result := False;
  FTask.DownFileFileSize:= -1;
  FhttpClient := THttpSend.Create;
  try
    FTask.TaskStatus:= sWorking;
    FHttpClient.Sock.OnStatus := @myHttpStatus;
    FHttpClient.Timeout := 10000;   //10秒超时
    FhttpClient.HTTPMethod('GET', FTask.DownFileName);
    if (FhttpClient.ResultCode >= 100) and (FhttpClient.ResultCode <= 299) and (FTask.DownFileFileSize >= 0) then
    begin
      FhttpClient.Document.SaveToFile(FTask.SaveFileName);
      FTask.TaskStatus:= sFinished;
      Result := True;
      Messager.Debug('文件下载 HTTP 方式下载成功,文件大小=' + IntToStr(FTask.DownFileFileSize));
    end else
      Messager.Debug('线程日志 ｜ 文件下载 HTTP 方式下载失败,HTTP返回码:' + IntToStr(FhttpClient.ResultCode));
  finally
    if Assigned(FHttpClient) then FreeAndNil(FHttpClient);
  end;
end;

function TFileDownTool.downFile: boolean;
begin
  Result := False;
  case FTask.TaskType of
    tftpType:
      begin
        Result := ftpdownfile;
      end;
    thttpType:
      begin
        Result := httpdownfile;
      end;
    tftpAndhttpType:
      begin
        Result := ftpdownfile;
        if not Result then
          Result := httpdownfile;
      end;
  end;
end;

{ TFileDownThread }

constructor TFileDownThread.Create(Task:TFileDownTask);
begin
  inherited Create(True);
  FLastRunTime := now;
  FdownClient := TFileDownTool.Create;
  FdownClient.setTask(Task);
  //Resume;
end;

destructor TFileDownThread.Destroy;
begin
  if Assigned(FdownClient) then FreeAndNil(FdownClient);
  inherited Destroy;
end;

function TFileDownThread.getLastRunTime: TDateTime;
begin
  Result := FLastRunTime;
end;

function TFileDownThread.setFileDownService(fds: TFileDownService): boolean;
begin
  Ffds := fds;
  Result := True;
end;

procedure TFileDownThread.execute;
begin
  while not Terminated do
  begin
    FLastRunTime := now;
    if not self.FdownClient.downFile then
      sleep(2000)  //这里应该要暂停2分钟，做任务循环下载用的 ，有些网络不好的，可会重复下载的，有断点续传
    else
      Terminate;
  end;
end;

{ TFileDownService }

constructor TFileDownService.Create;
begin
  inherited Create;
  FTaskList := TStringList.Create;
  FFileDownThreadList := TStringList.Create;
  FThreadLock := TCriticalSection.Create;
end;

function TFileDownService.getTaskList: TStringList;
begin
  Result := FTaskList;
end;

function TFileDownService.checkTaskInList(Task: TFileDownTask): boolean;
var i : integer;
begin
  Result := False;
  FThreadLock.Enter;
  try
    for i := 0 to getTaskList.Count - 1 do
    begin
      if TFileDownTask(getTaskList.Objects[i]).TaskId = Task.TaskId then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    FThreadLock.Leave;
  end;

end;

function TFileDownService.addTask(Task:TFileDownTask):boolean;
var fdt: TFileDownThread;
begin
  Result := False;
  if not checkTaskInList(Task) then
  begin
    FThreadLock.Enter;
    try
      getTaskList.AddObject(Task.TaskId,Task);
    finally
      FThreadLock.Leave;
    end;
    Messager.Debug('成功增加一个文件下载任务');
  end;
  //新建一个线程，并开始下载文件
  fdt := TFileDownThread.Create(Task);
  fdt.Start;
  FFileDownThreadList.AddObject(IntToStr(fdt.ThreadID),fdt);
  Result := True;
end;

function TFileDownService.delTask(Task: TFileDownTask): boolean;
var i : integer;
begin
  Result := False;
  FThreadLock.Enter;
  try
    for i := 0 to getTaskList.Count - 1 do
    begin
      if Task.TaskId = TFileDownTask(getTaskList.Objects[i]).TaskId then
      begin
        getTaskList.Objects[i].Free;
        getTaskList.Delete(i);
        Messager.Debug('成功删除一个文件下载任务');
        Exit;
      end;
    end;
  finally
    FThreadLock.Leave;
  end;
  Messager.Debug('任务队列中没有找到需要删除的下载任务');
end;

function TFileDownService.getTaskInfo(TaskId:string;out Task: TFileDownTask): boolean;
var i : integer;
begin
  Result := False;
  FThreadLock.Enter;
  try
    for i := 0 to getTaskList.Count - 1 do
    begin
      if TFileDownTask(getTaskList.Objects[i]).TaskId = TaskId then
      begin
        Task := TFileDownTask(getTaskList.Objects[i]);
        Result := True;
        Messager.Debug('取出下载任务信息成功:'+TaskId);
        Exit;
      end;
    end;
  finally
    FThreadLock.Leave;
  end;
  Messager.Debug('末找到下载任务信息:'+TaskId);
end;

function TFileDownService.checkThreadList: boolean;
begin
  Result := False;
  //这里规划做一个线程的检查机制，防止线程卡死占用资源的

end;

function TFileDownService.getWaitTask: TFileDownTask;
var i : integer;
begin
  Result := nil;
  FThreadLock.Enter;
  try
    for i := 0 to getTaskList.Count - 1 do
    begin
      if TFileDownTask(getTaskList.Objects[i]).TaskStatus = enmuFileDownTaskStatue.sWaiting then
      begin
        Result := TFileDownTask(getTaskList.Objects[i]);
        Messager.Debug('取出一个待下载的任务:'+TFileDownTask(getTaskList.Objects[i]).TaskId);
      end;
    end;
  finally
    FThreadLock.Leave;
  end;

end;

end.

