unit uDownFileService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_sysutils;

type                   //  末知     ftp下载     http下载       ftp和http下载    ftp上传
  enmuFileDownTaskType = (tNone=-1,tftpType=0, thttpType=1, tftpAndhttpType=2, tftpupType=3);
                          //无此任务  无状态     等待       工作         暂停          完成
  enmuFileDownTaskStatue = (sNoTsk=-1,sNone=0, sWaiting=1, sWorking=2, sPauseing=3, sFinished=4);
  //下载的文件类型  末知   数据初始化文件  数据更新包   广告文件  数据库上传   日志上传
  enmuFileType = (ftNone=-1,ftDbInitFile=0,ftDataPack=1,ftAdv=2,ftDbUpload=3,ftLogUpload=4);
  { TFileDownTask }

  TFileDownTask = class(TPersistent)
  private
    FTaskId: string;    //任务ID号,建议是GUID
    FTaskFileType:enmuFileType;  //文件类型  以这个来读取参数，获取相应的ftp地址
    FSourceKeyId:string;      //记录任务的来源ID，比如数据包，可以记录数据包的任务ID,这样好取回任务的信息
    FDownTaskType: enmuFileDownTaskType;  //任务下载类型
    FTaskStatus: enmuFileDownTaskStatue;//任务状态
    FTaskSuccess:boolean;         //任务是否成功
    FServerFileName: string;                //服务端的文件名
    FLocalFileName: string;            //本地的文件名
    FDownFileFileSize: integer;             //需要下载的文件的文件总大小
    FDownFileNowSize: integer;             //下载的文件当前已经下载的大小
    FTaskCreateTime: TDateTime;           //任务建立的时间
    FErrorInfo: string;    //任务的错误信息
    function Init: boolean;//初始化参数信息
  published
    constructor Create; overload;
    constructor Create(const AUUID: string); overload;
    property TaskId: string read FTaskId write FTaskId;
    property TaskFileType: enmuFileType read FTaskFileType write FTaskFileType;
    property SourceKeyId: string read FSourceKeyId write FSourceKeyId;
    property DownTaskType: enmuFileDownTaskType read FDownTaskType write FDownTaskType;
    property TaskStatus: enmuFileDownTaskStatue read FTaskStatus write FTaskStatus;
    property TaskSuccess: boolean read FTaskSuccess write FTaskSuccess;
    property ServerFileName: string read FServerFileName write FServerFileName;
    property LocalFileName: string read FLocalFileName write FLocalFileName;
    property DownFileFileSize: integer read FDownFileFileSize write FDownFileFileSize;
    property DownFileNowSize: integer read FDownFileNowSize write FDownFileNowSize;
    property TaskCreateTime: TDateTime read FTaskCreateTime write FTaskCreateTime;
    property ErrorInfo: string read FErrorInfo write FErrorInfo;
  end;

  IFileDownService = interface(ICMBase)
    ['{0B7C355C-50A5-4EB4-8292-5C7A91F84BC9}']
    //新增一个下载任务   boSaveDb是否保存任务到数据库，默认是True保存
    function addTask(Task:TFileDownTask;boSaveDb:boolean=True): boolean;
    //删除一个任务  boDelDb是否也要将数据库的数据删除 默认是True删除
    function delTask(Task:TFileDownTask;boDelDb:boolean=True): boolean;
    //取任务信息 任务号
    function getTaskInfoByTaskId(TaskId:string; out Task:TFileDownTask): boolean;
    //取任务信息 来源id
    function getTaskInfoBySourceKeyId(SourceKeyId: string; out Task: TFileDownTask): boolean;
    //取出一个待执行的任务
    function getWaitTask:TFileDownTask;
    //检查线程
    function checkThreadList: boolean;
  end;

implementation

{ TFileDownTask }

function TFileDownTask.Init: boolean;
begin
  FTaskFileType := enmuFileType.ftNone; //默认末知的文件类型
  FSourceKeyId:=''; //来源信息
  FDownTaskType := enmuFileDownTaskType.tftpType;  //任务类型
  FTaskStatus := enmuFileDownTaskStatue.sNone;//任务状态
  FTaskSuccess := False;   //默认任务是没有下载成功的
  FServerFileName := '';                //需要下载的文件名
  FLocalFileName := '';            //需要保存目标文件名
  FDownFileFileSize := -1;             //需要下载的文件的文件总大小
  FDownFileNowSize := -1;             //下载的文件当前已经下载的大小
  FTaskCreateTime := Now;           //任务建立的时间
  FErrorInfo := '';    //任务的错误信息
  Result := True;
end;

constructor TFileDownTask.Create;
begin
  TaskId := CreateGUIDStr;
  Init;
end;

constructor TFileDownTask.Create(const AUUID: string);
begin
  TaskId := AUUID;
  Init;
end;

end.

