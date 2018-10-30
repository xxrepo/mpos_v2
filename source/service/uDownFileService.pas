unit uDownFileService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_sysutils;

type
  enmuFileDownTaskType = (tftpType, thttpType, tftpAndhttpType);
  enmuFileDownTaskStatue = (sNone, sWaiting, sWorking, sPauseing, sFinished);

  { TFileDownTask }

  TFileDownTask = class(TPersistent)
  private
    FTaskId: string;    //任务ID号,建议是GUID
    FTaskType: enmuFileDownTaskType;  //任务类型
    FTaskStatus: enmuFileDownTaskStatue;//任务状态
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
    property TaskType: enmuFileDownTaskType read FTaskType write FTaskType;
    property TaskStatus: enmuFileDownTaskStatue read FTaskStatus write FTaskStatus;
    property ServerFileName: string read FServerFileName write FServerFileName;
    property LocalFileName: string read FLocalFileName write FLocalFileName;
    property DownFileFileSize: integer read FDownFileFileSize write FDownFileFileSize;
    property DownFileNowSize: integer read FDownFileNowSize write FDownFileNowSize;
    property TaskCreateTime: TDateTime read FTaskCreateTime write FTaskCreateTime;
    property ErrorInfo: string read FErrorInfo write FErrorInfo;
  end;

  IFileDownService = interface(ICMBase)
    ['{F030E5D8-13FB-4D74-BB7E-AEBB629AA39D}']
    //新增一个下载任务
    function addTask(Task: TFileDownTask): boolean;
    //删除一个任务
    function delTask(Task: TFileDownTask): boolean;
    //取任务信息
    function getTaskInfo(TaskId: string; out Task: TFileDownTask): boolean;
    //检查线程
    function checkThreadList: boolean;
  end;

implementation

{ TFileDownTask }

function TFileDownTask.Init: boolean;
begin
  FTaskType := enmuFileDownTaskType.tftpType;  //任务类型
  FTaskStatus := enmuFileDownTaskStatue.sNone;//任务状态
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

