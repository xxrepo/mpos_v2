unit uDownloadUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, FTPSend, HTTPSend, cm_netutils, cm_Messager, uPOS;

type

  { TDownloadTools }

  TDownloadTools = class(TCMMessageable)
  private
    FDownFileName: string;
    FTempFileSize: integer;
    FDownFileSize: integer;
    FTempFileName: string;
    property TempFileName: string read FTempFileName write FTempFileName;
    property TempFileSize: integer read FTempFileSize write FTempFileSize;
  private
    function DoFTPDownloadFile(Url: TCMURL; fileNameUrl: string): boolean;
    function DoHTTPDownloadFile(Address: string; fileNameUrl: string): boolean;
    function DoVerificationDownloadFile(): boolean;
  public
    property DownFileName: string read FDownFileName write FDownFileName;
    property DownFileSize: integer read FDownFileSize write FDownFileSize;
  public
    constructor Create;
    function DownloadFile(Address, SaveFileName, fileNameUrl: string): boolean;
  end;


var
  FileServiceUrl: string = '';

implementation

{ TDownloadTools }

function TDownloadTools.DownloadFile(Address, SaveFileName, fileNameUrl: string): boolean;
var
  Url: TCMURL;
  DownResult: boolean;
begin
  Result := False;
  try
    Url := TCMURL.Create(Address);

    FDownFileName := SaveFileName;
    FTempFileName := SaveFileName + '.tmp';

    //根据地址类型选择下载方式
    if SameText(Url.Protocol, 'FTP') then
      DownResult := DoFTPDownloadFile(Url, fileNameUrl)
    else if SameText(Url.Protocol, 'HTTP') then
      DownResult := DoHTTPDownloadFile(Address, fileNameUrl);

    //下载成功后验证文件
    if DownResult then
      Result := DoVerificationDownloadFile();
  finally
    Url.Free;
  end;

end;

function TDownloadTools.DoFTPDownloadFile(Url: TCMURL; fileNameUrl: string): boolean;
var
  FTPSender: TFTPSend;
begin
  Result := False;
  try
    FTPSender := TFTPSend.Create;
    FTPSender.Username := Url.Username;
    FTPSender.Password := Url.Password;
    FTPSender.TargetHost := Url.Host;
    FTPSender.TargetPort := Url.Port;

    try
      if not FTPSender.Login then
      begin
        Messager.Error('FTP下载: 登录失败!原因: %s', [FTPSender.ResultString]);
        Exit;
      end;
      Messager.Debug('FTP下载: 登录成功! ', [FTPSender.TargetHost]);
    except
      on e: Exception do
        Messager.Error('FTP下载 登录异常: ', e);
    end;

    //保存临时文件名称
    FTPSender.DirectFileName := DownFileName;
    FTPSender.DirectFile := True;
    Messager.Debug('FTP下载: 开始下载文件: %s  大小: %d ', [fileNameUrl, FTPSender.FileSize(fileNameUrl)]);

    try
      Result := FTPSender.RetrieveFile(fileNameUrl, True);
    except
      on e: Exception do
        Messager.Error('FTP下载 下载过程中异常:', e);
    end;

    if not Result then
    begin
      Messager.Error('FTP下载 下载失败。原因: %s', [FTPSender.ResultString]);
      Messager.Error('FTP下载 下载失败，开始尝试HTTP方式下载。');  //如果FTP下载失败，则用HTTP再试一次

      if not POSSupport.GetParameter.Get(POS_Service_FileService_Default).IsNull then
        FileServiceUrl := POSSupport.GetParameter.Get(POS_Service_FileService_Default).AsString;

      Result := Self.DoHTTPDownloadFile(FileServiceUrl, fileNameUrl);
      if not Result then
        Exit;
    end;

    Messager.Debug('FTP下载: 下载完成。文件: %s', [fileNameUrl]);
    try
      Messager.Debug('FTP下载: 断开连接。');
      FTPSender.Logout;
    except
      on e: Exception do
        Messager.Error('FTP下载 断开连接: ', e);
    end;
  finally
    try
      FTPSender.Free;
      Messager.Debug('FTP下载: FTP释放成功。');
    except
      on e: Exception do
        Messager.Error('FTP下载 FTP释放异常:　', e);
    end;
  end;
end;

function TDownloadTools.DoHTTPDownloadFile(Address: string; fileNameUrl: string): boolean;
var
  MaxBytes: integer;
  HTTPSender: THTTPSend;
begin
  Result := False;
  FDownFileSize := -1;
  FTempFileSize := -1;
  try
    HTTPSender := THTTPSend.Create;

    fileNameUrl := copy(fileNameUrl, 2, length(fileNameUrl));
    fileNameUrl := Address + fileNameUrl;
    Messager.Debug('文件下载 HTTP 下载 ' + fileNameUrl + ', 保存为:' + TempFileName);

    //HTTPSender.Sock.OnStatus := @myHttpStatus;
    HTTPSender.Timeout := 10000;   //10秒超时
    HTTPSender.HTTPMethod('GET', fileNameUrl);
    if (HTTPSender.ResultCode >= 100) and (HTTPSender.ResultCode <= 299) and (FDownFileSize >= 0) then
    begin
      FTempFileSize := FDownFileSize;
      HTTPSender.Document.SaveToFile(TempFileName);
      Messager.Debug('线程日志 ｜ 文件下载 HTTP 方式下载成功!');
    end
    else
    begin
      Messager.Debug('线程日志 ｜ 文件下载 HTTP 方式下载失败,HTTP返回码: %d', [HTTPSender.ResultCode]);
      Exit;
    end;
  finally
    HTTPSender.Free;
  end;

end;

function TDownloadTools.DoVerificationDownloadFile(): boolean;
begin
  try
    if not FileExists(FTempFileName) then
    begin
      Messager.Error(FTempFileName + '文件不存在，失败。');
      Exit;
    end;

    if (FileUtil.FileSize(FTempFileName) <> FTempFileSize) then
    begin
      Messager.Error('文件大小检验失败，下载失败.');
      Exit;
    end
    else
      Messager.Debug('下载文件完成后校验文件大小成功.');

    try
      // 下载完成,修改文件名
      {$IFDEF UNIX}
      fpSystem(Format('sudo chmod 777 %s', [TempFileName]));
      {$ENDIF}
      if not RenameFile(Trim(TempFileName), Trim(DownFileName)) then
      begin
        Messager.Debug('文件更名失败！tmpFilename=' + TempFileName + ', Filename=' + DownFileName);
        Exit;
      end;
      Messager.Debug('下载正常完成.');
    except
      Exit;
    end;
    //文件赋权
    {$IFDEF UNIX}
    fpSystem(Format('sudo chmod 777 %s', [DownFileName]));
    {$ENDIF}
    Messager.Debug('下载文件 "%s" 成功!', [DownFileName]);
    Result := True;
  except
    on e: Exception do
    begin
      Messager.Error('文件下载保存 | 失败: ', e);
    end;
  end;
end;

constructor TDownloadTools.Create;
begin
  inherited Create;
end;


end.


