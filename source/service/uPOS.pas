unit uPOS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_parameter,
  uApp;

const
  //系统参数节点
  POS_System_UserCode: string = 'UserCode';
  POS_System_ShopCode: string = 'ShopCode';
  POS_System_AuthCode: string = 'AuthCode';
  POS_System_TermCode: string = 'TermCode';
  POS_System_TermUUID: string = 'TermUUID';
  POS_System_CompCode: string = 'CompCode';
  POS_System_AreaConfig: string = 'AreaConfig';
  POS_System_MacAddress: string = 'MacAddress';
  POS_System_IntranetIP: string = 'IntranetIP';
  POS_System_InternetIP: string = 'InternetIP';
  POS_System_NeedInitTask: string = 'NeedInitTask';
  POS_System_InitTaskCode: string = 'InitTaskCode';

  POS_Service_POSCommandInterval: string = 'POSCommandInterval';
  POS_Service_DataUpdateInterval: string = 'DataUpdateInterval';
  POS_Service_DataUploadInterval: string = 'DataUploadInterval';
  POS_Service_ExceuteInterval: string = 'ExceuteInterval';

  //服务参数节点
  POS_Service: string = 'Service';
  POS_Service_POSService: string = 'POSService';
  POS_Service_MYJTicketService: string = 'MYJTicketServic';
  POS_Service_BaoZhangService: string = 'BaoZhangService';
  POS_Service_FileServices: string = 'FileServeices';
  POS_Service_FileService_Default: string = 'default';  //'ftp://ftpuser:myj@123456@mcloudftp.myj.com.cn:21';
  POS_Service_FileService_Upgrade: string = 'Upgrade';  //'ftp://ftpuser:myj@123456@mcloudftp.myj.com.cn:21';
  POS_Service_FileService_Advertise: string = 'Advertise'; //'ftp://myj-download:M1y2j3.4*5*6_@120.24.51.67:8021';
  POS_Service_FileService_DataPacket: string = 'DataPacket';//'ftp://ftpuser:myj@123456@mposftp.myj.com.cn:6002';
  POS_Service_FileService_UploadFile: string = 'UploadFile';//'ftp://ftpUpload:mpos@123456@mposftp.myj.com.cn:6002';

type

  ISupport = interface
    ['{231F7F25-E5CE-4FCE-B7E7-93C8BCAE8C3B}']
    function GetParameter: ICMParameter;
  end;

function POSSupport: ISupport;

implementation

var
  _POSSupport: ISupport = nil;

function POSSupport: ISupport;
begin
  Result := nil;
  if not Assigned(_POSSupport) then
    if not InterfaceRegister.OutInterface(ISupport, _POSSupport) then
      Exit;
  Result := _POSSupport;
end;


end.
