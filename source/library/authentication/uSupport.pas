unit uSupport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  cm_interfaces, cm_messager, cm_parameter, cm_parameterutils, cm_dialogs,
  uPOS;

type

  { TSupport }

  TSupport = class(TCMMessageable, ISupport)
  private
    FParameter: ICMParameter;
    procedure InitParamter;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetParameter: ICMParameter;
  end;


implementation

uses cm_controlutils, uDialogs;

{ TPOSSystem }

procedure TSupport.InitParamter;
begin
  GetParameter.AddString(POS_System_UserCode, '');
  GetParameter.AddString(POS_System_ShopCode, '');
  GetParameter.AddString(POS_System_AuthCode, '');
  GetParameter.AddString(POS_System_TermCode, '');
  GetParameter.AddString(POS_System_TermUUID, '');
  GetParameter.AddString(POS_System_CompCode, '');
  GetParameter.AddString(POS_System_AreaConfig, '');
  GetParameter.AddString(POS_System_MacAddress, '');
  GetParameter.AddString(POS_System_IntranetIP, '');
  GetParameter.AddString(POS_System_InternetIP, '');
  GetParameter.AddString(POS_System_NeedInitTask, '');
  GetParameter.AddString(POS_System_InitTaskCode, '');

  //服务参数节点
  GetParameter.AddString(POS_Service_POSService, 'POSService');

  GetParameter.AddInteger(POS_Service_POSCommandInterval, 300);
  GetParameter.AddInteger(POS_Service_DataUpdateInterval, 300);
  GetParameter.AddInteger(POS_Service_DataUploadInterval, 300);
  GetParameter.AddInteger(POS_Service_ExceuteInterval, 5);

  GetParameter.AddString(POS_Service_FileService_Default, 'Default');
  GetParameter.AddString(POS_Service_FileService_Upgrade, 'Upgrade');
  GetParameter.AddString(POS_Service_FileService_Advertise, 'Advertise');
  GetParameter.AddString(POS_Service_FileService_DataPacket, 'DataPacket');
  GetParameter.AddString(POS_Service_FileService_UploadFile, 'UploadFile');
  GetParameter.AddString(POS_Service_MYJTicketService, 'MYJTicketService');
  GetParameter.AddString(POS_Service_BaozhangService, 'BaozhangService');
end;

constructor TSupport.Create;
begin
  inherited Create;
  FParameter := TCMParameter.Create(nil, 'support_root', '');
  InitParamter;
end;

destructor TSupport.Destroy;
begin
  inherited Destroy;
end;

function TSupport.GetParameter: ICMParameter;
begin
  Result := FParameter;
end;


end.

