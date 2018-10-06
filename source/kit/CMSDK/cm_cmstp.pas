unit cm_cmstp;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_net,
  cm_parameter, cm_ParameterUtils,
  cm_servlet;


type

  { ICMSTPResponse }
  ICMSTPResponse = interface(ICMBase)
    ['{0B1111A0-9270-4A4D-BFED-CEEFE97D344A}']
    function GetContentType: string;
    function GetContent: ICMConstantParameterDataList;
  end;

  ICMSTP = interface(ICMBase)
    ['{E3875881-4509-470D-A07F-0185FD4FEA13}']
    function Post(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponse: ICMSTPResponse): Boolean;
  end;

  //ICMSTP = interface(ICMBase)
  //  ['{43A79E53-A8D0-472C-93B1-3EE0710A7E05}']
  //  function AddServlet(const AName: string; AServlet: IServlet): IServletHolder;
  //end;


  { TCMSTPResponse }

  TCMSTPResponse = class(TCMBase, ICMSTPResponse)
  private
    FContentType: string;
    FContent: ICMConstantParameterDataList;
  public
    constructor Create(const AContentType: string; AContent: ICMConstantParameterDataList);
    destructor Destroy; override;
    function GetContentType: string;
    function GetContent: ICMConstantParameterDataList;
  end;

  { TCMSTPURLConnection
    //TODO 连接未有响应时，响应体为空的情况；是否请求成功的标识
  }
  TCMSTPURLConnection = class(TURLConnection)
  private
    class var FCMSTPService: ICMSTP;
  public
    class property CMSTPService: ICMSTP write FCMSTPService;
  public
    constructor Create(const AURL: string); override;
    procedure Connect; override;
  end;

implementation

{ TCMSTPResponse }

constructor TCMSTPResponse.Create(const AContentType: string; AContent: ICMConstantParameterDataList);
begin
  FContentType := AContentType;
  FContent := AContent;
end;

destructor TCMSTPResponse.Destroy;
begin
  FContent := nil;
  inherited Destroy;
end;

function TCMSTPResponse.GetContentType: string;
begin
  Result := FContentType;
end;

function TCMSTPResponse.GetContent: ICMConstantParameterDataList;
begin
  Result := FContent;
end;

{ TCMSTPURLConnection }

constructor TCMSTPURLConnection.Create(const AURL: string);
begin
  inherited Create(AURL);
  FRequestParameters := TCMParameterDataList.Create;
end;

procedure TCMSTPURLConnection.Connect;
var
  rsp: ICMSTPResponse;
begin
  FConnected := False;
  FContentType := '';
  FResponseContent := nil;
  if Assigned(FCMSTPService) then
    begin
      if FCMSTPService.Post(URL, FRequestParameters, rsp) then
        begin
          FContentType := rsp.GetContentType;
          FResponseContent := rsp.GetContent;
          FConnected := True;
        end;
    end;
end;



end.

