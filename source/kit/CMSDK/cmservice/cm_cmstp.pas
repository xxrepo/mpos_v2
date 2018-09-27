unit cm_cmstp;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces,
  cm_net, cm_servlet,
  cm_parameter,
  cm_ParameterUtils;


type

  { TCMSTPURLConnection
    //TODO 连接未有响应时，响应体为空的情况；是否请求成功的标识
  }
  TCMSTPURLConnection = class(TURLConnection)
  public
    constructor Create(const AURL: string); override;
    procedure Connect; override;
  end;

  ICMSTPResponse = interface(ICMBase)
    ['{0B1111A0-9270-4A4D-BFED-CEEFE97D344A}']
    function GetContentType: string;
    function GetContent: ICMConstantParameterDataList;
  end;

  ICMSTPService = interface(ICMBase)
    ['{5B139E17-51B5-44B2-9045-E563EEEBB66B}']
    function CMSTP(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponse: ICMSTPResponse): Boolean;
  end;

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

  { IServletContainer
        这是一个 servlet 容器接口
    用于动态配置 servlet 应用时先行放入容器，后继可以依据配置的 code 找到相应的 servlet。
    由此，也意味着对于一个 servlet 应具有唯一的 code，这个 code 仅仅用于辨别无其他意义。
  }
  IServletCollection = interface(ICMBase)
    ['{04970409-8397-4A99-9BE6-EB46638B66BB}']
    function AddServlet(const ACode: string; AServlet: IServlet): Boolean;
    function AddFilter(const ACode: string; AFilter: IFilter): Boolean;
    function AddListener(const ACode: string; AListener: IListener): Boolean;
  end;

var
  CMSTPService: ICMSTPService = nil;

implementation

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
  if Assigned(CMSTPService) then
    begin
      if CMSTPService.CMSTP(URL, FRequestParameters, rsp) then
        begin
          FContentType := rsp.GetContentType;
          FResponseContent := rsp.GetContent;
          FConnected := True;
        end;
    end;
end;

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



end.

