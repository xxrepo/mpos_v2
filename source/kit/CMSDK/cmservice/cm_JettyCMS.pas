unit cm_JettyCMS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_parameter,
  cm_servlet, cm_servletutils,
  cm_jetty, cm_JettyImpl,
  cm_cmstp;

type

  { TCMSServer }

  TCMSServer = class(TServer, ICMSTPService)
  public //ICMSTPService
    function CMSTP(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponseContent: ICMConstantParameterDataList): Boolean;
  end;

implementation

{ TCMSServer }

function TCMSServer.CMSTP(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponseContent: ICMConstantParameterDataList): Boolean;
var
  request: TJettyServletRequest;
  response: TServletResponse;
  dl: ICMParameterDataList;
  i: Integer;
begin
  Result := False;
  request := TJettyServletRequest.Create(AURL, Self);
  response := TServletResponse.Create;
  //请求参数
  if Supports(ARequestParameters, ICMParameterDataList, dl) then
    request.Parameters := dl
  else
    begin
      for i:=0 to ARequestParameters.Count-1 do
        request.Parameters.SetData(ARequestParameters.GetName(i), ARequestParameters.Get(i));
    end;
  //
  Self.Handle(AURL, request, response);
  TheResponseContent := response.GetContent;
  response.Committed := True;
end;

end.

