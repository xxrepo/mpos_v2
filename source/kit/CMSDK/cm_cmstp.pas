unit cm_cmstp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces,
  cm_net,
  cm_parameter,
  cm_ParameterUtils,
  cm_servlet;


type

  ICMSTPService = interface(ICMBase)
    ['{5B139E17-51B5-44B2-9045-E563EEEBB66B}']
    function Service(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponseContent: ICMConstantParameterDataList): Boolean;
  end;

  { TCMSTPURLConnection }

  TCMSTPURLConnection = class(TURLConnection)
  private

  public
    constructor Create(const AURL: string); override;
    procedure Connect; override;
  end;

  //IServletFindRegister = interface(ICMBase)
  //  ['{FDF9A773-5EE7-4027-A0DF-109AB8245B82}']
  //  function Put(const AContextPath: string; AServletFind: IServletFind): Boolean;
  //  function Get(const AContextPath: string): IServletFind;
  //end;

  { IServletContainer
        这是一个 servlet 容器接口
    用于动态配置 servlet 应用时先行放入容器，后继可以依据配置的 code 找到相应的 servlet。
    由此，也意味着对于一个 servlet 应具有唯一的 code，这个 code 仅仅用于辨别无其他意义。
  }
  IServletContainer = interface(ICMBase)
    ['{04970409-8397-4A99-9BE6-EB46638B66BB}']
    function Put(const ACode: string; AServlet: IServlet): Boolean;
    function Get(const ACode: string): IServlet;
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
begin
  FResponseContent := nil;
  if Assigned(CMSTPService) then
    begin
      CMSTPService.Service(URL, FRequestParameters, FResponseContent);
    end;
end;

end.

