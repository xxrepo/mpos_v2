unit cm_servletutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_messager,
  cm_servlet, cm_parameter, cm_ParameterUtils,
  cm_netutils;

type

  { TServletRequest }

  TServletRequest = class(TCMBase, IServletRequest)
  protected
    FAttributes: ICMParameterDataList;
    FParameters: ICMParameterDataList;
    FURL: string;
    FProtocol, FHost: string;
    FPort: Word;
    FContextPath, FServletPath: string;
  public
    constructor Create(const AURL: string);
  public
    function GetAttributes: ICMParameterDataList;
    function GetParameters: ICMConstantParameterDataList;
    function GetRequestURL: string;
    function GetProtocol: string;
    function GetHost: string;
    function GetPort: Word;
    function GetContextPath: string;
    function GetServletPath: string;
    function GetRequestDispatcher(const APath: string): IRequestDispatcher; virtual; abstract;
  end;

  { TServletConfig }

  TServletConfig = class(TCMMessageable, IServletConfig)
  private
    FName: string;
    FContent: IServletContext;
    FInitParameterDataList: ICMParameterDataList;
  public
    constructor Create(const AName: string; AContext: IServletContext);
    destructor Destroy; override;
    property InitParameterDataList: ICMParameterDataList read FInitParameterDataList;
  public
    function GetServletName: string;
    function GetInitParameters: ICMConstantParameterDataList;
    function GetServletContext: IServletContext;
  end;

  { TServletContext }

  TServletContext = class(TCMMessageable, IServletContext)
  private
    FContextPath: string;
    FAttributes: ICMParameterDataList;
    FInitParameters: ICMParameterDataList;
    FServerInfo: string;
  public
    constructor Create;
    destructor Destroy; override;
    property ContextPath: string write FContextPath;
    property ServerInfo: string write FServerInfo;
  public //IServletContext
    function GetAttributes: ICMParameterDataList;
    function GetContext(const APath: string): IServletContext;
    function GetContextPath: string;
    function GetInitParameters: ICMConstantParameterDataList;
    function GetServletContextName: string;
    function GetNamedDispatcher(const AName: string): IRequestDispatcher; virtual; abstract;
    function GetRequestDispatcher(const APath: string): IRequestDispatcher; virtual; abstract;
    function GetServerInfo: string;
  end;

  { TServletResponse }

  TServletResponse = class(TCMMessageable, IServletResponse)
  private
    FCommitted: Boolean;
    FContentType: string;
    FContent: ICMParameterDataList;
  public
    constructor Create;
    destructor Destroy; override;
    property Committed: Boolean write FCommitted;
  public
    function IsCommitted: Boolean;
    procedure SetContentType(const AType: string);
    function GetContentType: string;
    function GetContent: ICMParameterDataList;
  end;


implementation

{ TServletRequest }

constructor TServletRequest.Create(const AURL: string);
var
  u: TCMURL;
begin
  FAttributes := TCMParameterDataList.Create;
  FParameters := TCMParameterDataList.Create;
  FURL := AURL;
  u := TCMURL.Create(FURL);
  try
    FProtocol := u.Protocol;
    FHost := u.Host;
    FPort := StrToIntDef(u.Port, 80);
    FContextPath := u.ContextPath;
    FServletPath := u.ServletPath;
  finally
    u.Free;
  end;
end;

function TServletRequest.GetAttributes: ICMParameterDataList;
begin
  Result := FAttributes;
end;

function TServletRequest.GetParameters: ICMConstantParameterDataList;
begin
  Result := FParameters;
end;

function TServletRequest.GetRequestURL: string;
begin
  Result := FURL;
end;

function TServletRequest.GetProtocol: string;
begin
  Result := FProtocol;
end;

function TServletRequest.GetHost: string;
begin
  Result := FHost;
end;

function TServletRequest.GetPort: Word;
begin
  Result := FPort;
end;

function TServletRequest.GetContextPath: string;
begin
  Result := FContextPath;
end;

function TServletRequest.GetServletPath: string;
begin
  Result := FServletPath;
end;

{ TServletResponse }

constructor TServletResponse.Create;
begin
  inherited Create;
  FCommitted := False;
  FContentType := '';
  FContent := TCMParameterDataList.Create;
end;

destructor TServletResponse.Destroy;
begin
  Messager.Warning('Destroy()...');
  inherited Destroy;
end;

function TServletResponse.IsCommitted: Boolean;
begin
  Result := FCommitted;
end;

procedure TServletResponse.SetContentType(const AType: string);
begin
  FContentType := AType;
end;

function TServletResponse.GetContentType: string;
begin
  Result := FContentType;
end;

function TServletResponse.GetContent: ICMParameterDataList;
begin
  Messager.Warning('GetContent()...' + BoolToStr(Assigned(FContent), True));
  Result := FContent;
end;

{ TServletConfig }

constructor TServletConfig.Create(const AName: string; AContext: IServletContext);
begin
  inherited Create;
  FName := AName;
  FContent := AContext;
  FInitParameterDataList := TCMParameterDataList.Create;
end;

destructor TServletConfig.Destroy;
begin
  FInitParameterDataList := nil;
  inherited Destroy;
end;

function TServletConfig.GetServletName: string;
begin
  Result := FName;
end;

function TServletConfig.GetInitParameters: ICMConstantParameterDataList;
begin
  Result := FInitParameterDataList;
end;

function TServletConfig.GetServletContext: IServletContext;
begin
  Result := FContent;
end;

{ TServletContext }

constructor TServletContext.Create;
begin
  inherited Create;
  FAttributes := TCMParameterDataList.Create;
  FInitParameters := TCMParameterDataList.Create;
end;

destructor TServletContext.Destroy;
begin
  FAttributes := nil;
  FInitParameters := nil;
  inherited Destroy;
end;

function TServletContext.GetAttributes: ICMParameterDataList;
begin
  Result := FAttributes;
end;

function TServletContext.GetContext(const APath: string): IServletContext;
begin
  Result := nil;
end;

function TServletContext.GetContextPath: string;
begin
  Result := FContextPath;
end;

function TServletContext.GetInitParameters: ICMConstantParameterDataList;
begin
  Result := FInitParameters;
end;

function TServletContext.GetServletContextName: string;
begin
  Result := FInitParameters.Get('display-name').AsString;
end;

function TServletContext.GetServerInfo: string;
begin
  Result := FServerInfo;
end;





end.

