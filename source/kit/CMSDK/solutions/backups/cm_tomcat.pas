unit cm_tomcat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Dialogs,
  cm_interfaces, cm_DOM, cm_XML,
  cm_servlet, cm_servletutils,
  cm_parameter, cm_ParameterUtils,
  cm_cmstp;


type

  TService = class;

  (********************************************************************************************)

  { TServer }

  TServer = class
  private
    FServiceList: TFPHashObjectList;
  public
    constructor Create;
    procedure AddService(AService: TService);
    procedure ParseConfig(const AConfigFileName: string);
    //procedure Start;
    //procedure Stop;
  end;

  TConnector = class;
  TEngine = class;

  { TService }

  TService = class(TCMBase, ICMSTPService) //Server中的一个逻辑功能层， 一个Server可以包含多个Service
  private
    FName: string;
    FConnectorList: TFPHashObjectList;
    FEngine: TEngine;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName;
    procedure AddConnector(AConnector: TConnector);
    procedure SetEngine(AEngine: TEngine);
  public
    function Service(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponseContent: ICMConstantParameterDataList): Boolean;
  end;

  TConnector = class  //连接器，一个Service可以有多个Connector，主要是连接客户端请求
  private
    FProtocol: string;
  public
    property Protocol: string read FProtocol;
  end;

  TAdapter = class //适配器，用于生成ServletRequest交给Container进行具体的处理。
  public
    function Adapt(ARequestParameters: ICMConstantParameterDataList): TServletRequest; virtual; abstract;
  end;

  { TProtocolHandler }

  TProtocolHandler = class //代表不同的连接类型
  private
    FAdapter: TAdapter;
  public
    function GetProtocol: string; virtual; abstract;
    procedure SetAcceptor(AAdapter: TAdapter);
    procedure Accpt(ARequestParameters: ICMConstantParameterDataList); virtual; abstract;
  end;

  { TContainer }

  TContainer = class //Service的另一个核心组件，按照层级有Engine，Host，Context，Wrapper四种，一个Service只有一个Engine，其主要作用是执行业务逻辑；
  private
    FServletContainer: IServletContainer;
  public
    constructor Create(AServletContainer: IServletContainer); virtual;
    destructor Destroy; override;
    property ServletContainer: IServletContainer read FServletContainer;
  end;

  THost = class;
  TContext = class;
  TWrapper = class;

  { TEngine }

  TEngine = class(TContainer)
  private
    FName: string;
    FDefaultHost: string;
    FHostList: TFPHashObjectList;
  public
    constructor Create(AServletContainer: IServletContainer); override;
    destructor Destroy; override;
    property Name: string read FName;
    property DefaultHost: string read FDefaultHost;
    procedure AddHost(AHost: THost);
  end;

  { THost } //代表一个站点，也可以叫虚拟主机

  THost = class(TContainer)
  private
    FName: string;
    FContextList: TFPHashObjectList;
  public
    constructor Create(AServletContainer: IServletContainer); override;
    destructor Destroy; override;
    property Name: string read FName;
    procedure AddContext(AContext: TContext);
  end;

  { TContext } //代表一个应用

  TContext = class(TContainer)
  private
    FPath: string;
    FSource: string;
    FDisplayName: string;
    FWrapperList: TFPHashObjectList;
  public
    constructor Create(AServletContainer: IServletContainer); override;
    destructor Destroy; override;
    property Path: string read FPath;
    property Source: string read FSource;
    procedure AddWrapper(AWrapper: TWrapper);
    procedure ParseConfig(const AConfigFileName: string);

  end;

  { TWrapper }

  TWrapper = class(TContainer)
  private
    FName: string;
    FInterfaceCode: string;
    FServlet: IServlet;
    FMappings: TStrings;
    function GetServlet: IServlet;
  public
    constructor Create(AServletContainer: IServletContainer); override;
    destructor Destroy; override;
    property Name: string read FName;
    property InterfaceCode: string read FInterfaceCode;
    property Servlet: IServlet read GetServlet;
    property Mappings: TStrings read FMappings;
  public
    procedure DoInit(AConfig: IServletConfig);
    procedure DoService(ARequest: IServletRequest; AResponse: IServletResponse);
    procedure DoOver;
  end;

implementation

{ TContainer }

constructor TContainer.Create(AServletContainer: IServletContainer);
begin
  FServletContainer := AServletContainer;
end;

destructor TContainer.Destroy;
begin
  FServletContainer := nil;
  inherited Destroy;
end;

{ TContext }

constructor TContext.Create(AServletContainer: IServletContainer);
begin
  inherited Create(AServletContainer);
  FWrapperList := TFPHashObjectList.Create(True);
end;

destructor TContext.Destroy;
begin
  FWrapperList.Free;
  inherited Destroy;
end;

procedure TContext.AddWrapper(AWrapper: TWrapper);
begin
  FWrapperList.Add(AWrapper.Name, AWrapper);
end;

procedure TContext.ParseConfig(const AConfigFileName: string);
var
  ns: TCMDOMNodeStreamer;
  node, tempNode, subNode: TCMDOMNode;
  wrapper: TWrapper;
begin
  ns := TCMDOMNodeStreamer.Create(nil);
  try
    if ns.ReadXML(node, AConfigFileName) then
      begin
        if node.Name = 'cms-app' then
          begin
            tempNode := node.FirstChild;
            while Assigned(tempNode) do
              begin
                if tempNode.Name = 'display-name' then
                  begin
                    FDisplayName := tempNode.Text;
                  end
                else if tempNode.Name = 'servlet' then
                  begin
                    if tempNode.ChildCount >= 2 then
                      begin
                        subNode := tempNode.ChildNodes[0];
                        if subNode.Name = 'servlet-name' then
                          begin
                            wrapper := TWrapper.Create(Self.ServletContainer);
                            wrapper.FName := subNode.Text;
                            subNode := tempNode.ChildNodes[1];
                            if subNode.Name = 'servlet-interface' then
                              wrapper.FInterfaceCode := subNode.Text;
                            Self.AddWrapper(wrapper);
                          end;
                      end;
                  end
                else if tempNode.Name = 'servlet-mapping' then
                  begin
                    if tempNode.ChildCount >= 2 then
                      begin
                        subNode := tempNode.ChildNodes[0];
                        if subNode.Name = 'servlet-name' then
                          begin
                            wrapper := TWrapper(Self.FWrapperList.Find(subNode.Text));
                            if Assigned(wrapper) then
                              begin
                                subNode := tempNode.ChildNodes[1];
                                while Assigned(subNode) do
                                  begin
                                    if subNode.Name = 'url-pattern' then
                                      wrapper.Mappings.Add(subNode.Text);
                                    subNode := subNode.NextSibling;
                                  end;
                              end;
                          end;
                      end;
                  end;
                tempNode := tempNode.NextSibling;
              end;
          end;
        node.Free;
      end;
  finally
    ns.Free;
  end;
end;

{ THost }

constructor THost.Create(AServletContainer: IServletContainer);
begin
  inherited Create(AServletContainer);
  FContextList := TFPHashObjectList.Create(True);
end;

destructor THost.Destroy;
begin
  FContextList.Free;
  inherited Destroy;
end;

procedure THost.AddContext(AContext: TContext);
begin
  FContextList.Add(AContext.Path, AContext);
end;

{ TEngine }

constructor TEngine.Create(AServletContainer: IServletContainer);
begin
  inherited Create(AServletContainer);
  FHostList := TFPHashObjectList.Create(True);
end;

destructor TEngine.Destroy;
begin
  FHostList.Free;
  FServletContainer := nil;
  inherited Destroy;
end;

procedure TEngine.AddHost(AHost: THost);
begin
  FHostList.Add(AHost.Name, AHost);
end;

{ TService }

constructor TService.Create;
begin
  FConnectorList := TFPHashObjectList.Create(True);
end;

destructor TService.Destroy;
begin
  FConnectorList.Free;
  inherited Destroy;
end;

procedure TService.AddConnector(AConnector: TConnector);
begin
  FConnectorList.Add(AConnector.Protocol, AConnector);
end;

procedure TService.SetEngine(AEngine: TEngine);
begin
  FEngine := AEngine;
end;

function TService.Service(const AURL: string; ARequestParameters: ICMConstantParameterDataList; out TheResponseContent: ICMConstantParameterDataList): Boolean;
var
  dl: TCMParameterDataList;
begin
  Result := False;
  dl := TCMParameterDataList.Create;
  dl.SetString('test', 'haha');
  TheResponseContent := dl;

  //
  if Assigned(FEngine) then
    begin

    end;
end;

{ TServer }

constructor TServer.Create;
begin
  FServiceList := TFPHashObjectList.Create(True);
end;

procedure TServer.AddService(AService: TService);
begin
  FServiceList.Add(AService.Name, AService);
end;

//先简单实现
procedure TServer.ParseConfig(const AConfigFileName: string);
var
  ds: TCMDOMNodeStreamer;
  node, serviceNode, tempNode: TCMDOMNode;
  service: TService;
  connector: TConnector;
  engine: TEngine;
  host: THost;
  context: TContext;
begin
  ds := TCMDOMNodeStreamer.Create(nil);
  try
    if ds.ReadXML(node, AConfigFileName) then
      begin
        if node.Name = 'Server' then
          begin
            serviceNode := node.FirstChild;
            if Assigned(serviceNode) then
              begin
                if serviceNode.Name = 'Service' then
                  begin
                    service := TService.Create;
                    service.FName := serviceNode.GetAttribute('name');
                    Self.AddService(service);
                    //
                    tempNode := serviceNode.FirstChild;
                    if Assigned(tempNode) and (tempNode.Name = 'Connector') then
                      begin
                        connector := TConnector.Create;
                        connector.FProtocol := tempNode.GetAttribute('protocol');
                        service.AddConnector(connector);
                      end;
                    tempNode := tempNode.NextSibling;
                    if Assigned(tempNode) and (tempNode.Name = 'Engine') then
                      begin
                        engine := TEngine.Create(TServletContainer.Create);
                        service.SetEngine(engine);
                        tempNode := tempNode.FirstChild;
                        if Assigned(tempNode) and (tempNode.Name = 'Host') then
                          begin
                            host := THost.Create(engine.ServletContainer);
                            host.FName := tempNode.GetAttribute('name');
                            tempNode := tempNode.FirstChild;
                            while Assigned(tempNode) do
                              begin
                                if tempNode.Name = 'Context' then
                                  begin
                                    context := TContext.Create(engine.ServletContainer);
                                    context.FPath := tempNode.GetAttribute('path');
                                    context.FSource := tempNode.GetAttribute('source');
                                    host.AddContext(context);
                                  end;
                                tempNode := tempNode.NextSibling;
                              end;
                          end;
                      end;
                  end;
              end;
          end;
        node.Free;
      end;
  finally
    ds.Free;
  end;
end;

{ TProtocolHandler }

procedure TProtocolHandler.SetAcceptor(AAdapter: TAdapter);
begin
  FAdapter := AAdapter;
end;


{ TWrapper }

constructor TWrapper.Create(AServletContainer: IServletContainer);
begin
  inherited Create(AServletContainer);
  FMappings := TStringList.Create;
  FServlet := nil;
end;

destructor TWrapper.Destroy;
begin
  FMappings.Free;
  FServlet := nil;
  inherited Destroy;
end;

function TWrapper.GetServlet: IServlet;
begin
  Result := nil;
  if Assigned(FServlet) then
    Result := FServlet
  else if Assigned(ServletContainer) then
    begin
      FServlet := ServletContainer.GetServlet(Self.Name);
      if Assigned(FServlet) then
        Result := FServlet;
    end;
end;

procedure TWrapper.DoInit(AConfig: IServletConfig);
begin
  FServlet.Init(AConfig);
end;

procedure TWrapper.DoService(ARequest: IServletRequest; AResponse: IServletResponse);
begin
  FServlet.Service(ARequest, AResponse);
end;

procedure TWrapper.DoOver;
begin
  FServlet.Over;
end;

end.

