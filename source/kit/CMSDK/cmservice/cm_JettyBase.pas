unit cm_JettyBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_messager, cm_parameter, cm_ParameterUtils,
  cm_servlet, cm_servletutils, cm_jetty;

type

  { TLifeCycle }

  TLifeCycle = class(TCMMessageable, ILifeCycle)
  private
    FIsRunning: Boolean;
    FIsStopped: Boolean;
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
  public
    constructor Create;
    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;
    function IsStopped: Boolean;
  end;

  { TConnector }

  TConnector = class(TLifeCycle, IConnector)
  private
    FProtocol: string;
    FPort: Word;
  public
    constructor Create(const AProtocol: string; APort: Word=80);
    function GetProtocol: string;
    function GetPort: Word;
  end;

  (***************************************** Holder ***********************************************)

  { THolder }

  THolder = class(TLifeCycle, IHolder)
  private
    FName: string;
    FInitParameters: ICMParameterDataList;
  public
    constructor Create;
    property InitParameters: ICMParameterDataList read FInitParameters;
    procedure SetName(const AName: string);
    function GetName: string;
    function GetInitParameters: ICMConstantParameterDataList;
  end;

  { TFilterHolder }

  TFilterHolder = class(THolder, IFilterHolder)
  private
    FFilter: IFilter;
  public
    constructor Create;
    procedure SetFilter(AFilter: IFilter);
    function GetFilter: IFilter;
  end;

  { TListenerHolder }

  TListenerHolder = class(THolder, IListenerHolder)
  private
    FListener: IListener;
  public
    constructor Create;
    procedure SetListener(AListener: IListener);
    function GetListener: IListener;
  end;

  { TServletHolder }

  TServletHolder = class(THolder, IServletHolder)
  private
    FServletContext: IServletContext;
    FServlet: IServlet;
    FURLPatterns: TStrings;
    FInitialized: Boolean;
    FServletConfig: IServletConfig;
    procedure Init; //TODO 后继改进
  public
    constructor Create(AServletContext: IServletContext);
    procedure SetServlet(AServlet: IServlet);
    function GetServlet: IServlet;
    procedure AddURLPattern(const AURLPattern: string);
    function GetURLPatterns: TStrings;
    function Initialized: Boolean;
    function GetServletConfig: IServletConfig;
  end;

implementation


{ TLifeCycle }

constructor TLifeCycle.Create;
begin
  inherited Create;
  FIsRunning := False;
  FIsStopped := False;
end;

procedure TLifeCycle.DoStart;
begin
  //
end;

procedure TLifeCycle.DoStop;
begin
  //
end;

procedure TLifeCycle.Start;
begin
  if not FIsRunning then
    begin
      DoStart;
      FIsRunning := True;
      FIsStopped := False;
    end;
end;

procedure TLifeCycle.Stop;
begin
  if not FIsStopped then
    begin
      DoStop;
      FIsRunning := False;
      FIsStopped := True;
    end;
end;

function TLifeCycle.IsRunning: Boolean;
begin
  Result := FIsRunning;
end;

function TLifeCycle.IsStopped: Boolean;
begin
  Result := FIsStopped;
end;

{ TConnector }

constructor TConnector.Create(const AProtocol: string; APort: Word);
begin
  inherited Create;
  FProtocol := AProtocol;
  FPort := APort;
end;

function TConnector.GetProtocol: string;
begin
  Result := FProtocol;
end;

function TConnector.GetPort: Word;
begin
  Result := FPort;
end;

{ THolder }

constructor THolder.Create;
begin
  inherited Create;
  FName := '';
  FInitParameters := TCMParameterDataList.Create;
end;

procedure THolder.SetName(const AName: string);
begin
  FName := AName;
end;

function THolder.GetName: string;
begin
  Result := FName;
end;

function THolder.GetInitParameters: ICMConstantParameterDataList;
begin
  Result := FInitParameters;
end;

{ TFilterHolder }

constructor TFilterHolder.Create;
begin
  inherited Create;
  FFilter := nil;
end;

procedure TFilterHolder.SetFilter(AFilter: IFilter);
begin
  FFilter := AFilter;
end;

function TFilterHolder.GetFilter: IFilter;
begin
  Result := FFilter;
end;

{ TListenerHolder }

constructor TListenerHolder.Create;
begin
  inherited Create;
  FListener := nil;
end;

procedure TListenerHolder.SetListener(AListener: IListener);
begin
  FListener := AListener;
end;

function TListenerHolder.GetListener: IListener;
begin
  Result := FListener;
end;

{ TServletHolder }

constructor TServletHolder.Create(AServletContext: IServletContext);
begin
  inherited Create;
  FServletContext := AServletContext;
  FServlet := nil;
  FURLPatterns := TStringList.Create;
  FInitialized := False;
  FServletConfig := nil;
end;

procedure TServletHolder.SetServlet(AServlet: IServlet);
begin
  FServlet := AServlet;
end;

function TServletHolder.GetServlet: IServlet;
begin
  Result := nil;
  Init;
  Result := FServlet;
end;

procedure TServletHolder.AddURLPattern(const AURLPattern: string);
begin
  FURLPatterns.Add(AURLPattern);
end;

function TServletHolder.GetURLPatterns: TStrings;
begin
  Result := FURLPatterns;
end;

procedure TServletHolder.Init;
begin
  if not Assigned(FServletConfig) then
    FServletConfig := TServletConfig.Create(Self.GetName, FServletContext);
  if (not FInitialized) and Assigned(FServlet) then
    begin
      FServlet.Init(FServletConfig);
      FInitialized := True;
    end;
end;

function TServletHolder.Initialized: Boolean;
begin
  Result := FInitialized;
end;

function TServletHolder.GetServletConfig: IServletConfig;
begin
  Result := FServletConfig;
end;

end.

