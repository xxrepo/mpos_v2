

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

{ THandler }

constructor THandler.Create;
begin
  inherited Create;
  FServer := nil;
end;

procedure THandler.SetServer(AServer: IServer);
begin
  FServer := AServer;
end;

function THandler.GetServer: IServer;
begin
  Result := FServer;
end;

{ THandlerContainer }

constructor THandlerContainer.Create;
begin
  inherited Create;
  FHandlers := THandlerList.Create;
end;

destructor THandlerContainer.Destroy;
begin
  FHandlers.Free;
  inherited Destroy;
end;

function THandlerContainer.GetHandlers: THandlerList;
begin
  Result := FHandlers;
end;

{ THandlerWrapper }

constructor THandlerWrapper.Create;
begin
  inherited Create;
  FWrapper := nil;
end;

procedure THandlerWrapper.BeforeHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse; var CanHandle: Boolean);
begin
  //
end;

procedure THandlerWrapper.DoHandle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
begin
  //
end;

//有 Wrapper 时交由其处理，否则经前置处理后有子处理器时交由处理器处理，无子处理器时调用 DoHandle()
procedure THandlerWrapper.Handle(const ATarget: string; ARequest: IServletRequest; AResponse: IServletResponse);
var
  h: IHandler;
  canHandle: Boolean;
begin
  Messager.Debug('Handle()...');
  if not Self.IsRunning then
    begin
      Messager.Info('Handle() There is no start handle.');
      Exit;
    end;
  if Assigned(FWrapper) then
    begin
      if Supports(FWrapper, IHandler, h) then
        h.Handle(ATarget, ARequest, AResponse)
      else
        begin
          //for i:=0 to FWrapper.GetHandlers.Count-1 do
          //  FWrapper.GetHandlers[i].Handle(ATarget, ARequest, AResponse);
          //暂不考虑 Handler Collection 的情况
          h := FWrapper.GetHandler;
          if Assigned(h) then
            h.Handle(ATarget, ARequest, AResponse);
        end;
    end
  else
    begin
      canHandle := True;
      BeforeHandle(ATarget, ARequest, AResponse, canHandle);
      if CanHandle then
        begin
          h := GetHandler;
          if Assigned(h) then
            h.Handle(ATarget, ARequest, AResponse)
          else
            DoHandle(ATarget, ARequest, AResponse);
        end;
    end;
end;

procedure THandlerWrapper.SetHandler(AHandler: IHandler);
begin
  FHandlers.Clear;
  FHandlers.Add(AHandler);
end;

function THandlerWrapper.GetHandler: IHandler;
begin
  Result := nil;
  if FHandlers.Count > 0 then
    Result := FHandlers[0];
end;

procedure THandlerWrapper.InsertHandler(AWrapper: IHandlerWrapper);
begin
  FWrapper := AWrapper;
end;




