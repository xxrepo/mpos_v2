



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

procedure THandlerWrapper.BeforeHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse; var CanHandle: Boolean)
  ;
begin
  //
end;

procedure THandlerWrapper.DoHandle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse)
  ;
begin
  //
end;

//有 Wrapper 时交由其处理，否则经前置处理后有子处理器时交由处理器处理，无子处理器时调用 DoHandle()
procedure THandlerWrapper.Handle(ARequest: IJettyServletRequest; AResponse: IJettyServletResponse);
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
      {if Supports(FWrapper, IHandler, h) then
        h.Handle(ARequest, AResponse)
      else
        begin
          //for i:=0 to FWrapper.GetHandlers.Count-1 do
          //  FWrapper.GetHandlers[i].Handle(ATarget, ARequest, AResponse);
          //暂不考虑 Handler Collection 的情况
          h := FWrapper.GetHandler;
          if Assigned(h) then
            h.Handle(ARequest, AResponse);
        end;}
      FWrapper.Handle(ARequest, AResponse);
    end
  else
    begin
      canHandle := True;
      //
      if not ARequest.GetForwordMode then
        BeforeHandle(ARequest, AResponse, canHandle);
      //
      if CanHandle then
        begin
          h := GetHandler;
          if Assigned(h) then
            h.Handle(ARequest, AResponse)
          else
            DoHandle(ARequest, AResponse);
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




