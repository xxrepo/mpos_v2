

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
