unit cm_InterfaceRegisterImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  Generics.Collections, Generics.Defaults,
  cm_interfaces, cm_messager, cm_freegenerics,
  cm_InterfaceRegister;

type

  TCMInterfaceCell = class(TPersistent)
  private
    FIID: TGUID;
    FIntf: IUnknown;    //接口实例
    FCode: string;      //标记代码
    FDescription: string;
  public
    constructor Create(AIID: TGUID; AIntf: IUnknown; const ACode, ADescription: string); overload;
    function Equals(Obj: TObject): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    function ToString: ansistring; override;
    property IID: TGUID read FIID write FIID;
    property Intf: IUnknown read FIntf write FIntf;
    property Code: string read FCode write FCode;
    property Description: string read FDescription write FDescription;
  end;

  TCMInterfaceList = class(TObjectList<TCMInterfaceCell>)
  private
    function theComparison(constref Left, Right: TCMInterfaceCell): Integer;
  protected
    function DoGetEnumerator: TEnumerator<TCMInterfaceCell>; override;
  public
    type TCMInterfaceEnumerator = class(TCustomListEnumerator<TCMInterfaceCell>);
    function GetEnumerator: TCMInterfaceEnumerator; reintroduce;
  public
    constructor Create;
    function Add(constref AValue: TCMInterfaceCell): SizeInt; reintroduce;
  end;

  TCMInterfaceIterator = class(TCMMessageableComponent, ICMInterfaceIterator)
  private
    FIndex: Integer;
    FList: TCMInterfaceList;
  protected
    function Next: TCMInterfaceCell; overload;
  public
    constructor Create(AOwner: TComponent; AList: TCMInterfaceList); overload;
    destructor Destroy; override;
    function HasNext: Boolean;
    function Next(out theDescription: string; out theIID: TGUID; out theIntf; out theCode: string): Boolean; overload;
    function Next(out theIID: TGUID; out theIntf; out theCode: string): Boolean; overload;
    function Next(out theIID: TGUID; out theIntf): Boolean; overload;
  end;

  { TCMInterfaceRegister }

  TCMInterfaceRegister = class(TCMMessageableComponent, ICMInterfaceRegister)
  private
    FList: TCMInterfaceList;
    FLock: TRTLCriticalSection;
    FListenerList: TCMInterfaceRegisterListener;
    function generateLogStr(const AIID: TGUID; const ACode: string): string;
  protected
    function  LockList: TCMInterfaceList;
    procedure UnlockList;
    function PutInterface(const ACell: TCMInterfaceCell): Integer; overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PutInterface(const ADescription: string; const AIID: TGUID; const AIntf: IUnknown; const ACode: string=''): Integer; overload;
    function PutInterface(const AIID: TGUID; const AIntf: IUnknown; const ACode: string=''): Integer; overload;
    function OutInterface(const AIID: TGUID; out theIntf; const ACode: string=''): Boolean;
    function CutInterface(const AIID: TGUID; const ACode: string=''): Integer;
    function Iterator: ICMInterfaceIterator;
    //----------------------------------------------------------------------------------------------
    function IsExist(const AIID: TGUID; const ACode: string=''): Boolean;
    procedure AddInterfaceRegisterListener(l: ICMInterfaceRegisterListener);
    procedure RemoveInterfaceRegisterListener(l: ICMInterfaceRegisterListener);
    function GetInterfaceRegisterListeners: TCMInterfaceRegisterListener;
  end;

  { TCMInterfaceRegisterEvent }

  TCMInterfaceRegisterEvent = class(TCMEvent, ICMInterfaceRegisterEvent)
  private
    FDescription: string;
    FIID: TGUID;
    FIntf: IUnknown;
    FCode: string;
    FIsAble: Boolean;
  public
    constructor Create(ASource: TObject; const ADescription: string; AIID: TGUID; AIntf: IUnknown; const ACode: string); reintroduce;
    function GetDescription: string;
    function GetIID: TGUID;
    function GetIntf: IUnknown;
    function GetCode: string;
    function IsAble: Boolean;
    procedure SetAble(b: Boolean);
  end;

implementation

{TCMInterfaceCell}

constructor TCMInterfaceCell.Create(AIID: TGUID; AIntf: IUnknown; const ACode, ADescription: string);
begin
  inherited Create;
  FIID := AIID;
  FIntf := AIntf;
  FCode := ACode;
  FDescription := ADescription;
end;

function TCMInterfaceCell.Equals(Obj: TObject): Boolean;
begin
  Result := False;
  if Assigned(Obj) and (Obj is TCMInterfaceCell) then
    begin
      //if Assigned(FIntf) and Assigned(TCMInterfaceCell(Obj).FIntf) then
      //  begin
      //    if (TCMInterfaceCell(Obj).Code = FCode) and IsEqualGUID(TCMInterfaceCell(Obj).FIID, FIID) then
      //      begin
      //        Result := Supports(TCMInterfaceCell(Obj).FIntf, FIID);
      //      end;
      //  end;
      //
      Result := (TCMInterfaceCell(Obj).Code = FCode) and IsEqualGUID(TCMInterfaceCell(Obj).FIID, FIID);
    end;
end;

procedure TCMInterfaceCell.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TCMInterfaceCell) then
    begin
      Self.IID := TCMInterfaceCell(Source).IID;
      Self.Intf := TCMInterfaceCell(Source).Intf;
      Self.Code := TCMInterfaceCell(Source).Code;
      Self.Description := TCMInterfaceCell(Source).Description;
    end;
end;

function TCMInterfaceCell.ToString: ansistring;
var
  base: ICMBase;
begin
  Result := inherited;
  try
    Result := BoolToStr(Self.Description='', '[GUID:', Self.Description+':[GUID:') + GUIDToString(Self.IID) + BoolToStr(Self.Code='', '', '; Code:' + Self.Code);
    if Assigned(Self.Intf) then
      if Self.Intf.QueryInterface(ICMBase, base) = S_OK then
        Result := Result + '; Impl:' + base.GetImplementorName;
    Result := Result + ']';
  except
  end;
end;

{TCMInterfaceList}

constructor TCMInterfaceList.Create;
begin
  inherited Create(TComparer<TCMInterfaceCell>.Construct(theComparison));
  Self.OwnsObjects := False;
end;

function TCMInterfaceList.DoGetEnumerator: TEnumerator<TCMInterfaceCell>;
begin
  Result := GetEnumerator;
end;

function TCMInterfaceList.GetEnumerator: TCMInterfaceEnumerator;
begin
  Result := TCMInterfaceEnumerator.Create(Self);
end;

function TCMInterfaceList.theComparison(constref Left, Right: TCMInterfaceCell): Integer;
begin
  if Left.Equals(Right) then
    Result := 0
  else
    Result := TComparer<TCMInterfaceCell>.Default.Compare(Left, Right);
end;

function TCMInterfaceList.Add(constref AValue: TCMInterfaceCell): SizeInt;
begin
  Result := -1;
  if not Self.Contains(AValue) then
    Result := inherited Add(AValue);
end;

{TCMInterfaceIterator}

constructor TCMInterfaceIterator.Create(AOwner: TComponent; AList: TCMInterfaceList);
begin
  inherited Create(AOwner);
  FIndex := -1;
  FList := AList;
  FList.OwnsObjects := True;
end;

destructor TCMInterfaceIterator.Destroy;
begin
  if Assigned(FList) then
    FList.Free;
  inherited Destroy;
end;

function TCMInterfaceIterator.Next: TCMInterfaceCell;
begin
  Result := nil;
  FIndex := FIndex + 1;
  Result := FList.Items[FIndex];
end;

function TCMInterfaceIterator.HasNext: Boolean;
begin
  Result := Assigned(FList) and (FList.Count > 0) and (FIndex + 1 < FList.Count);
end;

function TCMInterfaceIterator.Next(out theDescription: string; out theIID: TGUID; out theIntf; out theCode: string): Boolean;
var
  theCell: TCMInterfaceCell;
begin
  Result := False;
  theCell := Self.Next;
  if Assigned(theCell) then
    begin
      theIID := theCell.IID;
      if Assigned(theCell.Intf) then
        Result := theCell.Intf.QueryInterface(theIID, theIID) = S_OK;
      theCode := theCell.Code;
      theDescription := theCell.Description;
    end;
end;

function TCMInterfaceIterator.Next(out theIID: TGUID; out theIntf; out theCode: string): Boolean;
var
  aDescription: string;
begin
  Result := Self.Next(aDescription, theIID, theIntf, theCode);
end;

function TCMInterfaceIterator.Next(out theIID: TGUID; out theIntf): Boolean;
var
  aDescription, aCode: string;
begin
  Result := Self.Next(aDescription, theIID, theIntf, aCode);
end;

{TCMInterfaceRegister}

constructor TCMInterfaceRegister.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitCriticalSection(FLock);
  Flist := TCMInterfaceList.Create;
  FListenerList := TCMInterfaceRegisterListener.Create;
end;

destructor TCMInterfaceRegister.Destroy;
begin
  LockList;
  try
    Flist.Free;
    FListenerList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DoneCriticalSection(FLock);
  end;
end;

function TCMInterfaceRegister.LockList: TCMInterfaceList;
begin
  Result := FList;
  System.EnterCriticalSection(FLock);
end;

procedure TCMInterfaceRegister.UnlockList;
begin
  System.LeaveCriticalSection(FLock);
end;

function TCMInterfaceRegister.generateLogStr(const AIID: TGUID; const ACode: string): string;
begin
  Result := Format('[GUID:%s%s]', [GUIDToString(AIID), BoolToStr(ACode='', '', '; Code:' + ACode)]);
end;

function TCMInterfaceRegister.PutInterface(const ACell: TCMInterfaceCell): Integer;
var
  str: string;
  er: TFGInterfaceListEnumerator<ICMInterfaceRegisterListener>;
  ire: ICMInterfaceRegisterEvent;
  ertn: Boolean;
begin
  Result := -1;
  str := ACell.ToString;
  try
    LockList;
    try
      if FListenerList.Count > 0 then
        begin
          ertn := True;
          er := FListenerList.GetEnumerator;
          while er.MoveNext do
            begin
              ire := TCMInterfaceRegisterEvent.Create(Self, ACell.Description, ACell.IID, ACell.Intf, ACell.Code);
              er.GetCurrent.Putting(ire);
              ertn := ire.IsAble and ertn;
            end;
          if not ertn then
            begin
              Messager.Warning('Put failure(event Event handling is not allowed).' + str);
              Exit;
            end;
        end;
      //
      Result := Flist.Add(ACell);
    finally
      UnlockList;
    end;
    if Result >= 0 then
      begin
        Messager.Custom('Put << ' + str);
      end
    else
      begin
        Messager.Error('Interface already exists.' + str);
      end;
  except
    on e: Exception do
      Messager.Error('An error occurred while putting.' + str, e);
  end;
end;

function TCMInterfaceRegister.PutInterface(const ADescription: string; const AIID: TGUID; const AIntf: IUnknown; const ACode: string=''): Integer;
var
  aCell: TCMInterfaceCell;
begin
  Result := -1;
  aCell := TCMInterfaceCell.Create(AIID, AIntf, ACode, ADescription);
  Result := Self.PutInterface(aCell);
end;

function TCMInterfaceRegister.PutInterface(const AIID: TGUID; const AIntf: IUnknown; const ACode: string=''): Integer;
begin
  Result := -1;
  Result := Self.PutInterface('', AIID, AIntf, ACode);
end;

function TCMInterfaceRegister.OutInterface(const AIID: TGUID; out theIntf; const ACode: string=''): Boolean;
var
  str: string;
  i: Integer;
  aCell: TCMInterfaceCell;
  er: TFGInterfaceListEnumerator<ICMInterfaceRegisterListener>;
  ire: ICMInterfaceRegisterEvent;
  ertn: Boolean;
begin
  Result := False;
  IUnknown(theIntf) := nil;
  str := Self.generateLogStr(AIID, ACode);
  try
    LockList;
    try
      for i:=FList.Count-1 downto 0 do
        begin
          aCell := FList[i];
          if (aCell.Code = ACode) and IsEqualGUID(ACell.IID, AIID) then
            begin
              if Assigned(aCell.Intf) then
                begin
                  Result := aCell.Intf.QueryInterface(AIID, theIntf) = S_OK;
                  if Result then
                    begin
                      if FListenerList.Count > 0 then
                        begin
                          ertn := True;
                          er := FListenerList.GetEnumerator;
                          while er.MoveNext do
                            begin
                              ire := TCMInterfaceRegisterEvent.Create(Self, ACell.Description, ACell.IID, ACell.Intf, ACell.Code);
                              er.GetCurrent.Outting(ire);
                              ertn := ire.IsAble and ertn;
                            end;
                          if not ertn then
                            begin
                              Messager.Warning('Out failure(event Event handling is not allowed).' + str);
                              Exit;
                            end;
                        end;
                      Messager.Custom('Out >> ' + aCell.ToString);
                    end
                  else
                    Messager.Warning('Out failure. Not support ' + aCell.ToString);
                end
              else
                 Messager.Error('Out failure. Not assigned.' + str);
              Exit;
            end;
        end;
    finally
      UnlockList;
    end;
  except
    on e: Exception do
      begin
        Messager.Error('An error occurred while outputting. %s %s ' + str, e);
        Exit;
      end;
  end;
  Messager.Error('Out failure(Not found).' + str);
end;

function TCMInterfaceRegister.CutInterface(const AIID: TGUID; const ACode: string=''): Integer;
var
  aCell: TCMInterfaceCell;
  er: TFGInterfaceListEnumerator<ICMInterfaceRegisterListener>;
  ire: ICMInterfaceRegisterEvent;
  ertn: Boolean;
begin
  Result := -1;
  aCell := TCMInterfaceCell.Create(AIID, nil, ACode, '');
  LockList;
  try
    if (FListenerList.Count > 0) and (FList.IndexOf(aCell) > 0) then
      begin
        ertn := True;
        er := FListenerList.GetEnumerator;
        while er.MoveNext do
          begin
            ire := TCMInterfaceRegisterEvent.Create(Self, ACell.Description, ACell.IID, ACell.Intf, ACell.Code);
            er.GetCurrent.Outting(ire);
            ertn := ire.IsAble and ertn;
          end;
        if not ertn then
          begin
            Messager.Warning('Cut failure(event Event handling is not allowed).' + aCell.ToString);
            Exit;
          end;
      end;
    Result := FList.Remove(aCell);
    Messager.Custom('Cut >> ' + aCell.ToString)
  finally
    UnlockList;
    if Assigned(aCell) then
      aCell.Free;
  end;
end;

function TCMInterfaceRegister.Iterator: ICMInterfaceIterator;
var
  cloneList: TCMInterfaceList;
  i: Integer;
  cloneCell: TCMInterfaceCell;
begin
  cloneList := TCMInterfaceList.Create;
  LockList;
  try
    for i:=FList.Count-1 downto 0 do
      begin
        cloneCell := TCMInterfaceCell.Create;
        cloneCell.Assign(FList[i]);
        cloneList.Add(cloneCell);
      end;
  finally
    UnlockList;
  end;
  Result := TCMInterfaceIterator.Create(Self, cloneList);
end;

function TCMInterfaceRegister.IsExist(const AIID: TGUID; const ACode: string): Boolean;
var
  str: string;
  i: Integer;
  aCell: TCMInterfaceCell;
begin
  Result := False;
  str := Self.generateLogStr(AIID, ACode);
  try
    LockList;
    try
      for i:=FList.Count-1 downto 0 do
        begin
          aCell := FList[i];
          if (aCell.Code = ACode) and IsEqualGUID(ACell.IID, AIID) then
            begin
              Messager.Custom('Exists >> ' + aCell.ToString);
              Result := True;
              Exit;
            end;
        end;
    finally
      UnlockList;
    end;
  except
    on e: Exception do
      begin
        Messager.Error('An error occurred while judgment exists. %s %s ' + str, e);
        Exit;
      end;
  end;
  Messager.Custom('Not exists (Not found).' + str);
end;

procedure TCMInterfaceRegister.AddInterfaceRegisterListener(l: ICMInterfaceRegisterListener);
begin
  LockList;
  try
    FListenerList.Add(l);
  finally
    UnlockList;
  end;
end;

procedure TCMInterfaceRegister.RemoveInterfaceRegisterListener(l: ICMInterfaceRegisterListener);
begin
  LockList;
  try
    FListenerList.Remove(l);
  finally
    UnlockList;
  end;
end;

function TCMInterfaceRegister.GetInterfaceRegisterListeners: TCMInterfaceRegisterListener;
begin
  LockList;
  try
    Result := FListenerList.Clone;
  finally
    UnlockList;
  end;
end;

{ TCMInterfaceRegisterEvent }

constructor TCMInterfaceRegisterEvent.Create(ASource: TObject; const ADescription: string; AIID: TGUID; AIntf: IUnknown; const ACode: string);
begin
  inherited Create(ASource);
  FDescription := ADescription;
  FIID := AIID;
  FIntf := AIntf;
  FCode := ACode;
  FIsAble := True;
end;

function TCMInterfaceRegisterEvent.GetDescription: string;
begin
  Result := FDescription;
end;

function TCMInterfaceRegisterEvent.GetIID: TGUID;
begin
  Result := FIID;
end;

function TCMInterfaceRegisterEvent.GetIntf: IUnknown;
begin
  Result := FIntf;
end;

function TCMInterfaceRegisterEvent.GetCode: string;
begin
  Result := FCode;
end;

function TCMInterfaceRegisterEvent.IsAble: Boolean;
begin
  Result := FIsAble;
end;

procedure TCMInterfaceRegisterEvent.SetAble(b: Boolean);
begin
  FIsAble := b;
end;



end.

