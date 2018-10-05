unit uDAO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_messager,
  uDB, uDBUtils,
  uApp;

type

  { IPOSDAO }

  IPOSDAO = interface(ICMBase)
    ['{015E79F6-CA47-4897-982E-749971E9FB25}']
    function SetDBHelper(AHelper: TPOSDBHelper): Boolean;
    function GetDBHelper: TPOSDBHelper;
  end;

  { TPOSDAO }

  TPOSDAO = class(TCMMessageableComponent, IPOSDAO)
  private
    FDBHelper: TPOSDBHelper;
  public
    constructor Create(AOwner: TComponent); override;
    function SetDBHelper(AHelper: TPOSDBHelper): Boolean;
    function GetDBHelper: TPOSDBHelper;
  end;

  TPOSDAOClass = class of TPOSDAO;

  { TPOSDAOFactory }

  TPOSDAOFactory = class(TCMMessageableComponent)
  private
    class var FFactory: TPOSDAOFactory;
    class var FStatement: IPOSStatement;
  private
    FDBHelper: TPOSDBHelper;
    function GetDBHelper: TPOSDBHelper;
  public
    constructor Create(AOwner: TComponent); override; //Please do not use this constructor
    class function GetInstance: TPOSDAOFactory;
    class procedure SetStatement(AStatement: IPOSStatement);
    function OutDAO(ADAOClass: TPOSDAOClass; APOSDAOIID: TGuid; out theInft): Boolean;
    function GetDAOObject(ADAOClass: TPOSDAOClass): TPOSDAO;
  end;

  function DataFmtDateTime(ADT: TDateTime): string;

const
  DataFmtSet : TFormatSettings = (
                  CurrencyFormat: 4;
                  NegCurrFormat: 4;
                  ThousandSeparator: ',';
                  DecimalSeparator: '.';
                  CurrencyDecimals: 4;
                  DateSeparator: '-';
                  TimeSeparator: ':';
                  ListSeparator: ',';
                  CurrencyString: 'amount:';
                  ShortDateFormat: 'yyyy-MM-dd';
                  LongDateFormat: 'yyyy-MM-dd';
                  TimeAMString: 'am';
                  TimePMString: 'pm';
                  ShortTimeFormat: 'hh:nn.ss';
                  LongTimeFormat: 'hh:nn:ss.zzz';
                  ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
                  LongMonthNames: ('一月','二月','三月','四月','五月','六月','七月','八月','九月','十月','十一月','十二月');
                  ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
                  LongDayNames:  ('星期日','星期一','星期二','星期三','星期四','星期五','星期六');
                  TwoDigitYearCenturyWindow: 50;
                );

implementation

function DataFmtDateTime(ADT: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', ADT, DataFmtSet);
end;

{ TPOSDAO }

constructor TPOSDAO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDBHelper := nil;
end;

function TPOSDAO.SetDBHelper(AHelper: TPOSDBHelper): Boolean;
begin
  Result := False;
  FDBHelper := AHelper;
  Result := Assigned(FDBHelper);
end;

function TPOSDAO.GetDBHelper: TPOSDBHelper;
begin
  Result := FDBHelper;
end;

{ TPOSDAOFactory }

constructor TPOSDAOFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if TPOSDAOFactory.FStatement = nil then
    if Assigned(uApp.InterfaceRegister) then
      uApp.InterfaceRegister.OutInterface(IPOSStatement, TPOSDAOFactory.FStatement);
  FDBHelper := TPOSDBHelper.Create(Self);
  FDBHelper.Statement := TPOSDAOFactory.FStatement;
end;

function TPOSDAOFactory.GetDBHelper: TPOSDBHelper;
begin
  if not Assigned(FDBHelper) then
    FDBHelper := TPOSDBHelper.Create(Self);
  if FDBHelper.Statement = nil then
    FDBHelper.Statement := TPOSDAOFactory.FStatement;
  Result := FDBHelper;
end;

class function TPOSDAOFactory.GetInstance: TPOSDAOFactory;
begin
  Result := nil;
  if not Assigned(TPOSDAOFactory.FFactory) then
    TPOSDAOFactory.FFactory := TPOSDAOFactory.Create(nil);
  Result := TPOSDAOFactory.FFactory;
end;

class procedure TPOSDAOFactory.SetStatement(AStatement: IPOSStatement);
begin
  TPOSDAOFactory.FStatement := AStatement;
end;

function TPOSDAOFactory.OutDAO(ADAOClass: TPOSDAOClass; APOSDAOIID: TGuid; out theInft): Boolean;
var
  obj: TPOSDAO;
  inft: IUnknown;
begin
  Result := False;
  Messager.Debug('OutDAO(%s.%s,*,*)...', [ADAOClass.UnitName, ADAOClass.ClassName]);
  obj := ADAOClass.Create(Self);
  if Supports(obj, IPOSDAO, inft) then
    begin
      if Supports(inft, APOSDAOIID, theInft) then
        begin
          Result := True;
          IPOSDAO(theInft).SetDBHelper(GetDBHelper);
        end;
    end;
end;

function TPOSDAOFactory.GetDAOObject(ADAOClass: TPOSDAOClass): TPOSDAO;
begin
  Result := nil;
  Messager.Debug('GetDAOObject(%s.%s)...', [ADAOClass.UnitName, ADAOClass.ClassName]);
  Result := ADAOClass.Create(Self);
  Result.SetDBHelper(GetDBHelper);
end;


initialization
  TPOSDAOFactory.FFactory := nil;
  TPOSDAOFactory.FStatement := nil;

finalization
  if Assigned(TPOSDAOFactory.FFactory) then
    TPOSDAOFactory.FFactory.Free;
  TPOSDAOFactory.FStatement := nil;

end.

