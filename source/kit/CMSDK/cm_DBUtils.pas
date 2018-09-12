unit cm_DBUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  db,
  cm_DB;

type

  { TSQLBatch }

  TSQLBatch = class(TComponent)
  private
    FSQL: TStrings;
    FStatement: ICMStatement;
  public
    constructor Create(AOwner: TComponent); override;
    property Statement: ICMStatement read FStatement write FStatement;
    destructor Destroy; override;
    function AddSQL(const ASQL: string): Integer; overload;
    function AddSQL(const AFmtSQL:string; const Args: array of const): Integer; overload;
    procedure AddSQLStrings(ASQLStrings: TStrings);
    procedure InsertSQL(AIndex: Integer; const ASQL: string); overload;
    procedure InsertSQL(AIndex: Integer; const AFmtSQL:string; const Args: array of const); overload;
    property SQL: TStrings read FSQL;
    procedure Clear;
    function Execute: Boolean;
  end;

  { TDBHelper }

  TDBHelper = class(TComponent)
  private
    FStatement: ICMStatement;
  protected
    procedure setStatement(AValue: ICMStatement); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Statement: ICMStatement read FStatement write setStatement;
    function Execute(const ASQL: string): Boolean; overload;
    function Execute(const AFmtSQL:string; const Args: array of const): Boolean; overload;
    function Query(const ASQL: string): TDataSet; overload;
    function Query(const AFmtSQL:string; const Args: array of const): TDataSet; overload;
    function GetString(const ASQL: string; AIndex: Integer=0): string; overload;
    function GetString(const AFmtSQL:string; const Args: array of const; AIndex: Integer=0): string; overload;
    function GetInteger(const ASQL: string; AIndex: Integer=0; ADefault: Integer=-1): Integer; overload;
    function GetInteger(const AFmtSQL:string; const Args: array of const; AIndex: Integer=0; ADefault: Integer=-1): Integer; overload;
    function GetCurrency(const ASQL: string; AIndex: Integer=0; ADefault: Currency=-1): Currency; overload;
    function GetCurrency(const AFmtSQL:string; const Args: array of const; AIndex: Integer=0; ADefault: Currency=-1): Currency; overload;
    function GetFloat(const ASQL: string; AIndex: Integer=0; ADefault: Double=-1): Double; overload;
    function GetFloat(const AFmtSQL:string; const Args: array of const; AIndex: Integer=0; ADefault: Double=-1): Double; overload;
    function GetStrings(const ASQL: string; AIndex: Integer=0): TStrings; overload;
    function GetStrings(const AFmtSQL:string; const Args: array of const; AIndex: Integer=0): TStrings; overload;
    function GetSQLBatch: TSQLBatch;
  end;

implementation

{ TSQLBatch }

constructor TSQLBatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FStatement := nil;
end;

destructor TSQLBatch.Destroy;
begin
  FSQL.Free;
  FStatement := nil;
  inherited Destroy;
end;

function TSQLBatch.AddSQL(const ASQL: string): Integer;
begin
  Result := FSQL.Add(ASQL);
end;

function TSQLBatch.AddSQL(const AFmtSQL: string; const Args: array of const): Integer;
begin
  Result := FSQL.Add(Format(AFmtSQL, Args));
end;

procedure TSQLBatch.AddSQLStrings(ASQLStrings: TStrings);
begin
  FSQL.AddStrings(ASQLStrings);
end;

procedure TSQLBatch.InsertSQL(AIndex: Integer; const ASQL: string);
begin
  FSQL.Insert(AIndex, ASQL);
end;

procedure TSQLBatch.InsertSQL(AIndex: Integer; const AFmtSQL: string; const Args: array of const);
begin
  FSQL.Insert(AIndex, Format(AFmtSQL, Args));
end;

procedure TSQLBatch.Clear;
begin
  FSQL.Clear;
end;

function TSQLBatch.Execute: Boolean;
begin
  Result := False;
  if Assigned(Statement) then
    Result := Statement.Execute(FSQL.Text);
end;

{ TDBHelper }

constructor TDBHelper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatement := nil;
end;

destructor TDBHelper.Destroy;
begin
  FStatement := nil;
  inherited Destroy;
end;

procedure TDBHelper.setStatement(AValue: ICMStatement);
begin
  FStatement := AValue;
end;

function TDBHelper.Execute(const ASQL: string): Boolean;
begin
  Result := False;
  if Assigned(Statement) then
    Result := Statement.Execute(ASQL);
end;

function TDBHelper.Execute(const AFmtSQL: string; const Args: array of const): Boolean;
begin
  Result := False;
  if Assigned(Statement) then
    Result := Statement.Execute(Format(AFmtSQL, Args));
end;

function TDBHelper.Query(const ASQL: string): TDataSet;
begin
  Result := nil;
  if Assigned(Statement) then
    Result := Statement.Query(ASQL);
end;

function TDBHelper.Query(const AFmtSQL: string; const Args: array of const): TDataSet;
begin
  Result := nil;
  if Assigned(Statement) then
    Result := Statement.Query(Format(AFmtSQL, Args));
end;

function TDBHelper.GetString(const ASQL: string; AIndex: Integer): string;
var
  ds: TDataSet;
begin
  Result := '';
  if Assigned(Statement) then
    begin
      ds := Statement.Query(ASQL);
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsString;
          ds.Free;
        end;
    end;
end;

function TDBHelper.GetString(const AFmtSQL: string; const Args: array of const; AIndex: Integer): string;
var
  ds: TDataSet;
begin
  Result := '';
  if Assigned(Statement) then
    begin
      ds := Statement.Query(Format(AFmtSQL, Args));
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsString;
          ds.Free;
        end;
    end;
end;

function TDBHelper.GetInteger(const ASQL: string; AIndex: Integer; ADefault: Integer): Integer;
var
  ds: TDataSet;
begin
  Result := ADefault;
  if Assigned(Statement) then
    begin
      ds := Statement.Query(ASQL);
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsInteger;
          ds.Free;
        end;
    end;
end;

function TDBHelper.GetInteger(const AFmtSQL: string; const Args: array of const; AIndex: Integer; ADefault: Integer): Integer;
var
  ds: TDataSet;
begin
  Result := ADefault;
  if Assigned(Statement) then
    begin
      ds := Statement.Query(Format(AFmtSQL, Args));
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsInteger;
          ds.Free;
        end;
    end;
end;

function TDBHelper.GetCurrency(const ASQL: string; AIndex: Integer; ADefault: Currency): Currency;
begin
  Result := GetCurrency(ASQL, [], AIndex, ADefault);
end;

function TDBHelper.GetCurrency(const AFmtSQL: string; const Args: array of const; AIndex: Integer; ADefault: Currency): Currency;
var
  ds: TDataSet;
begin
  Result := ADefault;
  if Assigned(Statement) then
    begin
      ds := Statement.Query(Format(AFmtSQL, Args));
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsCurrency;
          ds.Free;
        end;
    end;
end;

function TDBHelper.GetFloat(const ASQL: string; AIndex: Integer; ADefault: Double): Double;
begin
  Result := GetFloat(ASQL, [], AIndex, ADefault);
end;

function TDBHelper.GetFloat(const AFmtSQL: string; const Args: array of const; AIndex: Integer; ADefault: Double): Double;
var
  ds: TDataSet;
begin
  Result := ADefault;
  if Assigned(Statement) then
    begin
      ds := Statement.Query(Format(AFmtSQL, Args));
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsFloat;
          ds.Free;
        end;
    end;
end;

function TDBHelper.GetStrings(const ASQL: string; AIndex: Integer): TStrings;
begin
  Result := GetStrings(ASQL, [], AIndex);
end;

function TDBHelper.GetStrings(const AFmtSQL: string; const Args: array of const; AIndex: Integer): TStrings;
var
  ds: TDataSet;
begin
  Result := TStringList.Create;
  if Assigned(Statement) then
    begin
      ds := Statement.Query(Format(AFmtSQL, Args));
      if Assigned(ds) then
        begin
          while not ds.EOF do
            begin
              Result.Add(ds.Fields[AIndex].AsString);
              ds.Next;
            end;
          ds.Free;
        end;
    end;
end;

function TDBHelper.GetSQLBatch: TSQLBatch;
begin
  Result := TSQLBatch.Create(Self);
  Result.Statement := Statement;
end;

end.

