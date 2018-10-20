unit uDBUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  cm_DB, cm_DBUtils, uDB;

type

  { TPOSSQLBatch }

  TPOSSQLBatch = class(TSQLBatch)
  public
    function Execute(out ex: Exception): Boolean; overload;
  end;

  { TPOSDBHelper }

  TPOSDBHelper = class(TDBHelper)
  private
    FPOSStatement: IPOSStatement;
  protected
    procedure setStatement(AValue: ICMStatement); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(const ASQL: string; out ex: Exception): Boolean; overload;
    function Execute(const AFmtSQL:string; const Args: array of const; out ex: Exception): Boolean; overload;
    function Query(const ASQL: string; out ex: Exception): TDataSet; overload;
    function Query(const AFmtSQL:string; const Args: array of const; out ex: Exception): TDataSet; overload;
    function GetString(const ASQL: string; out ex: Exception; AIndex: Integer=0): string; overload;
    function GetString(const AFmtSQL:string; const Args: array of const; out ex: Exception; AIndex: Integer=0): string; overload;
    function GetInteger(const ASQL: string; out ex: Exception; AIndex: Integer=0; ADefault: Integer=-1): Integer; overload;
    function GetInteger(const AFmtSQL:string; const Args: array of const; out ex: Exception; AIndex: Integer=0; ADefault: Integer=-1): Integer; overload;
    function GetCurrency(const ASQL: string; out ex: Exception; AIndex: Integer=0; ADefault: Currency=-1): Currency; overload;
    function GetCurrency(const AFmtSQL:string; const Args: array of const; out ex: Exception; AIndex: Integer=0; ADefault: Currency=-1): Currency; overload;
    function GetFloat(const ASQL: string; out ex: Exception; AIndex: Integer=0; ADefault: Double=-1): Double; overload;
    function GetFloat(const AFmtSQL:string; const Args: array of const; out ex: Exception; AIndex: Integer=0; ADefault: Double=-1): Double; overload;
    function GetStrings(const ASQL: string; out ex: Exception; AIndex: Integer=0): TStrings; overload;
    function GetStrings(const AFmtSQL:string; const Args: array of const; out ex: Exception; AIndex: Integer=0): TStrings;
    function GetPOSSQLBatch: TPOSSQLBatch;
  end;

implementation

{ TPOSSQLBatch }

function TPOSSQLBatch.Execute(out ex: Exception): Boolean;
var
  sta: IPOSStatement;
begin
  Result := False;
  ex := nil;
  if Assigned(Statement) and Supports(Statement, IPOSStatement, sta) then
    Result := sta.Execute(SQL.Text, ex);
end;

{ TPOSDBHelper }

procedure TPOSDBHelper.setStatement(AValue: ICMStatement);
begin
  inherited setStatement(AValue);
  Supports(AValue, IPOSStatement, FPOSStatement);
end;

constructor TPOSDBHelper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPOSStatement := nil;
end;

destructor TPOSDBHelper.Destroy;
begin
  FPOSStatement := nil;
  inherited Destroy;
end;

function TPOSDBHelper.Execute(const ASQL: string; out ex: Exception): Boolean;
begin
  Result := False;
  ex := nil;
  if Assigned(FPOSStatement) then
    Result := FPOSStatement.Execute(ASQL, ex);
end;

function TPOSDBHelper.Execute(const AFmtSQL: string; const Args: array of const; out ex: Exception): Boolean;
begin
  Result := False;
  ex := nil;
  if Assigned(FPOSStatement) then
    Result := FPOSStatement.Execute(Format(AFmtSQL, Args), ex);
end;

function TPOSDBHelper.Query(const ASQL: string; out ex: Exception): TDataSet;
begin
  Result := nil;
  ex := nil;
  if Assigned(FPOSStatement) then
    Result := FPOSStatement.Query(ASQL, ex);
end;

function TPOSDBHelper.Query(const AFmtSQL: string; const Args: array of const; out ex: Exception): TDataSet;
begin
  Result := nil;
  ex := nil;
  if Assigned(FPOSStatement) then
    Result := FPOSStatement.Query(Format(AFmtSQL, Args), ex);
end;

function TPOSDBHelper.GetString(const ASQL: string; out ex: Exception; AIndex: Integer): string;
var
  ds: TDataSet;
begin
  Result := '';
  ex := nil;
  if Assigned(FPOSStatement) then
    begin
      ds := FPOSStatement.Query(ASQL, ex);
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsString;
          ds.Free;
        end;
    end;
end;

function TPOSDBHelper.GetString(const AFmtSQL: string; const Args: array of const; out ex: Exception; AIndex: Integer): string;
var
  ds: TDataSet;
begin
  Result := '';
  ex := nil;
  if Assigned(FPOSStatement) then
    begin
      ds := FPOSStatement.Query(Format(AFmtSQL, Args), ex);
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsString;
          ds.Free;
        end;
    end;
end;

function TPOSDBHelper.GetInteger(const ASQL: string; out ex: Exception; AIndex: Integer; ADefault: Integer): Integer;
var
  ds: TDataSet;
begin
  Result := ADefault;
  ex := nil;
  if Assigned(FPOSStatement) then
    begin
      ds := FPOSStatement.Query(ASQL, ex);
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsInteger;
          ds.Free;
        end;
    end;
end;

function TPOSDBHelper.GetInteger(const AFmtSQL: string; const Args: array of const; out ex: Exception; AIndex: Integer; ADefault: Integer): Integer;
var
  ds: TDataSet;
begin
  Result := ADefault;
  ex := nil;
  if Assigned(FPOSStatement) then
    begin
      ds := FPOSStatement.Query(Format(AFmtSQL, Args), ex);
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsInteger;
          ds.Free;
        end;
    end;
end;

function TPOSDBHelper.GetCurrency(const ASQL: string; out ex: Exception; AIndex: Integer; ADefault: Currency): Currency;
begin
  Result := GetCurrency(ASQL, [], ex, AIndex, ADefault);
end;

function TPOSDBHelper.GetCurrency(const AFmtSQL: string; const Args: array of const; out ex: Exception; AIndex: Integer; ADefault: Currency): Currency;
var
  ds: TDataSet;
begin
  Result := ADefault;
  ex := nil;
  if Assigned(FPOSStatement) then
    begin
      ds := FPOSStatement.Query(Format(AFmtSQL, Args), ex);
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsCurrency;
          ds.Free;
        end;
    end;
end;

function TPOSDBHelper.GetFloat(const ASQL: string; out ex: Exception; AIndex: Integer; ADefault: Double): Double;
begin
  Result := GetFloat(ASQL, [], ex, AIndex, ADefault);
end;

function TPOSDBHelper.GetFloat(const AFmtSQL: string; const Args: array of const; out ex: Exception; AIndex: Integer; ADefault: Double): Double;
var
  ds: TDataSet;
begin
  Result := ADefault;
  ex := nil;
  if Assigned(FPOSStatement) then
    begin
      ds := FPOSStatement.Query(Format(AFmtSQL, Args), ex);
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            Result := ds.Fields[AIndex].AsFloat;
          ds.Free;
        end;
    end;
end;

function TPOSDBHelper.GetStrings(const ASQL: string; out ex: Exception; AIndex: Integer): TStrings;
begin
  Result := GetStrings(ASQL, [], ex, AIndex);
end;

function TPOSDBHelper.GetStrings(const AFmtSQL: string; const Args: array of const; out ex: Exception; AIndex: Integer): TStrings;
var
  ds: TDataSet;
begin
  Result := TStringList.Create;
  ex := nil;
  if Assigned(FPOSStatement) then
    begin
      ds := FPOSStatement.Query(Format(AFmtSQL, Args), ex);
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

function TPOSDBHelper.GetPOSSQLBatch: TPOSSQLBatch;
begin
  Result := TPOSSQLBatch.Create(Self);
  Result.Statement := Statement;
end;


end.

