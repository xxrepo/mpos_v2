unit cm_netutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_sysutils;


type

  { TCMURL }

  TCMURL = class
  protected
    FProtocol: string;
    FUserName: string;
    FPassword: string;
    FHost: string;
    FPort: string;
    FPath: string;
    FDocument: string;
    FParams: string;
    FBookmark: string;
    function GetURL: string;
    procedure SetURL(const Value: String);
  public
    constructor Create(const AURL: string); virtual;
    function GetFullURL(AuthAndBookmark: Boolean=True): string;
    function GetFullPath: string;
    property URL: string read GetURL write SetURL;
    property Protocol: string read FProtocol write FProtocol;
    property Username: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Host: string read FHost write FHost;
    property Port: string read FPort write FPort;
    property Path: string read FPath write FPath;
    property Document: string read FDocument write FDocument;
    property Params: string read FParams write FParams;
    property Bookmark : string read FBookmark write FBookMark; //锚点（也称为“引用”）。
  end;

implementation

{ TCMURL }

constructor TCMURL.Create(const AURL: string);
begin
  SetURL(AURL);
end;

function TCMURL.GetURL: string;
begin
  Result := GetFullURL(False);
end;

procedure TCMURL.SetURL(const Value: String);
var
  iStr: string;
  p: Integer;
begin
  iStr := Value;
  FProtocol := '';
  FUserName := '';
  FPassword := '';
  FHost := '';
  FPort := '';
  FPath := '';
  FDocument := '';
  FParams := '';
  FBookmark := '';
  //
  FProtocol := LCutStr(iStr, '://');
  FUserName := LCutStr(iStr, '@');
  if FUserName <> '' then
    FPassword := RCutStr(FUserName, ':');
  //
  FBookmark := RCutStr(iStr, '#');
  FParams := RCutStr(iStr, '?');
  //剩下 host port path document
  p := PosR('/', iStr);
  if p > 0 then
    begin
      FDocument := Copy(iStr, p+1, MaxInt);
      iStr := Copy(iStr, 1, p-1);
    end;
  FPath := '/' + RCutStr(iStr, '/');
  FHost := LCutStr(iStr, ':');
  FPort := iStr;
end;

function TCMURL.GetFullURL(AuthAndBookmark: Boolean): string;
var
  oStr: String;
begin
  oStr := FProtocol + '://';
  if (FUserName <> '') and AuthAndBookmark then
    begin
      oStr := oStr + FUserName;
      if FPassword <> '' then
        oStr := oStr + ':' + FPassword;
      oStr := oStr + '@';
    end;
  oStr := oStr + FHost;
  if FPort <> '' then
    oStr := oStr + ':' + FPort;
  oStr := oStr + FPath;
  if FParams <> '' then
    oStr := oStr + '?' + FParams;
  if (FBookmark <> '') and AuthAndBookmark then
    oStr := oStr + '#' + FBookmark;
  Result := oStr;
end;

function TCMURL.GetFullPath: string;
begin
  Result := FPath + '/' + FDocument;
end;



end.

