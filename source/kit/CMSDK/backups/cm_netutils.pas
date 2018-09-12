unit cm_netutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


type

  { TIdURI }

  TIdURI = class
  protected
    FDocument: string;
    FProtocol: string;
    FURI: String;
    FPort: string;
    Fpath: string;
    FHost: string;
    FBookmark: string;
    FUserName: string;
    FPassword: string;
    FParams: string;
    FIPVersion: Integer;
    function GetURI: string;
    procedure SetURI(const Value: String);
  public
    constructor Create(const AURI: string); virtual;
    function GetFullURI(AuthAndBookmark: Boolean=True): string;
    function GetPathAndParams: String;
    class procedure NormalizePath(var APath: string);
    property Bookmark : string read FBookmark write FBookMark;
    property Document: string read FDocument write FDocument;
    property Host: string read FHost write FHost;
    property Password: string read FPassword write FPassword;
    property Path: string read FPath write FPath;
    property Params: string read FParams write FParams;
    property Port: string read FPort write FPort;
    property Protocol: string read FProtocol write FProtocol;
    property URI: string read GetURI write SetURI;
    property Username: string read FUserName write FUserName;
    property IPVersion : Integer read FIPVersion write FIPVersion;
  end;

  function Fetch(var AInput: string; const ADelim: string): string;

implementation

function Fetch(var AInput: string; const ADelim: string): string;
var
  LPos: Integer;
begin
  if ADelim = #0 then
    LPos := Pos(ADelim, AInput) // AnsiPos does not work with #0
  else
    LPos := AnsiPos(ADelim, AInput);
  if LPos = 0 then
    begin
      Result := AInput;
      AInput := '';
    end
  else
    begin
      Result := Copy(AInput, 1, LPos - 1);
      AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
    end;
end;

// Find a token given a direction (>= 0 from start; < 0 from end)
//如为正，不到起点不返回位置
function RPos(const ASub, AIn: String; AStart: Integer = -1): Integer;
var
  i: Integer;
  LStartPos: Integer;
  LTokenLen: Integer;
begin
  Result := 0;
  LTokenLen := Length(ASub);
  // Get starting position
  if AStart < 0 then begin
    AStart := Length(AIn);
  end;
  if AStart < (Length(AIn) - LTokenLen + 1) then begin
    LStartPos := AStart;
  end else begin
    LStartPos := (Length(AIn) - LTokenLen + 1);
  end;
  // Search for the string
  for i := LStartPos downto 1 do
    begin
      // TODO: remove the need for Copy()
      if SameText(Copy(AIn, i, LTokenLen), ASub) then begin
        Result := i;
        Break;
      end;
    end;
end;

function TIdURI.GetURI: string;
begin
  FURI := GetFullURI;
  // Result must contain only the proto://host/path/document
  // If you need the full URI then you have to call GetFullURI
  Result := GetFullURI(False);
end;

procedure TIdURI.SetURI(const Value: String);
var
  LBuffer: string;
  LTokenPos: Integer;
  LURI: string;
begin
  FURI := Value;
  NormalizePath(FURI);
  LURI := FURI;
  FHost := '';
  FProtocol := '';
  FPath := '';
  FDocument := '';
  FPort := '';
  FBookmark := '';
  FUsername := '';
  FPassword := '';
  FParams := '';  {Do not localise}  //Peter Mee
  FIPVersion := 4;
  //
  LTokenPos := AnsiPos('://', LURI);
  if LTokenPos > 0 then begin
    // absolute URI
    // What to do when data don't match configuration ??
    // Get the protocol
    FProtocol := Copy(LURI, 1, LTokenPos  - 1);
    Delete(LURI, 1, LTokenPos + 2);
    // separate the path from the parameters
    LTokenPos := AnsiPos('?', LURI);
    // RLebeau: this is BAD! It messes up JSP and similar URLs that use '=' characters in the document
    {if LTokenPos = 0 then begin
      LTokenPos := IndyPos('=', LURI);
    end;}
    if LTokenPos > 0 then begin
      FParams := Copy(LURI, LTokenPos + 1, MaxInt);
      LURI := Copy(LURI, 1, LTokenPos - 1);
      // separate the bookmark from the parameters
      LTokenPos := AnsiPos('#', FParams);
      if LTokenPos > 0 then begin
        FBookmark := FParams;
        FParams := Fetch(FBookmark, '#');
      end;
    end else begin
      // separate the path from the bookmark
      LTokenPos := AnsiPos('#', LURI);
      if LTokenPos > 0 then begin
        FBookmark := Copy(LURI, LTokenPos + 1, MaxInt);
        LURI := Copy(LURI, 1, LTokenPos - 1);
      end;
    end;
    // Get the user name, password, host and the port number
    LBuffer := Fetch(LURI, '/');
    // Get username and password
    LTokenPos := RPos('@', LBuffer);
    if LTokenPos > 0 then begin
      FPassword := Copy(LBuffer, 1, LTokenPos  - 1);
      Delete(LBuffer, 1, LTokenPos);
      FUserName := Fetch(FPassword, ':');
      // Ignore cases where there is only password (http://:password@host/pat/doc)
      if Length(FUserName) = 0 then begin
        FPassword := '';
      end;
    end;
    // Get the host and the port number
    if (AnsiPos('[', LBuffer) > 0) and (AnsiPos(']', LBuffer) > AnsiPos('[', LBuffer)) then begin
      //This is for IPv6 Hosts
      FHost := Fetch(LBuffer, ']');
      Fetch(FHost, '[');
      Fetch(LBuffer, ':');
      FIPVersion := 6;
    end else begin
      FHost := Fetch(LBuffer, ':');
    end;
    FPort := LBuffer;
    // Get the path
    LTokenPos := RPos('/', LURI, -1);
    if LTokenPos > 0 then begin
      FPath := '/' + Copy(LURI, 1, LTokenPos);
      Delete(LURI, 1, LTokenPos);
    end else begin
      FPath := '/';
    end;
  end else begin
    // received an absolute path, not an URI
    LTokenPos := AnsiPos('?', LURI);
    // RLebeau: this is BAD! It messes up JSP and similar URLs that use '=' characters in the document
    {if LTokenPos = 0 then begin
      LTokenPos := IndyPos('=', LURI);
    end;}
    if LTokenPos > 0 then begin // The case when there is parameters after the document name
      FParams := Copy(LURI, LTokenPos + 1, MaxInt);
      LURI := Copy(LURI, 1, LTokenPos - 1);
      // separate the bookmark from the parameters
      LTokenPos := AnsiPos('#', FParams);
      if LTokenPos > 0 then begin
        FBookmark := FParams;
        FParams := Fetch(FBookmark, '#');
      end;
    end else begin
      // separate the bookmark from the path
      LTokenPos := AnsiPos('#', LURI);
      if LTokenPos > 0 then begin // The case when there is a bookmark after the document name
        FBookmark := Copy(LURI, LTokenPos + 1, MaxInt);
        LURI := Copy(LURI, 1, LTokenPos - 1);
      end;
    end;
    // Get the path
    LTokenPos := RPos('/', LURI, -1);
    if LTokenPos > 0 then begin
      FPath := Copy(LURI, 1, LTokenPos);
      Delete(LURI, 1, LTokenPos);
    end;
  end;
  // Get the document
  FDocument := LURI;
end;

constructor TIdURI.Create(const AURI: string);
begin
  SetURI(AURI);
end;

function TIdURI.GetFullURI(AuthAndBookmark: Boolean): string;
var
  LURI: String;
begin
  if FProtocol = '' then begin
    raise Exception.Create('Protocol field is empty');
  end;
  if FHost = '' then begin
    raise Exception.Create('Host field is empty');
  end;
  LURI := FProtocol + '://';
  if (FUserName <> '') and AuthAndBookmark then
    begin
      LURI := LURI + FUserName;
      if FPassword <> '' then begin
        LURI := LURI + ':' + FPassword;
      end;
      LURI := LURI + '@';
    end;
  if IPVersion = 6 then begin
    LURI := LURI + '[' + FHost + ']';
  end else begin
    LURI := LURI + FHost;
  end;
  if FPort <> '' then
  begin
    if AnsiPos('HTTP', FProtocol) > 0 then
      begin
        if FPort <> '80' then
          LURI := LURI + ':' + FPort;
      end
    else if AnsiPos('HTTPS', FProtocol) > 0 then
      begin
        if FPort <> '443' then
          LURI := LURI + ':' + FPort;
      end
    else if AnsiPos('FTP', FProtocol) > 0 then
      begin
        if FPort <> '21' then
          LURI := LURI + ':' + FPort;
      end
    else
      LURI := LURI + ':' + FPort;
  end;
  LURI := LURI + GetPathAndParams;
  if (FBookmark <> '') and AuthAndBookmark then begin
    LURI := LURI + '#' + FBookmark;
  end;
  Result := LURI;
end;

function TIdURI.GetPathAndParams: String;
begin
  Result := FPath + FDocument;
  if FParams <> '' then begin
    Result := Result + '?' + FParams;
  end;
end;

function CharPosInSet(const AString: string; const ACharPos: Integer; const ASet: String): Integer;
var
  LChar: Char;
  I: Integer;
begin
  Result := 0;
  if ACharPos < 1 then begin
    raise Exception.Create('Invalid ACharPos');{ do not localize }
  end;
  if ACharPos <= Length(AString) then
    begin
      LChar := AString[ACharPos];
      for I := 1 to Length(ASet) do
        begin
          if ASet[I] = LChar then
            begin
              Result := I;
              Exit;
            end;
        end;
    end;
end;

class procedure TIdURI.NormalizePath(var APath: string);
var
  i, PathLen: Integer;
  LChar: Char;
begin
  // Normalize the directory delimiters to follow the UNIX syntax
  // 8/10/2010: only normalize within the actual path,
  // nothing outside of it...
  i := Pos(':', APath);
  if i > 0 then
    begin
      Inc(i);
      // if the path does not already begin with '//', then do not
      // normalize the first two characters if they would produce
      // '//', as that will change the semantics of the URL...
      if (CharPosInSet(APath, I, '\/') > 0) and (CharPosInSet(APath, I+1, '\/')>0) then
        Inc(i, 2);
    end
  else
    i := 1;
  PathLen := Length(APath);
  while i <= PathLen do
    begin
      LChar := APath[i];
      if (LChar = '?') or (LChar = '#') then
        Break; // stop normalizing at query/fragment portion of the URL
      if LChar = '\' then
        APath[i] := '/';
      Inc(i);
    end;
end;

end.

