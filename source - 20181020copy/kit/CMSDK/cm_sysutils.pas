{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_sysutils

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_sysutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils,
  LazFileUtils,
  LazUTF8;

  function RepairLibraryFileExt(const fileName: string; checkPrefix: Boolean=False): string;
  function RepairPathDelim(const APath: string): string; //纠正分隔符。另参见:ForcePathDelims()
  function PathDelimToSlash(const APath: string): string; //分隔符切换成反斜杠
  function RepairPath(const APath: string): string;
  function RepairUTF8Path(const AUTF8Path: string): string;
  function RepairFilePath(const AFileName: string): string;
  function RepairFileUTF8Path(const AUFT8FileName: string): string;
  function RepairSpacePath(const APath: string): string; //路径存在空格时
  //Just at the beginning and end
  function QuotedCHStr(const AStr: string): string;
  function IfEmpty(const AValue, AThen: string): string;
  //
  function ValidateStr(const ARegExprExpression, AStr: string): Boolean;
  //
  function StreamToString(AStream: TStream): string;
  function FileToString(AFileName: TFileName): string;
  function StringToFile(const AStr: string; AFileName: TFileName): Boolean;
  //SysUtils has IntToHex()
  function HexToInt(const AHex: string): Integer;
  function EncodeHexString(const AStr: string): string;
  function DecodeHexString(const Hexs: string): string;

  function GetDefFmtDateTimeStr(const ADateTime: TDateTime): string;
  function GetDayTimeInteger(const ADateTime: TDateTime): Integer;
  function CreateGUIDStr: string;

  function LeftFillChar(const Str: string; Len: Integer; FillChar: Char): string;
  function RightFillChar(const Str: string; Len: Integer; FillChar: Char): string;

  function LCutStr(var AStr: string; const ADelim: string): string; //没找到 Delim 则左边切到空字符串
  function RCutStr(var AStr: string; const ADelim: string): string; //没找到 Delim 则右边切到空字符串
  function PosR(const ASubStr, AStr: string): Integer; //从右往左找，找到返回以左为基准位置

const
  CMFormatSettings : TFormatSettings = (
          CurrencyFormat: 2;
          NegCurrFormat: 2;
          ThousandSeparator: ',';
          DecimalSeparator: '.';
          CurrencyDecimals: 2;
          DateSeparator: '-';
          TimeSeparator: ':';
          ListSeparator: ',';
          CurrencyString: '金额:';
          ShortDateFormat: 'yyyy-MM-dd';
          LongDateFormat: 'yyyy年MM月dd日';
          TimeAMString: '上午';
          TimePMString: '下午';
          ShortTimeFormat: 'hh:nn.ss';
          LongTimeFormat: 'hh:nn:ss.zzz';
          ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
          LongMonthNames: ('一月','二月','三月','四月','五月','六月','七月','八月','九月','十月','十一月','十二月');
          ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
          LongDayNames:  ('星期日','星期一','星期二','星期三','星期四','星期五','星期六');
          TwoDigitYearCenturyWindow: 50;
        );

implementation

uses RegExpr;

function RepairLibraryFileExt(const fileName: string; checkPrefix: Boolean=False): string;
var
  fn: string;
begin
  fn := fileName;
  {$IFDEF UNIX}
  if checkPrefix then
    begin
      fn := ExtractFileName(fileName);
      if (LowerCase(Copy(fn, 1, 3)) = 'lib') then
        fn := fileName
      else
        fn := ExtractFilePath(fileName) + 'lib' + fn;
    end;
  Result := ChangeFileExt(fn, '.so');
  {$ELSE}
  if checkPrefix then
    begin
      fn := ExtractFileName(fileName);
      if (LowerCase(Copy(fn, 1, 3)) = 'lib') then
        fn := ExtractFilePath(fileName) + RightStr(fn, Length(fn)-3)
      else
        fn := fileName;
    end;
  Result := ChangeFileExt(fn, '.dll');
  {$ENDIF}
end;

function RepairPathDelim(const APath: string): string;
begin
  Result := APath;
  Result := StringReplace(APath, {$IFDEF UNIX}'\'{$ELSE}'/'{$ENDIF}, PathDelim, [rfReplaceAll]);
end;

function PathDelimToSlash(const APath: string): string;
begin
  Result := APath;
  Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
end;

function RepairPath(const APath: string): string;
var
  path: string;
begin
  Result := APath;
  path := RepairPathDelim(APath);
  {$IFDEF UNIX}
  if (LeftStr(path, 1) <> PathDelim) and (LeftStr(path, 2) <> '..') then
    path := ExtractFilePath(paramStr(0)) + path;
  {$ELSE}
  if (Copy(path, 2, 1) <> ':') and (LeftStr(path, 2) <> '..') then
    begin
      if LeftStr(path, 1) = PathDelim then
        path := ExtractFileDir(paramStr(0)) + path
      else
        path := ExtractFilePath(paramStr(0)) + path;
    end;
  {$ENDIF}
  if RightStr(path, 1) <> PathDelim then
    path := path + PathDelim;
  Result := path;
end;

function RepairUTF8Path(const AUTF8Path: string): string;
var
  path: string;
begin
  Result := AUTF8Path;
  path := RepairPathDelim(AUTF8Path);
  {$IFDEF UNIX}
  if (LeftStr(path, 1) <> PathDelim) and (LeftStr(path, 2) <> '..') then
    path := ExtractFilePath(SysToUTF8(paramStr(0))) + path;
  {$ELSE}
  if (Copy(path, 2, 1) <> ':') and (LeftStr(path, 2) <> '..') then
    begin
      if LeftStr(path, 1) = PathDelim then
        path := ExtractFileDir(SysToUTF8(paramStr(0))) + path
      else
        path := ExtractFilePath(SysToUTF8(paramStr(0))) + path;
    end;
  {$ENDIF}
  if RightStr(path, 1) <> PathDelim then
    path := path + PathDelim;
  Result := path;
end;

function RepairFilePath(const AFileName: string): string;
begin
  Result := RepairPath(ExtractFilePath(AFileName)) + ExtractFileName(AFileName);
end;

function RepairFileUTF8Path(const AUFT8FileName: string): string;
begin
  Result := RepairUTF8Path(ExtractFilePath(AUFT8FileName)) + ExtractFileName(AUFT8FileName);
end;

function RepairSpacePath(const APath: string): string;
begin
  Result := APath;
  {$IFDEF UNIX}
  if AnsiPos(' ', APath) > 0 then
    Result := '"' + APath + '"';
  {$ENDIF}
end;

function QuotedCHStr(const AStr: string): string;
begin
  Result := '“' + AStr + '”';
end;

function IfEmpty(const AValue, AThen: string): string;
begin
  if Trim(AValue) = '' then
    Result := AThen
  else
    Result := AValue;
end;

function ValidateStr(const ARegExprExpression, AStr: string): Boolean;
var
  re: TRegExpr;
begin
  Result := False;
  re := TRegExpr.Create(ARegExprExpression);
  try
    Result := RE.Exec(AStr);
  finally
    FreeAndNil(re);
  end;
end;

function StreamToString(AStream: TStream): string;
var
  i: Integer;
begin
  Result := '';
  if not Assigned(AStream) then
    Exit;
  SetLength(Result, AStream.Size);
  for  i := 0 to Pred(AStream.Size) do
    try
      AStream.Position := I;
      AStream.Read(Result[Succ(I)], 1);
    except
      Result := '';
    end;
end;

function FileToString(AFileName: TFileName): string;
var
  vFileChar: file of Char;
  vChar: Char;
begin
  Result := '';
  {$I-}
  AssignFile(vFileChar, AFileName);
  Reset(vFileChar);
  while not EOF(vFileChar) do
    begin
      Read(vFileChar, vChar);
      Result := Result + vChar;
    end;
  CloseFile(vFileChar);
  {$I+}
end;

function StringToFile(const AStr: string; AFileName: TFileName): Boolean;
var
  vFileChar: file of Char;
  i: Integer;
begin
  Result := False;
  if AFileName = '' then
    Exit;
  {$I-}
  AssignFile(vFileChar, AFileName);
  Rewrite(vFileChar);
  for  i := 1 to Length(AStr) do
    Write(vFileChar, AStr[i]);
  CloseFile(vFileChar);
  {$I+}
  Result := IOResult = 0;
end;

function HexToInt(const AHex: string): Integer;
var
  i: Integer;
  theHex: string;
  function Ncf(num, f: Integer): Integer;
  var
    i: Integer;
  begin
    Result := 1;
    if f = 0 then
      Exit;
    for i := 1 to f do
      Result := Result * num;
  end;
  function HexCharToInt(HexToken: Char): Integer;
  begin
    if HexToken > #97 then
      HexToken := Chr(Ord(HexToken) - 32);
      Result := 0;
    if (HexToken > #47) and (HexToken < #58) then { chars 0....9 }
      Result := Ord(HexToken) - 48
    else if (HexToken > #64) and (HexToken < #71) then { chars A....F }
      Result := Ord(HexToken) - 65 + 10;
  end;
begin
  Result := 0;
  theHex := AnsiUpperCase(Trim(AHex));
  if theHex = '' then
    Exit;
  for i := 1 to Length(theHex) do
    Result := Result + HexCharToInt(theHex[i]) * Ncf(16, length(theHex) - i);
end;

function EncodeHexString(const AStr: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(AStr) do
    begin
      Result := Result + IntToHex(Ord(AStr[i]), 2);
    end;
end;

function DecodeHexString(const Hexs: string): string;
var
  i: Integer;
begin
   Result := '';
   i := 1;
   while i < length(Hexs) do
     begin
       Result := Result + Chr(HexToInt(Hexs[i] + Hexs[i+1]));
       i := i + 2;
     end;
   Result := Result;
end;

function GetDefFmtDateTimeStr(const ADateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ADateTime, CMFormatSettings);
end;

function GetDayTimeInteger(const ADateTime: TDateTime): Integer;
begin
  Result := HourOf(ADateTime) * 10000000 + MinuteOf(ADateTime) * 100000 + SecondOf(ADateTime) * 1000 + MilliSecondOf(ADateTime);
end;

function CreateGUIDStr: string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  SetLength(Result, 36);
  StrLFmt(PChar(Result), 36,'%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',
    [
     Longint(GUID.D1), GUID.D2, GUID.D3,
     GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
     GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]
    ]);
end;

function LeftFillChar(const Str: string; Len: Integer; FillChar: Char): string;
begin
  Result := StringOfChar(FillChar, Len - Length(Str)) + Str;
end;

function RightFillChar(const Str: string; Len: Integer; FillChar: Char): string;
begin
  Result := Str + StringOfChar(FillChar, Len - Length(Str));
end;

function LCutStr(var AStr: string; const ADelim: string): string;
var
  LPos: Integer;
begin
  Result := '';
  if ADelim = #0 then
    LPos := Pos(ADelim, AStr) // AnsiPos does not work with #0
  else
    LPos := AnsiPos(ADelim, AStr);
  if LPos > 0 then
    begin
      Result := Copy(AStr, 1, LPos - 1);
      AStr := Copy(AStr, LPos + Length(ADelim), MaxInt);
    end;
end;

function RCutStr(var AStr: string; const ADelim: string): string;
var
  LPos: Integer;
begin
  Result := '';
  if ADelim = #0 then
    LPos := Pos(ADelim, AStr) // AnsiPos does not work with #0
  else
    LPos := AnsiPos(ADelim, AStr);
  if LPos > 0 then
    begin
      Result := Copy(AStr, LPos + Length(ADelim), MaxInt);
      AStr := Copy(AStr, 1, LPos - 1);
    end;
end;

function PosR(const ASubStr, AStr: string): Integer;
var
  i: Integer;
  subLen: Integer;
begin
  Result := 0;
  subLen := Length(ASubStr);
  for i := Length(AStr) - 1 downto 1 do
    begin
      if SameText(Copy(AStr, i, subLen), ASubStr) then
        begin
          Result := i;
          Break;
        end;
    end;
end;


end.

