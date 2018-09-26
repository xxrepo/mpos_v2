unit Unit1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, Menus,
  cm_messager, cm_sysutils, cm_netutils, cm_PlatBase, cm_logutils,
  cm_cmstp,
  cm_CMSTPUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button7: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure println(const msg: string);
  public
    Messager: TCMMessager;
  end;


var
  Form1: TForm1;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
var
  s: string;
  uri: TCMURL;
begin
  s := 'http://aaa:111@localhost:80/a/b/c.do?x=aaa&y=123#www.pbccrc.org.cn';
  Memo1.Lines.Add(s);
  uri := TCMURL.Create(s);
  Memo1.Lines.Add('Protocol:' + uri.Protocol);
  Memo1.Lines.Add('Username:' + uri.Username);
  Memo1.Lines.Add('Password:' + uri.Password);
  Memo1.Lines.Add('Host:' + uri.Host);
  Memo1.Lines.Add('Port:' + uri.Port);
  Memo1.Lines.Add('Path:' + uri.Path);
  Memo1.Lines.Add('ContextPath:' + uri.ContextPath);
  Memo1.Lines.Add('ServletPath:' + uri.ServletPath);
  Memo1.Lines.Add('Document:' + uri.Document);
  Memo1.Lines.Add('Params:' + uri.Params);
  Memo1.Lines.Add('Bookmark:' + uri.Bookmark);

  //Memo1.Lines.Add('URI:' + uri.URL);
  Memo1.Lines.Add('URI:' + uri.GetFullURL());
  Memo1.Lines.Add('URI:' + uri.GetFullURL(False));
end;

var
  _Str: string;

procedure TForm1.Button1Click(Sender: TObject);
var
   str: String;
begin
   str := 'abcd我';
   _Str := str;
   //Memo1.Lines.Text :=
   //  '分配大小: ' + IntToStr(PInteger(Integer(str) - 12)^) + #13#10 +
   //  '引用计数: ' + IntToStr(PInteger(Integer(str) - 8)^) + #13#10 +
   //  '字串长度: ' + IntToStr(PInteger(Integer(str) - 4)^) + #13#10 +
   //  '字串: ' + PChar(Integer(str));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Add('RPos:' + IntToStr(Pos('A7', '12456A7')));
  Memo1.Lines.Add('RPos:' + IntToStr(Pos('A7', '123456A78')));
  Memo1.Lines.Add('RPos:' + IntToStr(Pos('A7', 'A7123456')));
  Memo1.Lines.Add('------------------------------');
  Memo1.Lines.Add('RPos:' + IntToStr(PosR('A7', '12456A7')));
  Memo1.Lines.Add('RPos:' + IntToStr(PosR('A7', '123456A78')));
  Memo1.Lines.Add('RPos:' + IntToStr(PosR('A7', 'A7123456')));
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  a, b, c: string;
begin
  a := '123456';
  b := '3';
  c := RCutStr(a, b);

  Memo1.Lines.Add(a);
  Memo1.Lines.Add(b);
  Memo1.Lines.Add(c);
  Memo1.Lines.Add('------------------------------');
end;

procedure TForm1.Button5Click(Sender: TObject);
//var
//  conn: TCMSTPURLConnection;
//  s: TService;
//  server: TServer;
begin
  //s := TService.Create;
  //CMSTPService := s;
  //server := TServer.Create;
  //server.ParseConfig('server.xml');
  //
  //conn := TCMSTPURLConnection.Create('cmstp://test/a/b/c?x=aaa&y=123');
  //conn.Connect;
  //
  //println(conn.ResponseContent.Get('test1').AsString);
  //println('----------');
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  conn: TCMSTPURLConnection;
begin
  Messager.Info('----     TCMSTPURLConnection  ----------- ###################################');
  conn := TCMSTPURLConnection.Create('cmstp://test:80/test/a/b?x=aaa&y=123');
  conn.RequestParameters.SetString('msg', 'How are you?');
  conn.Connect;
  println('--re--:' + conn.ResponseContent.Get('test').AsString);
  println('--over--------');
end;

procedure TForm1.FormShow(Sender: TObject);
var
  FLogger: TCMJointFileLogger;
  FMessageHandler: ICMMessageHandler;
begin
  FLogger := TCMJointFileLogger.Create(Application);
  FLogger.FileNamePrefix := 'servlet_';
  FLogger.Info('============================================================');
  FMessageHandler := TCMLogMessageHandler.Create(FLogger);
  //SetDefaultMessageHandler(FMessageHandler);
  //Self.Messager.AddMessageHandler(FMessageHandler);

  cm_messager.TCMMessageManager.DefaultHandler := FMessageHandler;

  Self.Messager := TCMMessageManager.GetInstance.GetMessager(Self);
  //
  InitCMSTPService;
end;

procedure TForm1.println(const msg: string);
begin
  Memo1.Lines.Add(msg);
end;

end.

