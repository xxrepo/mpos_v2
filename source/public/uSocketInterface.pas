unit uSocketInterface;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  uSocketBasiceInterface;
  //uURLInterface;//,
  //Generics.Collections, Generics.Defaults;

Type
  TValueReturn = -1..1;

  TTimeOutMode=(M_TOTAL,M_STEP);
  THttpProtocalMode=(M_HTTP,M_HTTPS);

  TSocketProtocalMode=(P_TCP,P_UDP);
  //P_SOCKETPROTO=(P_TCP,P_UDP);
  TSocketBlockMode=(T_BLOCK,T_NONBLOCK);

  TNonblockCallBackProc=Procedure ();
  TNonblcokCallBackEven=Procedure () of object;
  TRecvDataCompleteEvenFunc=Function (RecvBytes:TBytes):TValueReturn of object;  //--返回值: 1:表示已完成
  TRecvDataCompleteFunc=Function (RecvBytes:TBytes):TValueReturn;      //--返回值: 1:表示已完成


  { ISocketSetting }

  ISocketSetting=Interface(IBasicParams)
  ['{B6C3EE4E-8AD4-4948-A65C-ABD668A32045}']
    Function GetRemoteHost():String;
    Function GetRemotePort():Word;
    Function GetHttpProtocalMode():THttpProtocalMode;
    Function GetSocketProtocolMode():TSocketProtocalMode;
    Function GetTimeOutMode():TTimeOutMode;  //--获取超时时间模式
    Function GetHttpTimeOut():Integer;
    Function GetConnectTimeOut():Integer ;
    Function GetSendTimeOut():Integer;
    Function GetRecvTimeOut():Integer;
    Function GetResolveHostTimeOut():Integer;   //--获取域名解析超时时长
    Function GetBestHostTimeOut():Integer;      //--获取找寻最优IP地址超时时长

    Function GetSocketBlockMode():TSocketBlockMode;  //--获取socket通信模式，如:阻塞,非阻塞等
    Function GetNonblockCallBackEven():TNonblcokCallBackEven; //--获取socket非阻塞模式回调事件
    Function GeTNonblockCallBackProc():TNonblockCallBackProc; //--获取socket非阻塞模式回调方法
    Function GetSendBufferSize():Integer;
    Function GetRecvBufferSize():Integer;
    Function GetCheckRecvCompletedEvenFunc():TRecvDataCompleteEvenFunc;
    Function GetCheckRecvCompletedFunc():TRecvDataCompleteFunc;

    Procedure AddRemoteHost(AValue:String);
    Procedure AddRemotePort(AValue:Word);
    Procedure AddHttpProtocalMode(AValue:THttpProtocalMode);
    Procedure AddSocketProtocolMode(AValue:TSocketProtocalMode);
    Procedure AddTimeOutMode(AValue:TTimeOutMode);  //--获取超时时间模式
    Procedure AddHttpTimeOut(AValue:Integer);
    Procedure AddConnectTimeOut(AValue:Integer);
    Procedure AddSendTimeOut(AValue:Integer);
    Procedure AddRecvTimeOut(AValue:Integer);
    Procedure AddResolveHostTimeOut(AValue:Integer);   //--设定域名解析超时时长
    Procedure AddBestHostTimeOut(AValue:Integer);      //--设定找寻最优IP地址超时时长

    Procedure AddSocketBlockMode(AValue:TSocketBlockMode);  //--获取socket通信模式，如:阻塞,非阻塞等
    Procedure AddNonblockCallBackEven(AValue:TNonblcokCallBackEven); //--获取socket回调事件
    Procedure AddNonblockCallBackProc(AValue:TNonblockCallBackProc); //--获取socket回调方法
    Procedure AddSendBufferSize(AValue:Integer);
    Procedure AddRecvBufferSize(AValue:Integer);
    Procedure AddCheckRecvCompleted(AValue:TRecvDataCompleteEvenFunc); overload;   //--设定检查接收数据的完整性的函数，已接收完，则返回1，异常返回-1;
    Procedure AddCheckRecvCompleted(AValue:TRecvDataCompleteFunc); overload;

    //Procedure Clear();
    //Function EnumParam():String;
  end;


  { IBaseSocket }
  TSocketHandles=Array of THandle;

  IBaseSocket=Interface
  ['{9A4952E8-571A-4B53-A5FD-4659AC4FBEAB}']
    Function SocketSettings:ISocketSetting;

    Procedure InitSocket();
    Procedure SetSocketMode(Mode:TSocketBlockMode);
    Function Connect(Timeout:Integer; isCheckConnected:Boolean=True):TValueReturn;
    Procedure Close();
    Function Send(Data:Pointer;Size:Integer; TimeOut:Integer):Integer;
    Function Recv(var RetData:TBytes; TimeOut:Integer):Integer; overload;
    Function Recv(var RetData:TBytes):integer; overload;
    Function SendAndRecv(Data:Pointer;Size:Integer;var RetData:TBytes; TimeOut:Integer):Integer;

    Function CanWrite(SocketIDs:TSocketHandles; var CanWriteSocket:TSocketHandles; TimeOut:Integer):Integer;
    Function CanRead(SocketIDs:TSocketHandles; var CanReadSocket:TSocketHandles; TimeOut:Integer):Integer;
    Function Recv(OptSocketID:THandle; var RetData:TBytes):integer; overload;

    Function GetSocketID():Integer;
    Function GetLastErrNo():Integer;
    Function GetSocketErrNo():Integer; overload;
    Function GetSocketErrNo(SocketNo:Integer):Integer; overload;

    //Procedure SetSocketMode(Mode:TSocketMode);  //--设置socket为非阻塞模式
    Procedure SetSendTimeOut(TimeOut:Integer);
    Procedure SetRecvTimeOut(TimeOut:Integer);
  end;

  IDNSSocket=Interface(IBaseSocket)
    ['{F7D2A6A7-CF1B-45D5-A45E-CD2FA3DD3755}']
    Function GetLocalDSNServerInfo( DNSServerList:TStringList):Integer;
    Procedure AddDNSServer( DNSServerName,DNSServerIP:String);  //--添加DNS域名解析服务器IP地址
    Procedure DeleteDNSServer(DNSServerName:String);
    Function ResolveHost(HostName:String;  IPList:TStringList; TimeOut:Integer):Integer;
  end;

  IHttpSocket=Interface(IDNSSocket)
    ['{C5036415-4CAE-45A4-9E45-974E7F19BA7B}']
    Function BestIPAddress(IPAddrList:TStringList;  TimeOut:Integer):Integer;
    Function Get(RequestContent:String; out ResponseBytes:TBytes):Integer;  //--返回接收到字节数据
    Function Post(RequestContent:String; out ResponseBytes:TBytes):Integer;  //--返回接收到字节数据
  end;


  { TSocketSetting }

  TSocketSetting=Class(TBasicParam,ISocketSetting)
  Public
    Function GetRemoteHost():String;
    Function GetRemotePort():Word;
    Function GetHttpProtocalMode():THttpProtocalMode;
    Function GetSocketProtocolMode():TSocketProtocalMode;
    Function GetTimeOutMode():TTimeOutMode;  //--获取超时时间模式
    Function GetHttpTimeOut():Integer;
    Function GetConnectTimeOut():Integer ;
    Function GetSendTimeOut():Integer;
    Function GetRecvTimeOut():Integer;
    Function GetResolveHostTimeOut():Integer;   //--获取域名解析超时时长
    Function GetBestHostTimeOut():Integer;      //--获取找寻最优IP地址超时时长

    Function GetSocketBlockMode():TSocketBlockMode;  //--获取socket通信模式，如:阻塞,非阻塞等

    Function GetNonblockCallBackEven():TNonblcokCallBackEven; //--获取socket非阻塞模式回调事件
    Function GeTNonblockCallBackProc():TNonblockCallBackProc; //--获取socket非阻塞模式回调方法

    Function GetSendBufferSize():Integer;
    Function GetRecvBufferSize():Integer;

    Function GetCheckRecvCompletedEvenFunc():TRecvDataCompleteEvenFunc;
    Function GetCheckRecvCompletedFunc():TRecvDataCompleteFunc;

    Procedure AddRemoteHost(AValue:String);
    Procedure AddRemotePort(AValue:Word);
    Procedure AddHttpProtocalMode(AValue:THttpProtocalMode);
    Procedure AddSocketProtocolMode(AValue:TSocketProtocalMode);
    Procedure AddTimeOutMode(AValue:TTimeOutMode);  //--获取超时时间模式
    Procedure AddHttpTimeOut(AValue:Integer);
    Procedure AddConnectTimeOut(AValue:Integer);
    Procedure AddSendTimeOut(AValue:Integer);
    Procedure AddRecvTimeOut(AValue:Integer);
    Procedure AddResolveHostTimeOut(AValue:Integer);   //--设定域名解析超时时长
    Procedure AddBestHostTimeOut(AValue:Integer);      //--设定找寻最优IP地址超时时长

    Procedure AddSocketBlockMode(AValue:TSocketBlockMode);  //--获取socket通信模式，如:阻塞,非阻塞等
    Procedure AddNonblockCallBackEven(AValue:TNonblcokCallBackEven); //--获取socket回调事件
    Procedure AddNonblockCallBackProc(AValue:TNonblockCallBackProc); //--获取socket回调方法
    Procedure AddSendBufferSize(AValue:Integer);
    Procedure AddRecvBufferSize(AValue:Integer);
    Procedure AddCheckRecvCompleted(AValue:TRecvDataCompleteEvenFunc); overload;   //--设定检查接收数据的完整性的函数，已接收完，则返回1，异常返回-1;
    Procedure AddCheckRecvCompleted(AValue:TRecvDataCompleteFunc); overload;

    //Procedure Clear; override;
    //Function EnumParam():String;override;
  end;
  {
  TSocket=Class(TSocketSetting,ISocket)
  Protected
    FDNSServer: TObjectList<TBasicURL>;
    Function IndexofDNSServer(DNSServer:TBasicURL):Integer; virtual;
  Public
    Constructor Create();
    Destructor Destroy; override;

    Procedure AddDNSServer(DNSServer:String);  //--添加DNS域名解析服务器IP地址
    Procedure DeleteDNSServer(DNSServer:String);
    Function ResolveHost(HostName:String; Const TimeOut:Integer=0):TValueReturn;

    Function BestIPAddress(IPAddrList:TStringList; Const TimeOut:Integer=0):Integer;

    Function Connect(Const TimeOut:Integer=0):TValueReturn;
    Function Send(Const Data;Size:Integer;Const TimeOut:Integer=0):Integer;
    Function Recv(out RetData:TBytes;Const TimeOut:Integer=0):Integer;
    Function SendAndRecv(Const Data;Size:Integer;out RetData:TBytes;Const TimeOut:Integer=0):TValueReturn;
  end;
  }
implementation

{ TSocket }
{
function TSocket.IndexofDNSServer(DNSServer: TBasicURL): Integer;
Var
  i:integer;
begin
  Result:=-1;
  For i:= FDNSServer.Count downto 1 do
  Begin
    if CompareText(DNSServer.Host, TBasicURL(FDNSServer.Items[i-1]).Host)=0 Then
    Begin
      Result:=i-1;
      Break;
    end;
  end;
end;

constructor TSocket.Create();
begin
  Inherited;
  FDNSServer:= TObjectList<TBasicURL>.Create();
end;

destructor TSocket.Destroy;
begin
  FDNSServer.Free;
  inherited Destroy;
end;

procedure TSocket.AddDNSServer(DNSServer: String);
Var
  vURLCls:TBasicURL;
begin
  vURLCls:=TBasicURL.Create;
  vURLCls.SetURLAddr(DNSServer);
  if IndexofDNSServer(vURLCls)=-1 Then
    FDNSServer.Add(vURLCls)
  else
    vURLCls.Free;
end;

procedure TSocket.DeleteDNSServer(DNSServer: String);
Var
  index:Integer;
  vURLCls:TBasicURL;
begin
  vURLCls:=TBasicURL.Create;
  vURLCls.SetURLAddr(DNSServer);
  Index:= IndexofDNSServer(vURLCls);
  if index>=0 then
    FDNSServer.Delete(Index);
  vURLCls.Free;
end;

function TSocket.ResolveHost(HostName: String; const TimeOut: Integer): TValueReturn;
begin

end;

function TSocket.BestIPAddress(IPAddrList: TStringList; const TimeOut: Integer): Integer;
begin

end;

function TSocket.Connect(const TimeOut: Integer): TValueReturn;
begin

end;

function TSocket.Send(const Data; Size: Integer; const TimeOut: Integer): Integer;
begin

end;

function TSocket.Recv(out RetData: TBytes; const TimeOut: Integer): Integer;
begin

end;

function TSocket.SendAndRecv(const Data; Size: Integer; out RetData: TBytes;
  const TimeOut: Integer): TValueReturn;
begin

end;
}
{ TSocketSetting }

function TSocketSetting.GetRemoteHost(): String;
begin
  Result:= Self.Get('RemoteHost').AsString;
  //Result:= GetParam('RemoteHost','');
end;

function TSocketSetting.GetRemotePort(): Word;
begin
  Result:= Self.Get('RemotePort').AsInteger;
  //Result:=GetParam('RemotePort',0);
end;

function TSocketSetting.GetHttpProtocalMode: THttpProtocalMode;
begin
  Result:= THttpProtocalMode(Self.Get('HttpProtocalMode').AsInteger);

end;

function TSocketSetting.GetSocketProtocolMode(): TSocketProtocalMode;
begin
  Result:= TSocketProtocalMode(Self.Get('SocketProtocolMode').AsInteger);
  //Result:= P_SOCKETPROTO(GetParam('SocketProtocolMode',ord(P_TCP)));
end;

function TSocketSetting.GetTimeOutMode(): TTimeOutMode;
begin
  Result:=TTimeOutMode(Get('TimeOutMode').AsInteger);
end;

function TSocketSetting.GetHttpTimeOut(): Integer;
begin
  Result:= Get('HttpTimeOut').AsInteger;
end;

function TSocketSetting.GetConnectTimeOut(): Integer;
begin
  Result:= Get('ConnectTimeOut').AsInteger;
end;

function TSocketSetting.GetSendTimeOut(): Integer;
begin
  Result:= Get('SendTimeOut').AsInteger;
end;

function TSocketSetting.GetRecvTimeOut(): Integer;
begin
  Result:= Get('RecvTimeOut').AsInteger;
end;

function TSocketSetting.GetResolveHostTimeOut(): Integer;
begin
  Result:= Get('ResolveHostTimeOut').AsInteger;
end;

function TSocketSetting.GetBestHostTimeOut(): Integer;
begin
  Result:= Get('BestHostTimeOut').AsInteger;
end;

function TSocketSetting.GetSocketBlockMode(): TSocketBlockMode;
begin
  Result:= TSocketBlockMode(Get('SocketBlockMode').AsInteger) ;
end;

function TSocketSetting.GetNonblockCallBackEven(): TNonblcokCallBackEven;
Var
  P:Pointer;
begin
  Result:=Nil;
  P:= get('NonblockBackEven').AsPointer;//,Nil);
  if P<>Nil Then
    Pointer(@Result):= P;
end;

function TSocketSetting.GeTNonblockCallBackProc(): TNonblockCallBackProc;
Var
  P:Pointer;
begin
  Result:=Nil;
  P:= get('NonblockBackProc').AsPointer;//,Nil);
  if P<>Nil Then
    Pointer(@Result):= P;
end;

function TSocketSetting.GetSendBufferSize(): Integer;
begin
  Result:= get('SendBufferSize').AsInteger;
end;

function TSocketSetting.GetRecvBufferSize(): Integer;
begin
  Result:= get('RecvBufferSize').AsInteger;
end;

function TSocketSetting.GetCheckRecvCompletedEvenFunc(): TRecvDataCompleteEvenFunc;
Var
  P:Pointer;
begin
  Result:=Nil;
  P:= get('CheckRecvCompletedEvenFunc').AsPointer;// ,Nil);
  if P<>Nil Then
    Pointer(@Result):= P;
end;


function TSocketSetting.GetCheckRecvCompletedFunc(): TRecvDataCompleteFunc;
Var
  P:Pointer;
begin
  Result:=Nil;

  P:= get('CheckRecvCompletedFunc').AsPointer;//,Nil);
  if P<>Nil Then
    Pointer(@Result):= P;
end;

procedure TSocketSetting.AddRemoteHost(AValue: String);
begin
  SetString('RemoteHost',AValue);
end;

procedure TSocketSetting.AddRemotePort(AValue: Word);
begin
  SetInteger('RemotePort',AValue);
end;

procedure TSocketSetting.AddHttpProtocalMode(AValue: THttpProtocalMode);
begin
  SetInteger('HttpProtocalMode',Ord(AValue));
end;

procedure TSocketSetting.AddSocketProtocolMode(AValue: TSocketProtocalMode);
begin
  SetInteger('SocketProtocolMode',Ord(AValue));
end;

procedure TSocketSetting.AddTimeOutMode(AValue: TTimeOutMode);
begin
  SetInteger('TimeOutMode',ord(AValue));
end;

procedure TSocketSetting.AddHttpTimeOut(AValue: Integer);
begin
  SetInteger('HttpTimeOut',AValue);
end;

procedure TSocketSetting.AddConnectTimeOut(AValue: Integer);
begin
  SetInteger('ConnectTimeOut',AValue);
end;

procedure TSocketSetting.AddSendTimeOut(AValue: Integer);
begin
  SetInteger('SendTimeOut',AValue);
end;

procedure TSocketSetting.AddRecvTimeOut(AValue: Integer);
begin
  SetInteger('RecvTimeOut',AValue);
end;

procedure TSocketSetting.AddResolveHostTimeOut(AValue: Integer);
begin
  SetInteger('ResolveHostTimeOut',AValue);
end;

procedure TSocketSetting.AddBestHostTimeOut(AValue: Integer);
begin
  SetInteger('BestHostTimeOut',AValue);
end;

procedure TSocketSetting.AddSocketBlockMode(AValue: TSocketBlockMode);
begin
  SetInteger('SocketBlockMode',ord(AValue));
end;

procedure TSocketSetting.AddNonblockCallBackEven(AValue: TNonblcokCallBackEven);
begin
  SetPointer('NonblockBackEven',@AValue);
end;

procedure TSocketSetting.AddNonblockCallBackProc(AValue: TNonblockCallBackProc);
begin
  SetPointer('NonblockBackProc',@AValue);
end;

procedure TSocketSetting.AddSendBufferSize(AValue: Integer);
begin
  SetInteger('SendBufferSize',AValue);
end;

procedure TSocketSetting.AddRecvBufferSize(AValue: Integer);
begin
  SetInteger('RecvBufferSize',AValue);
end;

procedure TSocketSetting.AddCheckRecvCompleted(AValue: TRecvDataCompleteEvenFunc);
begin
  SetPointer('CheckRecvCompletedEvenFunc',@AValue);
end;

procedure TSocketSetting.AddCheckRecvCompleted(AValue: TRecvDataCompleteFunc);
begin

  SetPointer('CheckRecvCompletedFunc',@AValue);
end;
{
procedure TSocketSetting.Clear();
begin
  Inherited;
end;

function TSocketSetting.EnumParam(): String;
begin
  Result:=Inherited;
end;
}
end.

