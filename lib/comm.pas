unit comm;
interface
uses Windows, SysUtils, WinSock;

// THTTP allows exchanging data with web servers via GET and POST requests.

type
  THTTP         = class(TObject)
  private
    procedure   Execute(Server : AnsiString; Port : Cardinal);
  public
    Header      : AnsiString;
    Request     : AnsiString;
    Response    : AnsiString;
    constructor Create;
    procedure   Get(Server,URI : AnsiString; Port : Cardinal = 80);
    procedure   Post(Server,URI,Data : AnsiString; Port : Cardinal = 80);
    procedure   CropHeader;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
constructor THTTP.Create;
begin
  inherited Create;
  Header:='';
end;

////////////////////////////////////////////////////////////////////////////////
procedure THTTP.Execute(Server : AnsiString; Port : Cardinal);
// connects to server, sends data, waits for and returns response
var
  Host : PHostEnt;
  Addr : PInAddr;
  Info : TSockAddrIn;
  S1   : Integer;
  s    : AnsiString;
  i    : Integer;
  t    : Cardinal;
begin
  Response:='';

  // find IP
  Host:=gethostbyname(PAnsiChar(Server));
  if Host=nil then Exit;
  Addr:=PInAddr(Host^.h_addr_list^);

  // initialize request
  ZeroMemory(@Info,SizeOf(Info));
  Info.sin_family:=AF_INET;
  Info.sin_port:=htons(Port);
  Info.sin_addr:=Addr^;

  // send request
  S1:=socket(AF_INET,SOCK_STREAM,0);
  if S1=INVALID_SOCKET then Exit;
  try
    if connect(S1,Info,SizeOf(Info))<>0 then Exit;
    if send(S1,Request[1],Length(Request),0)=SOCKET_ERROR then Exit;

    // wait up to 5min for response
    t:=GetTickCount;
    repeat
      if GetTickCount-t>300000 then Exit;
      SetLength(s,4096);
      i:=recv(S1,s[1],4096,0);
      SetLength(s,i);
      Response:=Response+s;
    until i=0;

  finally
    closesocket(S1);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure THTTP.Get(Server,URI : AnsiString; Port : Cardinal = 80);
// sends GET request
begin
  Request:='GET '+URI+' HTTP/1.0'#13#10+
           'Host: '+Server+#13#10+
           Header;
  Request:=Trim(Request)+#13#10#13#10;
  Execute(Server,Port);
end;

////////////////////////////////////////////////////////////////////////////////
procedure THTTP.Post(Server,URI,Data : AnsiString; Port : Cardinal = 80);
// sends POST request
begin
  Request:='POST '+URI+' HTTP/1.0'#13#10+
           'Host: '+Server+#13#10+
           'Content-Length: '+IntToStr(Length(Data))+#13#10+
           Header;
  Request:=Trim(Request)+#13#10#13#10+Data;
  Execute(Server,Port);
end;

////////////////////////////////////////////////////////////////////////////////
procedure THTTP.CropHeader;
// removes header from response
var
  Cnt : Integer;
begin
  Cnt:=Pos(#13#10#13#10,Response);
  Delete(Response,1,Cnt+3);
end;

////////////////////////////////////////////////////////////////////////////////
var WSData : WSAData;
initialization
  WSAStartup(2,WSData);
finalization
  WSACleanUp;
end.
