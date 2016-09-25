unit uoversion;
interface
uses Windows, SysUtils, Classes, global;

  // This unit provides version information of client executables.

  function GetExePath(PHnd : Cardinal) : AnsiString;
  function ScanVer(WHnd : Cardinal) : AnsiString;

implementation

var
  PSHnd  : Cardinal;
  PSProc : function(PHnd, MHnd : Cardinal; FN : PAnsiChar; Size : Cardinal) : Cardinal; stdcall;

////////////////////////////////////////////////////////////////////////////////
function GetExePath(PHnd : Cardinal) : AnsiString;
// if you know the process handle, this function will give you the file path
var
  Buf : array[0..4095] of Byte;
begin
  Result:='';
  if PHnd=0 then Exit;
  if not Assigned(PSProc) then Exit;
  PSProc(PHnd,0,@Buf,SizeOf(Buf));
  Result:=PAnsiChar(@Buf);
end;

////////////////////////////////////////////////////////////////////////////////
function GetFileVer(FN : AnsiString) : AnsiString;
// reads version string from file info structure
var
  c    : Cardinal;
  s    : AnsiString;
  Info : PVSFixedFileInfo;
begin
  Result:='';

  // get size
  c:=GetFileVersionInfoSize(PAnsiChar(FN),c);
  if c=0 then Exit;

  // get info
  SetLength(s,c);
  GetFileVersionInfo(PAnsiChar(FN),0,c,@s[1]);
  VerQueryValue(@s[1],'\',Pointer(Info),c);
  if c<SizeOf(Info^) then Exit;

  // return version string
  Result:=IntToStr(HiWord(Info^.dwFileVersionMS))+'.'+
          IntToStr(LoWord(Info^.dwFileVersionMS))+'.'+
          IntToStr(HiWord(Info^.dwFileVersionLS))+'.'+
          IntToStr(LoWord(Info^.dwFileVersionLS));
end;

////////////////////////////////////////////////////////////////////////////////
function ReadScan1(PHnd : Cardinal) : AnsiString;
// detects version of clients 1.26.x - 2.0.x
const
  Scan1 = #199#134#152#0#0#0#255#255#255#255#161#1#1#1#1#80#141#76#36#36#104;
var
  c : Cardinal;
begin
  Result:='';
  c:=SearchMem(PHnd,Scan1,#1);
  if c=0 then Exit;

  ReadMem(PHnd, c+11, @c, 4);
  Result:=#0#0#0#0#0#0#0#0#0#0#0;
  ReadMem(PHnd, c+4, @Result[1], 10);
  Result:=PAnsiChar(Result);
end;

////////////////////////////////////////////////////////////////////////////////
function ReadScan2(PHnd : Cardinal) : AnsiString;
// detects version of clients 3.0.x - 5.0.6e
const
  Scan2 = #104#2#2#2#2#232#2#2#2#2#131#196#24#198#5#2#2#2#2#1#184;
var
  a : array[0..9] of Byte;
  c : Cardinal;
begin
  Result:='';
  c:=SearchMem(PHnd,Scan2,#2);
  if c=0 then Exit;

  ReadMem(PHnd, c-15, @a, 10);
  Result:=IntToStr(Byte(a[9]))+'.'+
          IntToStr(Byte(a[7]))+'.'+
          IntToStr(Byte(a[5]));
  ReadMem(PHnd, PCardinal(@a)^, @a, 10);
  Result:=Result+PAnsiChar(@a);
end;

////////////////////////////////////////////////////////////////////////////////
function ScanVer(WHnd : Cardinal) : AnsiString;
// returns the client version string
var
  PHnd : Cardinal;
begin
  Result:='';

  // get process handle
  GetWindowThreadProcessID(WHnd,PHnd);
  PHnd:=OpenProcess(PROCESS_ALL_ACCESS,False,PHnd);
  if PHnd=0 then Exit;

  repeat

    // use file version if client >= 6.0.0 (very fast)
    Result:=GetFileVer(GetExePath(PHnd));
    if Result<>'' then
      if Result[1]>='6' then Break;

    // Beware: Clients 5.0.4f - 5.0.6e also have file versions, but they
    // differ from the official 3 numbers + 1 letter scheme. So they actually
    // have two versions, one in code (correct) and one in file info (wrong).

    // detect clients 3.0.x - 5.0.6e
    Result:=ReadScan2(PHnd);
    if Result<>'' then Break;

    // detect clients 1.26.x - 2.0.x
    Result:=ReadScan1(PHnd);
    if Result<>'' then Break;

    // still nothing? must be clients 5.0.7.0 - 5.0.9.x
    Result:=GetFileVer(GetExePath(PHnd));

  until True;

  CloseHandle(PHnd);
end;

////////////////////////////////////////////////////////////////////////////////
initialization
  PSHnd:=LoadLibrary('psapi.dll');
  Pointer(PSProc):=GetProcAddress(PSHnd,'GetModuleFileNameExA');
finalization
  FreeLibrary(PSHnd);
end.
