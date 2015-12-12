unit uoselector;
interface
uses Windows, SysUtils, Classes, uoclidata, uoversion;

// TUOSel allows focusing on a specific client while still being aware of other
// clients along with their window and process handles and version strings.

type
  TUOSel        = class(TObject)
  private
    WHnd        : Cardinal;
    PHnd        : Cardinal;
    function    Refresh : Cardinal;
  public
    CstDB       : TCstDB;
    constructor Create;
    procedure   Free;
    function    GetTitle(Nr : Cardinal) : AnsiString;
    function    GetPID(Nr : Cardinal) : Cardinal;
    function    GetVer(Nr : Cardinal) : AnsiString;
    function    SelectClient(Nr : Cardinal; Version : AnsiString = '') : Boolean;
    function    Cnt : Cardinal;
    function    Nr : Cardinal;
    function    Ver : AnsiString;
    function    PID : Cardinal;
    function    ExePath : AnsiString;
    property    HWnd : Cardinal read WHnd;
    property    HProc : Cardinal read PHnd;
  end;

////////////////////////////////////////////////////////////////////////////////
implementation

type
  TStat       = (S_INIT, S_FAIL, S_READY);
  TRec        = record
    Wnd       : Cardinal;
    Ver       : AnsiString;
    Stat      : TStat;
  end;
  PRec        = ^TRec;

var
  WndList     : TStringList;   // list of client window handles (only ready)
  RecList     : TStringList;   // holds a TRec for each window (all clients)
  ObjList     : TList;         // list of all TUOSel objects
  CS          : TSimpleRWSync; // critical section object
  ThreadHnd   : Cardinal;
  FirstAccess : Boolean = True;

////////////////////////////////////////////////////////////////////////////////
procedure CheckFirstAccess(PHnd : Cardinal);
// displays an error message once if client memory cannot be accessed
begin
  if not FirstAccess then Exit;
  FirstAccess:=False;
  if PHnd=0 then MessageBox(0,PAnsiChar(
    'It appears that EasyUO cannot access the UO client.'+#13#10+
    'Please restart the program with administrative privileges!'+#13#10+
    'Hint: Right-click '+ExtractFileName(ParamStr(0))+' '+
    'and select "Run as administrator"'),
    'Administrative Rights Required',MB_ICONWARNING or MB_TASKMODAL);
end;

////////////////////////////////////////////////////////////////////////////////
function GetProcHandle(Wnd : Cardinal) : Cardinal;
// returns process handle of specified window
begin
  GetWindowThreadProcessID(Wnd,Result);
  if Result=0 then Exit;
  Result:=OpenProcess(PROCESS_ALL_ACCESS,False,Result);
  CheckFirstAccess(Result);
end;

////////////////////////////////////////////////////////////////////////////////
// TUOSel //////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TUOSel.Create;
begin
  inherited Create;
  CstDB:=TCstDB.Create;
  CS.BeginWrite;
  ObjList.Add(Self);
  CS.EndWrite;
  WHnd:=0;
  PHnd:=0;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOSel.Free;
begin
  CS.BeginWrite;
  ObjList.Remove(Self);
  CS.EndWrite;
  CstDB.Free;
  inherited Free;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOSel.GetTitle(Nr : Cardinal) : AnsiString;
// returns window title of specified client
var
  Buf : array[0..1023] of Byte;
begin
  CS.BeginRead;
  Result:='';
  if (Nr<=WndList.Count) and (Nr>0) then
  begin
    GetWindowText(Cardinal(WndList.Objects[Nr-1]),@Buf,1023);
    Result:=PAnsiChar(@Buf);
  end;
  CS.EndRead;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOSel.GetPID(Nr : Cardinal) : Cardinal;
// returns process ID of specified client
begin
  CS.BeginRead;
  Result:=0;
  if (Nr<=WndList.Count)and(Nr>0) then
    GetWindowThreadProcessID(Cardinal(WndList.Objects[Nr-1]),@Result);
  CS.EndRead;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOSel.GetVer(Nr : Cardinal) : AnsiString;
// returns version string of specified client
var
  Rec : PRec;
  i   : Integer;
begin
  CS.BeginRead;
  Result:='';
  if (Nr<=WndList.Count) and (Nr>0) then
    if RecList.Find(WndList[Nr-1],i) then
  begin
    Rec:=Pointer(RecList.Objects[i]);
    Result:=Rec^.Ver;
  end;
  CS.EndRead;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOSel.SelectClient(Nr : Cardinal; Version : AnsiString = '') : Boolean;
// selects a specific client to work on, also allows version override
var
  Rec : PRec;
  i   : Integer;
begin
  CS.BeginRead;
  repeat
    if (Nr>WndList.Count) or (Nr=0) then Break;
    if not RecList.Find(WndList[Nr-1],i) then Break;
    Rec:=Pointer(RecList.Objects[i]);

    // assign handles
    WHnd:=Cardinal(Rec^.Wnd);
    PHnd:=GetProcHandle(WHnd);
    if PHnd=0 then Break;

    // Override the version string in case someone purposely messed with a
    // client executable's version info as an anti-macroing measure. Also,
    // sometimes when the client is updated, the constants don't really
    // change at all (no real update, just artwork patches and version
    // increase) so you may get away with loading the previous client's
    // constants. Check variables and events to make sure everything works.

    // load constants
    if Version='' then CstDB.Update(Rec^.Ver)
    else CstDB.Update(Version);

    Result:=True;
    CS.EndRead;
    Exit;
  until True;
  CS.EndRead;

  // otherwise reset all values
  WHnd:=0;
  PHnd:=0;
  CstDB.Update('');
  Result:=False;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOSel.Cnt : Cardinal;
begin
  CS.BeginRead;
  Result:=WndList.Count;
  CS.EndRead;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOSel.Refresh : Cardinal;
// refreshes handles and returns client nr, does not use CS on purpose
var
  i : Integer;
begin
  Result:=0;
  if WndList.Find(IntToStr(WHnd),i) then
  begin
    Result:=i+1;
    Exit;
  end;
  if PHnd>0 then
    CloseHandle(PHnd);
  WHnd:=0;
  PHnd:=0;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOSel.Nr : Cardinal;
var
  i : Integer;
begin
  CS.BeginRead;
  Result:=Refresh;
  CS.EndRead;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOSel.Ver : AnsiString;
begin
  Result:=GetVer(Nr);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOSel.PID : Cardinal;
begin
  Result:=GetPID(Nr);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOSel.ExePath : AnsiString;
begin
  Result:=GetExePath(PHnd);
end;

////////////////////////////////////////////////////////////////////////////////
// Timer Thread ////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// The TUOSel class above is accessed by many different threads, each owning its
// own object. In contrast, the code below is used only by a single timer thread
// which keeps track of available client windows. Access on shared resources
// (i.e. lists) is synchronized using a critical section (CS).

function EnumCliWnd(Wnd : Cardinal; Obj : Integer) : LongBool; stdcall;
// adds all windows with classname "Ultima Online" to list Obj
var
  Buf  : array[1..14] of Byte;
begin
  GetClassName(Wnd,@Buf,14);
  if PAnsiChar(@Buf)='Ultima Online' then
    TStringList(Obj).AddObject(IntToStr(Wnd),TObject(Wnd));
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TimerProc;
// refreshes internal list of available clients once every 50ms
var
  List : TStringList;
  Rec  : PRec;
  i,j  : Integer;
  s    : AnsiString;
begin
  // scan all windows
  List:=TStringList.Create;
  List.Sorted:=True; // ensures consistent order, important!
  EnumWindows(@EnumCliWnd,Integer(List));

  // EnumWindows may be slow, so we try to use a CS as late as possible.

  CS.BeginWrite; // begin critical section
  WndList.Free;  // release old list
  WndList:=List; // replace with new list

  // add new windows to RecList
  for i:=0 to WndList.Count-1 do
  begin
    s:=WndList[i];
    if not RecList.Find(s,j) then
    begin
      New(Rec);
      Rec^.Wnd:=Cardinal(WndList.Objects[i]);
      Rec^.Ver:=ScanVer(Rec^.Wnd); // call scanver()
      Rec^.Stat:=S_READY;
      if Pos(' '+LowerCase(Rec^.Ver)+' ',SupportedCli)<1 then
        Rec^.Stat:=S_FAIL;
      RecList.AddObject(s,TObject(Rec));
    end;
  end;

  // delete invalid windows from RecList (e.g. closed meanwhile)
  for i:=RecList.Count-1 downto 0 do
  begin
    s:=RecList[i];
    if not WndList.Find(s,j) then
    begin
      Rec:=Pointer(RecList.Objects[i]);
      Dispose(Rec);
      RecList.Delete(i);
    end;
  end;

  // Both lists are synced now!

  for i:=0 to RecList.Count-1 do
  begin
    Rec:=Pointer(RecList.Objects[i]);
    if Rec^.Stat=S_READY then Continue;

    // ...
    // additional handler code can be placed here
    // (reserved for future use)
    // ...

    // remove unready clients from WndList
    // (i.e. I do not want to see what I cannot use)
    if WndList.Find(RecList[i],j) then
      WndList.Delete(j);
  end;

  // refresh all instances
  for i:=0 to ObjList.Count-1 do
    TUOSel(ObjList[i]).Refresh;

  CS.EndWrite;
end;

////////////////////////////////////////////////////////////////////////////////
function TimerThread(Parameter : Pointer) : Integer;
begin
  repeat
    Sleep(50);
    TimerProc;
  until False; // infinite loop
  Result:=0;
end;

////////////////////////////////////////////////////////////////////////////////
procedure Initialize;
begin
  WndList:=TStringList.Create;
  RecList:=TStringList.Create;
  ObjList:=TList.Create;
  RecList.Sorted:=True;

  // start timer thread
  CS:=TSimpleRWSync.Create;
  TimerProc; // perform first call right away
  ThreadHnd:=BeginThread(nil,0,@TimerThread,nil,0,ThreadHnd);
end;

////////////////////////////////////////////////////////////////////////////////
procedure Terminate;
var
  Rec : PRec;
begin
  TerminateThread(ThreadHnd,0); // kill timer
  CloseHandle(ThreadHnd);
  CS.Free;
  ObjList.Free;
  while RecList.Count>0 do
  begin
    Rec:=Pointer(RecList.Objects[0]);
    Dispose(Rec);
    RecList.Delete(0);
  end;
  RecList.Free;
  WndList.Free;
end;

////////////////////////////////////////////////////////////////////////////////
initialization
  Initialize;
finalization
  Terminate;
end.
