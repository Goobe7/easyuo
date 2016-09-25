unit commands;
interface
uses SysUtils, Windows, ShellApi, MMSystem, Classes, global;

// This unit contains general commands that are useful for scripting in any language.

type
  TDelayFunc    = function(Duration : Cardinal) : Boolean of object;
  TCmd          = class(TObject)
  private
    Delay       : TDelayFunc;
    function    StdDelay(Duration : Cardinal) : Boolean;
  public
    constructor Create(DFunc : TDelayFunc);
    procedure   Wait(Duration, Rnd : Cardinal);
    function    OnHotkey(KeyStr : AnsiString; Ctrl,Alt,Shift : Boolean) : Boolean;
    procedure   Shutdown(Force : Boolean = False);
    function    Execute(App,Par : AnsiString; Show : Boolean; TimeOut : Cardinal = 0) : Boolean;
    procedure   Sound(SndFile : AnsiString = '');
    function    GetPix(Wnd : Cardinal; X,Y : Cardinal) : Cardinal;
    function    GetCliTitle(Wnd : Cardinal) : AnsiString;
    procedure   SetCliTitle(Wnd : Cardinal; Title : AnsiString);
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
function TCmd.StdDelay(Duration : Cardinal) : Boolean;
begin
  Sleep(Duration);
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
constructor TCmd.Create(DFunc : TDelayFunc);
begin
  inherited Create;

  // assign standard delay function if none is provided
  if DFunc=nil then Delay:=@StdDelay
  else Delay:=DFunc;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TCmd.Wait(Duration, Rnd : Cardinal);
// waits a specified amount of time
begin
  Delay(Duration+Random(Rnd));
end;

////////////////////////////////////////////////////////////////////////////////
function TCmd.OnHotkey(KeyStr : AnsiString; Ctrl,Alt,Shift : Boolean) : Boolean;
// returns true if the specified key combination is pressed
var
  VK : Cardinal;
begin
  Result:=False;
  VK:=GetVK(KeyStr);
  if VK=0 then Exit;

  // check individual keys
  if (Hi(GetAsyncKeyState(VK))<128) then Exit;
  if Ctrl xor (Hi(GetAsyncKeyState(VK_CONTROL))>127) then Exit;
  if Alt xor (Hi(GetAsyncKeyState(VK_MENU))>127) then Exit;
  if Shift xor (Hi(GetAsyncKeyState(VK_SHIFT))>127) then Exit;

  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TCmd.Execute(App,Par : AnsiString; Show : Boolean; TimeOut : Cardinal = 0) : Boolean;
// executes a file and waits for completion or until a timeout is reached
var
  Info : ShellExecuteInfoA;
begin
  Info.cbSize:=SizeOf(ShellExecuteInfoA);
  Info.fMask:=SEE_MASK_DOENVSUBST or SEE_MASK_NOCLOSEPROCESS;
  Info.Wnd:=0;
  Info.lpVerb:='open';
  Info.lpFile:=PAnsiChar(App);
  Info.lpParameters:=PAnsiChar(Par);
  Info.lpDirectory:='';
  Info.nShow:=SW_SHOW*Byte(Show)+SW_HIDE*Byte(not Show);
  ShellExecuteExA(@Info);

  // exit on failure
  Result:=Info.hInstApp>32;
  if not Result then Exit;

  // wait for completion or timeout
  if TimeOut>0 then
    Result:=WaitForSingleObject(Info.hProcess,TimeOut)<>WAIT_FAILED;

  CloseHandle(Info.hProcess);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TCmd.Shutdown(Force : Boolean = False);
// shuts the computer down, even in the presence of unsaved files if forced
var
  hTokenHandle : Cardinal;
  PrivLUID     : Int64;
  TokenPriv    : TOKEN_PRIVILEGES;
  tkpDummy     : TOKEN_PRIVILEGES;
  lDummy       : Cardinal;
begin
  // adjust privileges
  OpenProcessToken(GetCurrentProcess,
    TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hTokenHandle);
  LookupPrivilegeValue('', 'SeShutdownPrivilege', PrivLUID);
  TokenPriv.PrivilegeCount:=1;
  TokenPriv.Privileges[0].Luid:=PrivLUID;
  TokenPriv.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
  AdjustTokenPrivileges(hTokenHandle, False, TokenPriv,
    SizeOf(tkpDummy), tkpDummy, lDummy);
  CloseHandle(hTokenHandle);

  // initiate shutdown
  ExitWindowsEx(EWX_FORCE*Byte(Force)+EWX_POWEROFF+EWX_SHUTDOWN, 0);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TCmd.Sound(SndFile : AnsiString = '');
// play a specific file or a simple beep
begin
  if SndFile='' then MessageBeep($FFFFFFFF)
  else SndPlaySound(@SndFile[1], SND_ASYNC or SND_NOSTOP or SND_NODEFAULT);
end;

////////////////////////////////////////////////////////////////////////////////
function TCmd.GetPix(Wnd : Cardinal; X,Y : Cardinal) : Cardinal;
// returns the value of a pixel on screen
var
  DC : HDC;
begin
  DC:=GetDC(Wnd);
  Result:=GetPixel(DC,X,Y);
  ReleaseDC(Wnd,DC);
end;

////////////////////////////////////////////////////////////////////////////////
function TCmd.GetCliTitle(Wnd : Cardinal) : AnsiString;
// returns the specified window's title
begin
  SetLength(Result,1024);
  GetWindowText(Wnd,@Result[1],1024);
  Result:=PAnsiChar(@Result[1]);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TCmd.SetCliTitle(Wnd : Cardinal; Title : AnsiString);
// sets the specified window's title
begin
  SetWindowText(Wnd,@Title[1]);
end;

////////////////////////////////////////////////////////////////////////////////
initialization
  Randomize;
end.
