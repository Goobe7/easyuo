unit uocommands;
interface
uses Windows, SysUtils, Messages, Classes, Registry, uovariables, uoselector,
     uoclidata, uotiles, global;

type
  TSkillEntry   = record
    Name        : AnsiString;
    Code        : Integer;
  end;
  TSkillTable   = array[0..57] of TSkillEntry;
  PSkillTable   = ^TSkillTable;

  TItemRes      = record
    ItemID      : Cardinal;
    ItemType    : Cardinal;
    ItemKind    : Integer;
    ContID      : Cardinal;
    ItemX       : Integer;
    ItemY       : Integer;
    ItemZ       : Integer;
    ItemStack   : Cardinal;
    ItemRep     : Cardinal;
    ItemCol     : Cardinal;
  end;
  PItemRes      = ^TItemRes;

  TDelayFunc    = function(Duration : Cardinal) : Boolean of object;
  TUOCmd        = class(TObject)
  private
    UOSel       : TUOSel;
    Cst         : TCstDB;
    UOVar       : TUOVar;
    TileObj     : TTTBasic;
    JournalList : TStringList;
    Delay       : TDelayFunc;
    IgnoreIDs   : TStringList;
    IgnoreLists : TStringList;
    IgnoreCnt   : Cardinal;
    ItemList    : TList;
    FilterList  : TList;
    FilterID    : TList;
    FilterType  : TList;
    FilterCont  : TList;
    FilterGnd   : Cardinal;
    FilterAllC  : Boolean;
    FilterAllG  : Boolean;
    procedure   RMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
    procedure   WMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
    function    StdDelay(Duration : Cardinal) : Boolean;
    procedure   ClearFilter;
    procedure   RefreshFilter;
  public
    ItemRes     : TItemRes;
    ItemCnt     : Cardinal;
    SkillReal   : Cardinal;
    SkillNorm   : Cardinal;
    SkillCap    : Cardinal;
    SkillLock   : Cardinal;
    JournalRef  : Cardinal;
    JournalCnt  : Cardinal;
    JournalStr  : AnsiString;
    JournalCol  : Cardinal;
    TileType    : Cardinal;
    TileZ       : Integer;
    TileName    : AnsiString;
    TileFlags   : Cardinal;
    ContKind    : Cardinal;
    ContName    : AnsiString;
    ContX       : Integer;
    ContY       : Integer;
    ContSX      : Cardinal;
    ContSY      : Cardinal;
    ContHP      : Cardinal;
    ContID      : Cardinal;
    ContType    : Cardinal;
    ShopPos     : Cardinal;
    ShopCnt     : Cardinal;
    ShopID      : Cardinal;
    ShopType    : Cardinal;
    ShopMax     : Cardinal;
    ShopPrice   : Cardinal;
    ShopName    : AnsiString;
    constructor Create(UOSelector : TUOSel; UOVariables : TUOVar; DFunc : TDelayFunc);
    procedure   Free;
    function    OpenClient(SwitchTo : Boolean) : Boolean;
    procedure   CloseClient;
    function    SetShopItem(ID,Quantity : Cardinal) : Boolean;
    function    GetShopInfo : Boolean;
    function    GetCont(Index : Cardinal) : Boolean;
    function    TileInit(NoOverrides : Boolean = False) : Boolean;
    function    TileCnt(X,Y : Cardinal; Facet : Integer = -1) : Cardinal;
    function    TileGet(X,Y,Layer : Cardinal; Facet : Integer = -1) : Boolean;
    procedure   GetJournal(Index : Cardinal);
    function    ScanJournal(OldRef : Cardinal) : Boolean;
    function    IgnoreItemReset(List : AnsiString = '') : Boolean;
    function    IgnoreItem(IDType : Cardinal; List : AnsiString = '') : Boolean;
    function    HideItem(ID : Cardinal) : Boolean;
    procedure   GetItem(Index : Cardinal);
    procedure   FilterItems(Filter : Cardinal; Value : Cardinal = $FFFFFFFF);
    procedure   ScanItems(VisibleOnly : Boolean = True);
    procedure   GetSkill(SkillStr : AnsiString);
    function    GetPix(X,Y : Cardinal) : Cardinal;
    procedure   Msg(Str : AnsiString);
    function    Move(X,Y,Acc,Timeout : Cardinal) : Boolean;
    procedure   Click(X,Y : Cardinal; Cnt : Cardinal = 1; Left : Boolean = True; Down : Boolean = True;
                Up : Boolean = True; Fast : Boolean = False; MC : Boolean = False);
    procedure   Key(KeyStr : AnsiString; Ctrl,Alt,Shift : Boolean);
  end;

  function SkillFind(A,Z : Integer; Name : AnsiString; Table : PSkillTable) : Integer;

var
  SkillList : TSkillTable = (
  (Name: 'alch'; Code: 000), (Name: 'anat'; Code: 001),
  (Name: 'anil'; Code: 002), (Name: 'anim'; Code: 035),
  (Name: 'arch'; Code: 031), (Name: 'arms'; Code: 004),
  (Name: 'begg'; Code: 006), (Name: 'blac'; Code: 007),
  (Name: 'bowc'; Code: 008), (Name: 'bush'; Code: 052),
  (Name: 'camp'; Code: 010), (Name: 'carp'; Code: 011),
  (Name: 'cart'; Code: 012), (Name: 'chiv'; Code: 051),
  (Name: 'cook'; Code: 013), (Name: 'dete'; Code: 014),
  (Name: 'disc'; Code: 015), (Name: 'eval'; Code: 016),
  (Name: 'fenc'; Code: 042), (Name: 'fish'; Code: 018),
  (Name: 'focu'; Code: 050), (Name: 'fore'; Code: 019),
  (Name: 'heal'; Code: 017), (Name: 'herd'; Code: 020),
  (Name: 'hidi'; Code: 021), (Name: 'imbu'; Code: 056),
  (Name: 'insc'; Code: 023), (Name: 'item'; Code: 003),
  (Name: 'lock'; Code: 024), (Name: 'lumb'; Code: 044),
  (Name: 'mace'; Code: 041), (Name: 'mage'; Code: 025),
  (Name: 'medi'; Code: 046), (Name: 'mini'; Code: 045),
  (Name: 'musi'; Code: 029), (Name: 'myst'; Code: 055),
  (Name: 'necr'; Code: 049), (Name: 'ninj'; Code: 053),
  (Name: 'parr'; Code: 005), (Name: 'peac'; Code: 009),
  (Name: 'pois'; Code: 030), (Name: 'prov'; Code: 022),
  (Name: 'remo'; Code: 048), (Name: 'resi'; Code: 026),
  (Name: 'snoo'; Code: 028), (Name: 'spel'; Code: 054),
  (Name: 'spir'; Code: 032), (Name: 'stea'; Code: 033),
  (Name: 'stlt'; Code: 047), (Name: 'swor'; Code: 040),
  (Name: 'tact'; Code: 027), (Name: 'tail'; Code: 034),
  (Name: 'tast'; Code: 036), (Name: 'thro'; Code: 057),
  (Name: 'tink'; Code: 037), (Name: 'trac'; Code: 038),
  (Name: 'vete'; Code: 039), (Name: 'wres'; Code: 043));

implementation

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.RMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
// read from memory of currently selected client
begin
  if UOSel.Nr=0 then ZeroMemory(Buf,Length)
  else ReadMem(UOSel.HProc,MemPos,Buf,Length);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.WMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
// write to memory of currently selected client
begin
  if UOSel.Nr>0 then
    WriteMem(UOSel.HProc,MemPos,Buf,Length);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.StdDelay(Duration : Cardinal) : Boolean;
begin
  Sleep(Duration);
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
constructor TUOCmd.Create(UOSelector : TUOSel; UOVariables : TUOVar; DFunc : TDelayFunc);
begin
  inherited Create;
  UOSel:=UOSelector;
  Cst:=UOSel.CstDB;
  UOVar:=UOVariables;
  TileObj:=nil;
  JournalList:=TStringList.Create;
  IgnoreIDs:=TStringList.Create;
  IgnoreLists:=TStringList.Create;
  ItemList:=TList.Create;
  FilterList:=TList.Create;
  FilterID:=TList.Create;
  FilterType:=TList.Create;
  FilterCont:=TList.Create;

  // assign standard delay function if none is provided
  if DFunc=nil then Delay:=@StdDelay
  else Delay:=DFunc;

  ZeroMemory(@ItemRes,SizeOf(ItemRes));
  ItemCnt:=0;
  FilterGnd:=$FFFFFFFF;
  FilterAllC:=False;
  FilterAllG:=False;
  IgnoreCnt:=0;
  SkillReal:=0;
  SkillNorm:=0;
  SkillCap:=0;
  SkillLock:=0;
  JournalRef:=0;
  JournalCnt:=0;
  JournalStr:='';
  JournalCol:=0;
  TileType:=0;
  TileZ:=0;
  TileName:='';
  TileFlags:=0;
  ContKind:=0;
  ContName:='';
  ContX:=0;
  ContY:=0;
  ContSX:=0;
  ContSY:=0;
  ContHP:=0;
  ContID:=0;
  ContType:=0;
  ShopPos:=0;
  ShopCnt:=0;
  ShopID:=0;
  ShopType:=0;
  ShopMax:=0;
  ShopPrice:=0;
  ShopName:='';
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.Free;
var
  i : Integer;
begin
  JournalList.Free;
  IgnoreIDs.Free;
  IgnoreLists.Free;
  for i:=0 to ItemList.Count-1 do
    Dispose(PItemRes(ItemList[i]));
  ItemList.Free;
  FilterList.Free;
  FilterID.Free;
  FilterType.Free;
  FilterCont.Free;
  inherited Free;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.OpenClient(SwitchTo : Boolean) : Boolean;
// launches a new client instance (optionally waits and switches to it)
type
  TScanStr = record
    Str    : AnsiString;
    Offs   : Integer;
    Joker  : AnsiChar;
  end;
const
  SplashRemover : TScanStr =
    (Str: #139#1#36#1#139#1#36#1#137#134#192#0; Offs: 9; Joker: #1);
  MultiClientWarning : TScanStr =
    (Str: #133#192#139#29#1#1#1#1#116#1#106#4#104; Offs: 0; Joker: #1);
var
  Reg   : TRegistry;
  Path  : AnsiString;
  sInfo : TStartupInfo;
  pInfo : TProcessInformation;
  PHnd  : Cardinal;
  PID   : Cardinal;
  c     : Cardinal;
  i,j   : Integer;
begin
  Result:=False;

  // If a running client is already selected, launch another one of the same
  // version (UOSel.ExePath) and save path in registry. Otherwise read path
  // from registry if available (from last successful launch). Otherwise give
  // up and return false. (Script/GUI should tell user to select a client!)

  Path:=UOSel.ExePath;
  Reg:=TRegistry.Create;
  Reg.OpenKey('\Software\EasyUO',True);
  try
    if Path='' then Path:=Reg.ReadString('ExePath')
    else Reg.WriteString('ExePath',Path);
  except
    Path:='';
  end;
  Reg.Free;
  if not FileExists(Path) then Exit;

  // initialize structures
  ZeroMemory(@sInfo,sizeof(sInfo));
  ZeroMemory(@pInfo,sizeof(pInfo));
  sInfo.cb:=sizeof(sInfo);
  sInfo.dwFlags:=STARTF_USESHOWWINDOW;
  sInfo.wShowWindow:=SW_SHOW;

  // launch process (in suspended mode)
  if not CreateProcess(
    {executable } PAnsiChar(Path),
    {cmdline    } nil,
    {secuattrib } nil,
    {threadattr } nil,
    {inherhandle} False,
    {creatflags } CREATE_SUSPENDED,
    {environment} nil,
    {currentdir } PAnsiChar(ExtractFilePath(Path)),
    {startupinfo} sInfo,
    {processinfo} pInfo) then Exit;

  // patch client process
  PHnd:=pInfo.HProcess;
  with SplashRemover do c:=SearchMem(PHnd,Str,Joker)+Offs;
  if c>$FF then WriteMem(PHnd,c,#158,1);
  with MultiClientWarning do c:=SearchMem(PHnd,Str,Joker)+Offs;
  if c>$FF then WriteMem(PHnd,c,#49,1);

  // resume process
  PID:=pInfo.dwProcessId;
  ResumeThread(pInfo.hThread);

  // no switching required?
  if not SwitchTo then
  begin
    Result:=True;
    Exit;
  end;

  // wait up to 10sec for client to open
  for i:=1 to 40 do
  begin
    if not Delay(250) then Exit; // (delay can be cancelled!)

    // compare PIDs of all open clients with expected PID
    for j:=1 to UOSel.Cnt do
    begin
      if UOSel.GetPID(j)<>PID then Continue;

      // select client and exit
      UOSel.SelectClient(j);
      Result:=True;
      Exit;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.CloseClient;
// terminates the currently selected client
begin
  SendMessage(UOSel.HWnd,WM_CLOSE,0,0);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.SetShopItem(ID,Quantity : Cardinal) : Boolean;
// sets the quantity of an item (needs to be on the bill) [LEGACY/OBSOLETE]
var
  s   : AnsiString;
  c,d : Cardinal;
begin
  Result:=False;

  // find shop container
  SetLength(s,9);
  c:=Cst.CONTPOS-Cst.BCONTNEXT;
  repeat
    RMem(c+Cst.BCONTNEXT,@c,4); // next container
    if c=0 then Exit;
    RMem(c+$08,@d,4);
    RMem(d,@s[1],9); // read contname
  until PAnsiChar(s)='bill gump';

  // get the first item on the bill
  RMem(c+Cst.BBILLFIRST,@c,4);
  if c=0 then Exit;

  // find correct item
  repeat
    RMem(c+$04,@d,4); // read ID
    if d=ID then Break;
    RMem(c+$0C,@c,4); // next item
    if c=0 then Exit;
  until False;

  // write quantity and exit
  WMem(c+$0A,@Quantity,2);
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.GetShopInfo : Boolean;
// queries topmost item in shopping list (use mouse to scroll) [LEGACY/OBSOLETE]
var
  Buf : array[0..255] of Byte;
  s   : AnsiString;
  c,d : Cardinal;
begin
  Result:=False;
  ShopPos:=0;
  ShopCnt:=0;
  ShopID:=0;
  ShopType:=0;
  ShopMax:=0;
  ShopPrice:=0;
  ShopName:='';

  // find shop container
  SetLength(s,9);
  c:=Cst.CONTPOS-Cst.BCONTNEXT;
  repeat
    RMem(c+Cst.BCONTNEXT,@c,4); // next container
    if c=0 then Exit;
    RMem(c+$08,@d,4);
    RMem(d,@s[1],9); // read contname
  until s='shop gump';

  // get the topmost visible item on the shopping list
  RMem(c+Cst.BSHOPCURRENT,@c,4);
  if c=0 then Exit;

  // measure distance to first item
  d:=c;
  repeat
    RMem(d+Cst.BSHOPNEXT+4,@d,4);
    Inc(ShopPos);
    Inc(ShopCnt);
  until d=0;

  // measure distance to last item
  d:=c;
  repeat
    RMem(d+Cst.BSHOPNEXT,@d,4);
    if d=0 then Break;
    Inc(ShopCnt);
  until False;

  // read values
  RMem(c,@Buf,SizeOf(Buf));
  ShopType:=PWord(@Buf[Cst.BITEMTYPE])^;
  ShopID:=PCardinal(@Buf[Cst.BITEMID])^;
  ShopMax:=PWord(@Buf[Cst.BITEMSTACK])^;
  ShopPrice:=PCardinal(@Buf[Cst.BITEMID+Cst.BSHOPPRICE])^;

  // read shopname
  d:=PCardinal(@Buf[Cst.BITEMID+Cst.BSHOPPRICE-4])^;
  RMem(d,@Buf,64);
  ShopName:=PAnsiChar(@Buf);

  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.GetCont(Index : Cardinal) : Boolean;
// returns info of any container (not just topmost) via ContXYZ variables
var
  c,d : Cardinal;
begin
  ContKind:=0;
  ContName:='';
  ContX:=0;
  ContY:=0;
  ContSX:=0;
  ContSY:=0;
  ContID:=0;
  ContType:=0;
  ContHP:=0;
  Result:=False;

  // find container or exit
  if not UOVar.GetContAddr(c,Index) then Exit;
  Result:=True;

  // read kind, pos and size
  RMem(c,@ContKind,2);
  RMem(c+Cst.BCONTX,@ContX,4);
  RMem(c+Cst.BCONTX+4,@ContY,4);
  RMem(c+Cst.BCONTSIZEX,@ContSX,4);
  RMem(c+Cst.BCONTSIZEX+4,@ContSY,4);

  // read contname
  RMem(c+$08,@d,4);
  SetLength(ContName,64);
  RMem(d,@ContName[1],64);
  ContName:=PAnsiChar(@ContName[1]);

  // read HP (if statbar)
  if ContName='status gump' then
  begin
    RMem(c+Cst.BCONTNEXT+$92,@ContHP,4); // own char?
    if ContHP shr 16 > 0 then // maxhp available?
      ContHP:=Word(ContHP)*100 div (ContHP shr 16); // HP * 100 div 25
  end;

  // is container associated with an object (e.g. bag, char)?
  RMem(c+Cst.BCONTITEM,@d,4);
  if d=0 then Exit;

  // if yes, get id and type
  RMem(d+Cst.BITEMID,@ContID,4);
  RMem(d+Cst.BITEMTYPE,@ContType,2);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.TileInit(NoOverrides : Boolean = False) : Boolean;
// initializes the tile subsystem
var
  Path : AnsiString;
begin
  Result:=False;

  // get client directory
  Path:=ExtractFilePath(UOSel.ExePath);
  if not DirectoryExists(Path) then Exit;

  // recreate tile object
  if TileObj<>nil then TileObj.Free;
  TileObj:=TTTBasic.init(Path,not NoOverrides,Cst);

  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.TileCnt(X,Y : Cardinal; Facet : Integer = -1) : Cardinal;
// get number of layers at specified tile position
var
  b : Byte;
begin
  Result:=0;
  if TileObj=nil then Exit; // tile subsystem initialized?
  if Facet<0 then Facet:=UOVar.CursKind; // get current facet if not specified
  if not TileObj.GetLayerCount(X,Y,Facet,b) then Exit; // get count
  Result:=b+1;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.TileGet(X,Y,Layer : Cardinal; Facet : Integer = -1) : Boolean;
// get tile info at specified position and layer
var
  w : Word;
  i : ShortInt;
begin
  repeat
    if TileObj=nil then Break; // tile subsystem initialized?
    if Facet<0 then Facet:=UOVar.CursKind; // get current facet if not specified
    if not TileObj.GetTileData(X,Y,Facet,Layer-1,w,i,TileName,TileFlags) then Break;

    // write data into variables and exit
    TileType:=w;
    TileZ:=i;
    Result:=True;
    Exit;
  until True;

  // reset info on failure
  TileType:=0;
  TileZ:=0;
  TileName:='';
  TileFlags:=0;
  Result:=False;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.GetJournal(Index : Cardinal);
// loads chosen entry into JournalStr/Col
begin
  JournalStr:='';
  JournalCol:=0;
  if Index>=JournalList.Count then Exit; // entry in list?
  JournalStr:=JournalList[Index];
  JournalCol:=Cardinal(JournalList.Objects[Index]);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.ScanJournal(OldRef : Cardinal) : Boolean;
// loads journal entries into internal list, use GetJournal to query
type
  TItem  = packed record
    Pos  : Cardinal;
    Col  : Cardinal;
    Kind : Byte;
    Fill : array[$09..$1B] of Byte;
    Next : Cardinal;
  end;
var
  Item : TItem;
  Buf  : array[0..$3FF] of Byte;
  i,j  : Integer;
begin
  Result:=False;
  JournalList.Clear;

  // read address of first entry (=JournalRef)
  RMem(Cst.JOURNALPTR,@Item.Next,4);
  JournalRef:=Item.Next;

  // Note: The implementation of ScanJournal has been changed many times due
  // to problems with consistency. At first, refs were stored internally (bad
  // for library support and client swapping). Passing refs to the user really
  // seems to be the best solution!

  i:=0;
  while Item.Next<>0 do
  begin

    // If OldRef is found, JournalCnt is limited to items up to that mark,
    // i.e. we pretend that there are no more entries even though the full
    // journal is actually read and available (usually 100 entries).

    if Item.Next=OldRef then
    begin
      JournalCnt:=i;
      Result:=True;
    end;

    // load entry into buffers
    RMem(Item.Next,@Item,SizeOf(Item));
    RMem(Item.Pos,@Buf,SizeOf(Buf));

    // remove unicode if necessary (should be changed for asian uo.dll support)
    Buf[$3FE]:=0;
    if Item.Kind=$12 then
      for j:=1 to $1FF do Buf[j]:=Buf[j*2];

    // add str/col to list
    JournalList.AddObject(PAnsiChar(@Buf),TObject(Item.Col));
    Inc(i);
  end;

  // return false if OldRef wasn't found (i.e. client may have missed some lines)
  if not Result then
    JournalCnt:=JournalList.Count;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.IgnoreItemReset(List : AnsiString = '') : Boolean;
// remove all ignored items (or those belonging to a certain List)
var
  i,j : Integer;
begin
  if List='' then
  begin
    IgnoreIDs.Clear;
    IgnoreLists.Clear;
    Result:=True;
    Exit;
  end;

  List:=LowerCase(List);
  Result:=IgnoreLists.Find(List,i);
  if not Result then Exit; // does List exist?

  // check each IDType if it shares an IgnoreCnt with the specified List
  for j:=IgnoreIDs.Count-1 downto 0 do
    if IgnoreIDs.Objects[j]=IgnoreLists.Objects[i] then
      IgnoreIDs.Delete(j); // remove IDType entry

  // remove List entry
  IgnoreLists.Delete(i);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.IgnoreItem(IDType : Cardinal; List : AnsiString = '') : Boolean;
// adds an ID or type to the list of ignored items (or removes it again)
var
  s   : AnsiString;
  i,j : Integer;
begin
  s:=IntToStr(IDType);
  List:=LowerCase(List);

  // IDType already in list? -> unignore!
  if IgnoreIDs.Find(s,i) then
  begin
    IgnoreIDs.Delete(i);
    Result:=False; // let the caller know that we did not actually ignore
    Exit;
  end;

  // IgnoreItem used to be a single TStringList with List stored in Strings and
  // IDType stored in Objects. This was later split into two separate lists
  // because Objects cannot be searched efficiently which slowed down not only
  // IgnoreItem but more importantly FindItem itself. Now, both List and IDType
  // are stored in their own lists in the easily searchable Strings property.
  // The two are connected by a shared IgnoreCnt counter in Objects.

  // get List entry (by adding a new one if absent)
  if not IgnoreLists.Find(List,j) then
  begin
    // add List/IgnoreCnt pair
    Inc(IgnoreCnt);
    IgnoreLists.InsertObject(j,List,TObject(IgnoreCnt));
  end;

  // add IDType/IgnoreCnt pair
  IgnoreIDs.InsertObject(i,s,IgnoreLists.Objects[j]);
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.HideItem(ID : Cardinal) : Boolean;
// deletes an item from memory
var
  Buf : array[0..3] of Cardinal;
  c   : Cardinal;
begin
  Result:=False;
  RMem(Cst.CHARPTR,@c,4);
  repeat
    RMem(c+Cst.BITEMID,@Buf,16);
    if Buf[0]=ID then Break;
    c:=Buf[3];
    if c=0 then Exit;
  until False;

  // Setting an item's coordinates to 0/0 removes it from the nearby area
  // and the client unloads it from memory a moment later (normal process).

  WMem(c+$24,#0#0#0#0,4); // set to 0/0
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.GetItem(Index : Cardinal);
// loads chosen item into ItemRes
begin
  if Index>=FilterList.Count then
  begin
    ZeroMemory(@ItemRes,SizeOf(ItemRes));
    ItemRes.ItemKind:=-1;
  end
  else ItemRes:=TItemRes(FilterList[Index]^);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.ClearFilter;
// clears all filter conditions
begin
  FilterID.Clear;
  FilterType.Clear;
  FilterCont.Clear;
  FilterGnd:=$FFFFFFFF;
  FilterAllC:=False;
  FilterAllG:=False;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.RefreshFilter;
var
  i    : Integer;
  f1   : Boolean;
  f2   : Boolean;
  p    : PItemRes;
  a,b  : Cardinal;
  x,y  : Cardinal;
begin
  FilterList.Clear;
  x:=UOVar.CharPosX;
  y:=UOVar.CharPosY;

  // There are two separate filters f1 and f2 that decide if an item passes
  // (both true) or gets dropped (at least one false).

  for i:=0 to ItemList.Count-1 do // foreach item
  begin
    p:=ItemList[i];

    // both filters pass by default (in case there are no conditions)
    f1:=True;
    f2:=True;

    // filter by ID or type
    if (FilterID.Count>0) or (FilterType.Count>0) then
    begin
      f1:=False; // drop by default since there is at least one condition
      if FilterID.IndexOf(Pointer(p^.ItemID))>=0 then f1:=True; // check id
      if FilterType.IndexOf(Pointer(p^.ItemType))>=0 then f1:=True; // check type
    end;

    // filter by container or range
    if (FilterCont.Count>0) or (FilterGnd<$FFFFFFFF) or FilterAllC or FilterAllG then
    begin
      f2:=False; // drop by default since there is at least one condition

      // filter by container
      if p^.ItemKind=0 then
      begin
        if FilterAllC then f2:=True // check container
        else if FilterCont.IndexOf(Pointer(p^.ContID))>=0 then f2:=True;
      end;

      // filter by range
      if p^.ItemKind=1 then
      begin
        if FilterGnd<$FFFFFFFF then
        begin
          a:=Abs(p^.ItemX - x);
          b:=Abs(p^.ItemY - y);
          if b>a then a:=b; // calculate range
          if a<=FilterGnd then f2:=True; // check range
        end;
        if FilterAllG then f2:=True;
      end;
    end;

    // if both filters agree, add item to list
    if f1 and f2 then FilterList.Add(p);
  end;

  // clear ItemRes and update ItemCnt
  ZeroMemory(@ItemRes,SizeOf(ItemRes));
  ItemCnt:=FilterList.Count;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.FilterItems(Filter : Cardinal; Value : Cardinal = $FFFFFFFF);
// adds a condition to the filter set
const
  CLEAR     = 0;
  ITEMID    = 1;
  ITEMTYPE  = 2;
  CONTAINER = 3;
  GROUND    = 4;
begin
  // add new filter condition
  case Filter of
    CLEAR     : ClearFilter;
    ITEMID    : if Value<$FFFFFFFF then FilterID.Add(Pointer(Value));
    ITEMTYPE  : if Value<$FFFFFFFF then FilterType.Add(Pointer(Value));
    CONTAINER : if Value<$FFFFFFFF then FilterCont.Add(Pointer(Value))
                else FilterAllC:=True;
    GROUND    : if Value<$FFFFFFFF then FilterGnd:=Value
                else FilterAllG:=True;
  end;
  RefreshFilter; // apply!
end;

////////////////////////////////////////////////////////////////////////////////
function ListCompare(i1,i2 : Pointer) : Integer;
begin
  Result:=0;
  if PItemRes(i1)^.ItemID<PItemRes(i2)^.ItemID then Result:=+1;
  if PItemRes(i1)^.ItemID>PItemRes(i2)^.ItemID then Result:=-1;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.ScanItems(VisibleOnly : Boolean = True);
// loads client items into internal list, use GetItem to query
var
  Buf     : array[0..8191] of Byte;
  Dat     : TItemRes;
  PDat    : PItemRes;
  Addr    : Cardinal;
  i       : Integer;
  c       : Cardinal;
begin
  // clear item-list
  for i:=0 to ItemList.Count-1 do
    Dispose(PItemRes(ItemList[i]));
  ItemList.Clear;

  RMem(Cst.CHARPTR,@Addr,4);
  while Addr>0 do
  begin
    RMem(Addr,@Buf,Cst.BITEMID+Cst.BFINDREP+1); // load item into buffer
    Addr:=PCardinal(@Buf[Cst.BITEMID+$0C])^; // prepare next

    // fill values into structure
    Dat.ItemID:=PCardinal(@Buf[Cst.BITEMID])^;
    Dat.ItemType:=PWord(@Buf[Cst.BITEMTYPE])^;
    Dat.ItemKind:=Buf[$20];
    Dat.ItemX:=PSmallInt(@Buf[$24])^;
    Dat.ItemY:=PSmallInt(@Buf[$26])^;
    Dat.ItemZ:=PSmallInt(@Buf[$28])^;
    Dat.ItemStack:=PWord(@Buf[Cst.BITEMSTACK])^;
    Dat.ItemRep:=Buf[Cst.BITEMID+Cst.BFINDREP];
    Dat.ItemCol:=PWord(@Buf[Cst.BITEMSTACK+2])^;

    // discard if ItemID or ItemType is ignored
    if IgnoreIDs.Find(IntToStr(Dat.ItemID),i) then Continue;
    if IgnoreIDs.Find(IntToStr(Dat.ItemType),i) then Continue;

    Dat.ContID:=0;
    if Dat.ItemKind=0 then // item in bag?
    begin
      Dat.ItemZ:=0;

      // load container into buffer
      c:=PCardinal(@Buf[Cst.BITEMID+$04])^;
      if c=0 then Continue; // item must be in bag, error?
      RMem(c,@Buf,Cst.BITEMID+Cst.BGUMPPTR+4);

      // read ContID
      Dat.ContID:=PCardinal(@Buf[Cst.BITEMID])^;

      // is gump of container visible/open?
      c:=PCardinal(@Buf[Cst.BITEMID+Cst.BGUMPPTR])^;
      if c>0 then
      begin
        // turn relative item x/y into absolute by adding container x/y
        RMem(c+Cst.BCONTX,@Buf,8);
        Dat.ItemX:=Dat.ItemX + PInteger(@Buf[0])^;
        Dat.ItemY:=Dat.ItemY + PInteger(@Buf[4])^;
      end
      else begin

        // Some items can be found in memory even though they are not directly
        // visible on screen, e.g. items in your recently closed backpack or
        // bank box. Items on your char (clothes, weapons, even hair and
        // moustache) can also be found without your paperdoll open. This may be
        // counterintuitive as scripters expect items to disappear once the bag
        // that contains them is closed.

        if VisibleOnly then Continue; // discard if VisibleOnly
        Dat.ItemX:=0;
        Dat.ItemY:=0;
      end;
    end;

    // add new entry to list
    New(PDat);
    PDat^:=Dat;
    ItemList.Add(PDat);
  end;
  ItemList.Sort(@ListCompare); // sort list

  // ItemList now contains all items that were found during the scan. There is
  // also a second list (initialized below) named FilterList that contains items
  // after filtering. It needs to be separate to allow undoing a filter without
  // losing the previously discarded items. Note: GetItem reads from FilterList.

  ClearFilter;
  RefreshFilter;
end;

////////////////////////////////////////////////////////////////////////////////
function SkillFind(A,Z : Integer; Name : AnsiString; Table : PSkillTable) : Integer;
// quickly finds entry using binary search
var
  M,i : Integer;
begin
  Result:=-1;
  if (Z<A) then Exit;
  M:=A+(Z-A)shr 1;
  i:=CompareStr(Table^[M].Name,Name);
  if i>0 then Result:=SkillFind(A,M-1,Name,Table)
  else if i<0 then Result:=SkillFind(M+1,Z,Name,Table)
  else Result:=M;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.GetSkill(SkillStr : AnsiString);
var
  i : Integer;
begin
  // initialize values
  SkillReal:=0;
  SkillNorm:=0;
  SkillCap:=0;
  SkillLock:=0;

  // get skill-code
  i:=SkillFind(0,High(SkillList),Copy(LowerCase(SkillStr),1,4),@SkillList);
  if i<0 then Exit;
  i:=SkillList[i].Code;

  // read values
  RMem(Cst.SKILLSPOS+2*i,@SkillReal,2);
  RMem(Cst.SKILLSPOS+2*i+Cst.BSKILLDIST,@SkillNorm,2);
  RMem(Cst.SKILLCAPS+2*i,@SkillCap,2);
  RMem(Cst.SKILLLOCK+i,@SkillLock,1);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.GetPix(X,Y : Cardinal) : Cardinal;
// reads a single pixel
var
  DC : HDC;
begin
  DC:=GetDC(UOSel.HWnd);
  Result:=GetPixel(DC,X,Y);
  ReleaseDC(UOSel.HWnd,DC);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.Msg(Str : AnsiString);
// sends a string to the client
var
  i : Integer;
begin
  for i:=1 to Length(Str) do
    PostMessage(UOSel.HWnd,WM_CHAR,Integer(Str[i]),0);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOCmd.Move(X,Y,Acc,Timeout : Cardinal) : Boolean;
// moves char to specified coordinates
const
  Keys : array[0..7] of Integer = (VK_PRIOR,VK_RIGHT,VK_NEXT,
  VK_DOWN,VK_END,VK_LEFT,VK_HOME,VK_UP);
var
  Dir     : Cardinal;
  Speed   : Cardinal;
  Dist    : Cardinal;
  Nearest : Cardinal;
  dx,dy   : Integer;
  b       : Boolean;
  i,j     : Integer;
  c       : Cardinal;
  GTC     : Int64;
begin
  Result:=False;
  Nearest:=$FFFFFFFF;
  Dir:=0;
  b:=False;
  i:=0;
  c:=0;

  // initialize timeout in ms (0 = infinite)
  GTC:=GetTickCount;
  Dec(TimeOut);

  repeat
    if GetTickCount-GTC>TimeOut then Exit;

    // calculate delta values and distance
    dx:=X-UOVar.CharPosX;
    dy:=Y-UOVar.CharPosY;
    Dist:=Abs(dy);
    if Abs(dx)>Abs(dy) then Dist:=Abs(dx);

    // goal reached?
    if Dist<=Acc then
    begin
      if Acc>1 then Break; // accept right away if inaccurate

      // The problem with high accuracy is that the char sometimes overshoots
      // the target area, so we need to recheck after waiting a small delay.

      if b then Break;
      if not Delay(250) then Exit; // (delay can be cancelled!)
      b:=True;
      Continue;
    end;
    b:=False;

    // assume 0 if between -1 and 1 to prevent oscillation
    if Dist>1 then
    begin
      if Abs(dx)<=1 then dx:=0;
      if Abs(dy)<=1 then dy:=0;
    end;

    // calculate direction
    if (dx=0) and (dy<0) then Dir:=0;
    if (dx>0) and (dy<0) then Dir:=1;
    if (dx>0) and (dy=0) then Dir:=2;
    if (dx>0) and (dy>0) then Dir:=3;
    if (dx=0) and (dy>0) then Dir:=4;
    if (dx<0) and (dy>0) then Dir:=5;
    if (dx<0) and (dy=0) then Dir:=6;
    if (dx<0) and (dy<0) then Dir:=7;

    // slow down when getting close
    Speed:=50;
    if Dist<6 then Speed:=250;
    if Dist<3 then Speed:=400;

    // reset anti-block on progress
    if Dist<Nearest then
    begin
      Nearest:=Dist;
      i:=0;
      c:=0;
    end;
    Inc(i);

    // anti-block code
    if i>20 then
    begin
      if c>20 then Exit; // give up if we're hopelessly stuck
      c:=c+2;
      Dir:=(Dir + 4*Random(2)-2) mod 8; // try sideways
      Speed:=100;
      i:=10; // give it some time
    end;

    // send keys
    for j:=1 to 1+c do
    begin
      PostMessage(UOSel.HWnd,WM_KEYDOWN,Keys[Dir],$80000001);
      if not Delay(Speed) then Exit; // (delay can be cancelled!)
    end;

  until False;
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.Click(X,Y : Cardinal; Cnt : Cardinal = 1; Left : Boolean = True;
  Down : Boolean = True; Up : Boolean = True; Fast : Boolean = False; MC : Boolean = False);
// sends click to client
var
  Point1  : Cardinal;
  Point2  : TPoint;
  MsgDown : Cardinal;
  Cnt2    : Cardinal;
begin
  // initialize coordinates
  Point1:=(Y shl 16)+X;
  Point2.X:=X;
  Point2.Y:=Y;
  CLIENTTOSCREEN(UOSel.HWnd,Point2);

  // set button
  if Left then MsgDown:=WM_LBUTTONDOWN
  else MsgDown:=WM_RBUTTONDOWN;

  // move cursor
  if MC then SetCursorPos(Point2.X,Point2.Y);

  // send clicks
  for Cnt2:=1 to Cnt do
  begin
    if not Fast then
      if not Delay(150) then Exit; // (delay can be cancelled!)
    if Down then SendMessage(UOSel.HWnd,MsgDown,0,Point1);
    if Up then SendMessage(UOSel.HWnd,MsgDown+1,0,Point1); // +1 = MsgUp!
  end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOCmd.Key(KeyStr : AnsiString; Ctrl,Alt,Shift : Boolean);
// sends key to client
var
  VK       : Cardinal;
  MC,MA,MS : Boolean;
begin
  VK:=GetVK(KeyStr);
  if VK=0 then Exit;

  // use sendmessage in simple cases
  if not (Ctrl or Alt or Shift) then
  begin
    PostMessage(UOSel.HWnd,WM_KEYDOWN,VK,0);
    Exit;
  end;

  // try to bring target window to foreground
  ShowWindow(UOSel.HWnd,SW_SHOW);
  SetForegroundWindow(UOSel.HWnd);

  // get current state of modifier keys
  MC:=GetAsyncKeyState(VK_CONTROL) and $80>0;
  MA:=GetAsyncKeyState(VK_MENU) and $80>0;
  MS:=GetAsyncKeyState(VK_SHIFT) and $80>0;

  // press modifier keys if necessary (unless already pressed by real user)
  if Ctrl<>MC then keybd_event(VK_CONTROL,0,Byte(MC)*KEYEVENTF_KEYUP,0);
  if Alt<>MA then keybd_event(VK_MENU,0,Byte(MA)*KEYEVENTF_KEYUP,0);
  if Shift<>MS then keybd_event(VK_SHIFT,0,Byte(MS)*KEYEVENTF_KEYUP,0);

  // press key
  keybd_event(VK,0,0,0);
  keybd_event(VK,0,KEYEVENTF_KEYUP,0);

  // release modifier keys if necessary
  if Ctrl<>MC then keybd_event(VK_CONTROL,0,Byte(Ctrl)*KEYEVENTF_KEYUP,0);
  if Alt<>MA then keybd_event(VK_MENU,0,Byte(Alt)*KEYEVENTF_KEYUP,0);
  if Shift<>MS then keybd_event(VK_SHIFT,0,Byte(Shift)*KEYEVENTF_KEYUP,0);
end;

////////////////////////////////////////////////////////////////////////////////
initialization
  Randomize; // initialize random seed
end.
