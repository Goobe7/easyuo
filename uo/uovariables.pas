unit uovariables;
interface
uses Windows, SysUtils, Classes, access, asmstring, uoselector, uoclidata;

type
  TUOVar        = class(TObject)
  private
    UOSel       : TUOSel;
    Cst         : TCstDB;
    AsmStr      : TAsmStr;
    ContList    : TStringList;
    BPID        : Cardinal;
    BPIDChar    : Cardinal;
    procedure   RMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
    procedure   WMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
    function    GetStat(out Addr : Cardinal) : Boolean;
    procedure   PrepareStr(var s : AnsiString);
    //////////////////////////////////////////////////////////////////////////
    function    ReadLShard : Cardinal;      procedure WriteLShard(Value : Cardinal);
    function    ReadContPosX : Integer;     procedure WriteContPosX(Value : Integer);
    function    ReadContPosY : Integer;     procedure WriteContPosY(Value : Integer);
    function    ReadNextCPosX : Integer;    procedure WriteNextCPosX(Value : Integer);
    function    ReadNextCPosY : Integer;    procedure WriteNextCPosY(Value : Integer);
    function    ReadTargCurs : Boolean;     procedure WriteTargCurs(Value : Boolean);
    function    ReadCliXRes : Cardinal;     procedure WriteCliXRes(Value : Cardinal);
    function    ReadCliYRes : Cardinal;     procedure WriteCliYRes(Value : Cardinal);
    function    ReadCliLeft : Cardinal;     procedure WriteCliLeft(Value : Cardinal);
    function    ReadCliTop : Cardinal;      procedure WriteCliTop(Value : Cardinal);
    function    ReadLastSpell : Cardinal;   procedure WriteLastSpell(Value : Cardinal);
    function    ReadLastSkill : Cardinal;   procedure WriteLastSkill(Value : Cardinal);
    function    ReadLTargetKind : Cardinal; procedure WriteLTargetKind(Value : Cardinal);
    function    ReadLTargetTile : Cardinal; procedure WriteLTargetTile(Value : Cardinal);
    function    ReadLTargetX : Integer;     procedure WriteLTargetX(Value : Integer);
    function    ReadLTargetY : Integer;     procedure WriteLTargetY(Value : Integer);
    function    ReadLTargetZ : Integer;     procedure WriteLTargetZ(Value : Integer);
    function    ReadLTargetID : Cardinal;   procedure WriteLTargetID(Value : Cardinal);
    function    ReadLHandID : Cardinal;     procedure WriteLHandID(Value : Cardinal);
    function    ReadRHandID : Cardinal;     procedure WriteRHandID(Value : Cardinal);
    function    ReadLObjectID : Cardinal;   procedure WriteLObjectID(Value : Cardinal);
    function    ReadCliTitle : AnsiString;  procedure WriteCliTitle(Value : AnsiString);
  public
    VarRes      : Boolean;
    constructor Create(UOSelector : TUOSel);
    procedure   Free;
    function    GetContAddr(out Addr : Cardinal; Index : Integer = 0) : Boolean;
    procedure   IgnoreCont(s : AnsiString); overload;
    procedure   IgnoreCont(c : Cardinal); overload;
    //////////////////////////////////////////////////////////////////////////
    function    CharPosX    : Integer;
    function    CharPosY    : Integer;
    function    CharPosZ    : Integer;
    function    CharID      : Cardinal;
    function    CharGhost   : Boolean;
    function    CharType    : Cardinal;
    function    CharStatus  : AnsiString;
    function    CharDir     : Cardinal;
    function    CharName    : AnsiString;
    function    Sex         : Integer;
    function    Str         : Integer;
    function    MaxWeight   : Integer;
    function    Dex         : Integer;
    function    Int         : Integer;
    function    Hits        : Integer;
    function    MaxHits     : Integer;
    function    Stamina     : Integer;
    function    MaxStam     : Integer;
    function    Mana        : Integer;
    function    MaxMana     : Integer;
    function    Gold        : Integer;
    function    Weight      : Integer;
    function    MaxStats    : Integer;
    function    AR          : Integer;
    function    Followers   : Integer;
    function    MaxFol      : Integer;
    function    FR          : Integer;
    function    CR          : Integer;
    function    PR          : Integer;
    function    ER          : Integer;
    function    Luck        : Integer;
    function    MaxDmg      : Integer;
    function    MinDmg      : Integer;
    function    TP          : Integer;
    function    Shard       : AnsiString;
    function    ContSizeX   : Cardinal;
    function    ContSizeY   : Cardinal;
    function    ContKind    : Cardinal;
    function    ContID      : Cardinal;
    function    ContType    : Cardinal;
    function    ContName    : AnsiString;
    function    CliLang     : AnsiString;
    function    CliVer      : AnsiString;
    function    CliLogged   : Boolean;
    function    CursKind    : Cardinal;
    function    EnemyHits   : Cardinal;
    function    EnemyID     : Cardinal;
    function    LObjectType : Cardinal;
    function    LLiftedID   : Cardinal;
    function    LLiftedType : Cardinal;
    function    LLiftedKind : Cardinal;
    function    SysMsg      : AnsiString;
    function    BackpackID  : Cardinal;
    function    CursorX     : Integer;
    function    CursorY     : Integer;
    //////////////////////////////////////////////////////////////////////////
    property    LShard      : Cardinal read ReadLShard write WriteLShard;
    property    ContPosX    : Integer read ReadContPosX write WriteContPosX;
    property    ContPosY    : Integer read ReadContPosY write WriteContPosY;
    property    NextCPosX   : Integer read ReadNextCPosX write WriteNextCPosX;
    property    NextCPosY   : Integer read ReadNextCPosY write WriteNextCPosY;
    property    TargCurs    : Boolean read ReadTargCurs write WriteTargCurs;
    property    CliXRes     : Cardinal read ReadCliXRes write WriteCliXRes;
    property    CliYRes     : Cardinal read ReadCliYRes write WriteCliYRes;
    property    CliLeft     : Cardinal read ReadCliLeft write WriteCliLeft;
    property    CliTop      : Cardinal read ReadCliTop write WriteCliTop;
    property    LSpell      : Cardinal read ReadLastSpell write WriteLastSpell;
    property    LSkill      : Cardinal read ReadLastSkill write WriteLastSkill;
    property    LTargetKind : Cardinal read ReadLTargetKind write WriteLTargetKind;
    property    LTargetTile : Cardinal read ReadLTargetTile write WriteLTargetTile;
    property    LTargetX    : Integer read ReadLTargetX write WriteLTargetX;
    property    LTargetY    : Integer read ReadLTargetY write WriteLTargetY;
    property    LTargetZ    : Integer read ReadLTargetZ write WriteLTargetZ;
    property    LTargetID   : Cardinal read ReadLTargetID write WriteLTargetID;
    property    LHandID     : Cardinal read ReadLHandID write WriteLHandID;
    property    RHandID     : Cardinal read ReadRHandID write WriteRHandID;
    property    LObjectID   : Cardinal read ReadLObjectID write WriteLObjectID;
    property    CliTitle    : AnsiString read ReadCliTitle write WriteCliTitle;
  end;

implementation
////////////////////////////////////////////////////////////////////////////////

const
  IgnoredConts = // needs to be in alphabetical order!
    'BARK_GUMP'#13#10+
    'DAMAGENUMBERS_GUMP'#13#10+
    'DUMB_GUMP'#13#10+
    'GAMEAREAEDGEGUMP'#13#10+
    'MAP_GUMP'#13#10+
    'MENUBAR'#13#10+
    'MISSILE_GUMP'#13#10+
    'NEW_ITEM_PROP_GUMP'#13#10+
    'RETICLE_GUMP'#13#10+
    'TARGET_GUMP'#13#10+
    'UNICODE_BARK_GUMP';

////////////////////////////////////////////////////////////////////////////////
constructor TUOVar.Create(UOSelector : TUOSel);
begin
  inherited Create;
  UOSel:=UOSelector;
  Cst:=UOSel.CstDB;
  AsmStr:=TAsmStr.Create;
  ContList:=TStringList.Create;
  ContList.Text:=IgnoredConts;
  BPIDChar:=$00FFFFFF;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOVar.Free;
begin
  ContList.Free;
  AsmStr.Free;
  inherited Free;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOVar.RMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
// read from memory of currently selected client
begin
  ZeroMemory(Buf,Length);
  if UOSel.Nr>0 then
    ReadMem(UOSel.HProc,MemPos,Buf,Length);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOVar.WMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
// write to memory of currently selected client
begin
  if UOSel.Nr>0 then
    WriteMem(UOSel.HProc,MemPos,Buf,Length);
end;

////////////////////////////////////////////////////////////////////////////////
// Container Management ////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function TUOVar.GetStat(out Addr : Cardinal) : Boolean;
// returns address of status bar structure (fails if closed)
begin
  RMem(Cst.CHARPTR,@Addr,4);
  RMem(Addr+Cst.BITEMID+$10,@Addr,4);
  Result:=Addr=0;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOVar.PrepareStr(var s : AnsiString);
// converts string into IgnoreCont format
var
  i : Integer;
begin
  s:=UpperCase(s);
  for i:=1 to Length(s) do
    if s[i]=' ' then s[i]:='_';
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.GetContAddr(out Addr : Cardinal; Index : Integer = 0) : Boolean;
// returns address of topmost container structure that is not ignored
// (or a container below the topmost if Index>0)
var
  i : Integer;
  c : Cardinal;
  s : AnsiString;
begin
  Result:=False;

  // Although CONTPOS points right to the first container entry, we subtract
  // BCONTNEXT initially because it gets added on each round.

  Addr:=Cst.CONTPOS-Cst.BCONTNEXT;
  repeat
    RMem(Addr+Cst.BCONTNEXT,@Addr,4); // load next entry
    if Addr=0 then Exit; // fail on reaching end of list

    // The very first 4 bytes of the structure (object) points to a list (class)
    // with handler functions. The first two bytes were chosen as ContKind
    // identifier. Pretty stupid since that changes frequently depending on the
    // compiler and code changes but it does identify the container quite well.

    RMem(Addr,@c,4); // get ContKind
    if c=0 then Exit;
    if ContList.Find(IntToStr(Word(c)),i) then Continue; // ContKind ignored?

    // get ContName string
    RMem(Addr+$08,@c,4);
    SetLength(s,64);
    RMem(c,@s[1],64);
    s:=PAnsiChar(s);

    PrepareStr(s);
    if ContList.Find(s,i) then Continue; // ContName ignored?

    Dec(Index);
  until Index<0;
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOVar.IgnoreCont(s : AnsiString);
// adds a ContName to the list of ignored gumps
var
  i : Integer;
begin
  PrepareStr(s);
  if not ContList.Find(s,i) then
    ContList.Insert(i,s);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOVar.IgnoreCont(c : Cardinal);
// adds a ContKind to the list of ignored gumps, or clears/resets the list
begin
  case c of
    $00000000: ContList.Clear;
    $FFFFFFFF: ContList.Text:=IgnoredConts;
    else IgnoreCont(IntToStr(c));
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Functions ///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Note: Many of the functions and properties below look fairly similar, an
// indication that their functionality could have been covered by a single
// generic handler reading variations (offsets, sizes, types etc.) from a table.
// But since we want to be able to access variables within Delphi code itself,
// e.g. for writing a hardcoded script, we cannot get rid of the boilerplate.
//
// Some of the functions below can fail, e.g. if an item in memory cannot be
// found. In that case, VarRes is used to signal failure. Be mindful of race
// conditions when using multiple threads (script/GUI) to access the same object.

function TUOVar.CharPosX : Integer;
var
  c : Cardinal;
begin
  RMem(Cst.CHARPTR,@c,4);
  RMem(c+$24,@c,2);
  Result:=SmallInt(c);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CharPosY : Integer;
var
  c : Cardinal;
begin
  RMem(Cst.CHARPTR,@c,4);
  RMem(c+$26,@c,2);
  Result:=SmallInt(c);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CharPosZ : Integer;
var
  c : Cardinal;
begin
  RMem(Cst.CHARPTR,@c,4);
  RMem(c+$28,@c,2);
  Result:=SmallInt(c);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CharID : Cardinal;
var
  c : Cardinal;
begin
  RMem(Cst.CHARPTR,@c,4);
  RMem(c+Cst.BITEMID,@c,4);
  Result:=c;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CharGhost : Boolean;
var
  c : Cardinal;
  w : Word;
begin
  RMem(Cst.CHARPTR,@c,4);
  RMem(c+Cst.BITEMTYPE,@w,2);
  Result:=(w=$0192) or (w=$0193) or (w=$025F) or (w=$0260); // male/female
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CharType : Cardinal;
var
  c : Cardinal;
begin
  RMem(Cst.CHARPTR,@c,4);
  RMem(c+Cst.BITEMTYPE,@c,2);
  Result:=Word(c);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CharStatus : AnsiString;
var
  c : Cardinal;
  w : Word;
  b : Byte;
  i : Integer;
begin
  Result:='';

  // get CharStatus
  RMem(Cst.CHARPTR,@c,4);
  RMem(c+Cst.BITEMID+Cst.BCHARSTATUS,@w,2);

  // apply correction for CharStat C (later clients require this)
  if Cst.FEXCHARSTATC>0 then
  begin
    RMem(c+Cst.BITEMID+Cst.BCHARSTATUS+Cst.FEXCHARSTATC,@b,1); // read byte
    if b=1 then w:=w or 4; // set C if byte is 1
  end;

  // split word into flag string
  for i:=0 to 15 do
    if w and (1 shl i)>0 then
      Result:=Chr(65+i)+Result;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CharDir : Cardinal;
begin
  Result:=0;
  RMem(Cst.CHARDIR,@Result,1);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CharName : AnsiString;
var
  c : Cardinal;
  s : AnsiString;
begin
  VarRes:=False;
  Result:='';
  if GetStat(c) then Exit;
  SetLength(s,32);
  RMem(c+Cst.BSTATNAME,@s[1],32);
  Result:=PAnsiChar(s);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Sex : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=-1;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+00,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Str : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+02,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.MaxWeight : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+02,@c,2);
  Result:=((7*SmallInt(c)) div 2)+40; // apply formula
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Dex : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+04,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Int : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+06,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Hits : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+08,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.MaxHits : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+10,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Stamina : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+12,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.MaxStam : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+14,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Mana : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+16,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.MaxMana : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+18,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Gold : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+$1E+22,@Result,4);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Weight : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+Cst.BSTATWEIGHT,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.MaxStats : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit; // very old clients do not have extended stats
  if GetStat(c) then Exit;

  // Mondain's Legacy clients added 4 bytes in the middle, so we adjust
  // accordingly using BSTATML without breaking old clients.

  RMem(c+Cst.BSTATNAME+Cst.BSTATML+$1E+30,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.AR : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+Cst.BSTATML+Cst.BSTATAR,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Followers : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+Cst.BSTATML+$1E+32,@c,1);
  Result:=ShortInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.MaxFol : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+Cst.BSTATML+$1E+33,@c,1);
  Result:=ShortInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.FR : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+Cst.BSTATML+$1E+34,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CR : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+Cst.BSTATML+$1E+36,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.PR : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+Cst.BSTATML+$1E+38,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ER : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+Cst.BSTATML+$1E+40,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Luck : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit;
  if GetStat(c) then Exit;

  // yet another adjustment was necessary using BSTAT1
  RMem(c+Cst.BSTATNAME+Cst.BSTATML+Cst.BSTAT1+$1E+42,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.MaxDmg : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+Cst.BSTATML+Cst.BSTAT1+$1E+44,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.MinDmg : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit;
  if GetStat(c) then Exit;
  RMem(c+Cst.BSTATNAME+Cst.BSTATML+Cst.BSTAT1+$1E+46,@c,2);
  Result:=SmallInt(c);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.TP : Integer;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if Cst.FEXTSTAT<>1 then Exit;
  RMem(Cst.NEXTCPOS+Cst.BTITHE,@c,4);
  Result:=c;
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.Shard : AnsiString;
var
  s : AnsiString;
begin
  SetLength(s,32);
  RMem(Cst.SHARDPOS,@s[1],32);
  Result:=PAnsiChar(s);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ContSizeX : Cardinal;
var
  c : Cardinal;
begin
  Result:=0;
  if GetContAddr(c) then
    RMem(c+Cst.BCONTSIZEX,@Result,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ContSizeY : Cardinal;
var
  c : Cardinal;
begin
  Result:=0;
  if GetContAddr(c) then
    RMem(c+Cst.BCONTSIZEX+4,@Result,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ContKind : Cardinal;
var
  c : Cardinal;
begin
  Result:=0;
  if GetContAddr(c) then
    RMem(c,@Result,2);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ContID : Cardinal;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if not GetContAddr(c) then Exit;

  // BCONTITEM in the container structure points to the item structure!

  RMem(c+Cst.BCONTITEM,@c,4);
  if c=0 then Exit;
  RMem(c+Cst.BITEMID,@Result,4);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ContType : Cardinal;
var
  c : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  if not GetContAddr(c) then Exit;
  RMem(c+Cst.BCONTITEM,@c,4);
  if c=0 then Exit;
  RMem(c+Cst.BITEMTYPE,@Result,2);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ContName : AnsiString;
var
  c : Cardinal;
  s : AnsiString;
begin
  Result:='';
  if not GetContAddr(c) then Exit;
  RMem(c+$08,@c,4); // get string address
  SetLength(s,64);
  RMem(c,@s[1],64);
  Result:=PAnsiChar(s);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CliLang : AnsiString;
begin
  SetLength(Result,3);
  RMem(Cst.LHANDID-Cst.BLANG,@Result[1],3);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CliVer : AnsiString;
begin
  Result:=UOSel.Ver;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CliLogged : Boolean;
var
  c : Cardinal;
begin
  RMem(Cst.CLILOGGED,@c,4);
  Result:=c=1;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CursKind : Cardinal;
begin
  Result:=0;
  RMem(Cst.CURSORKIND,@Result,1);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.EnemyHits : Cardinal;
var
  c,d : Cardinal;
begin
  VarRes:=False;
  Result:=0;
  RMem(Cst.ENEMYHITS,@c,4);
  if c=0 then Exit;
  RMem(c+4,@d,4);
  if d<>$FEEDBEEF then Exit;

  // FEEDBEEF is a mark usually found at offset 04 in item structures, so we
  // check that here to make sure the item is valid.

  RMem(c+Cst.BENEMYHPVAL,@c,2);
  Result:=Word(c) shl 2; // scale up to 100
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.EnemyID : Cardinal;
begin
  RMem(Cst.ENEMYID,@Result,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.LObjectType : Cardinal;
begin
  Result:=0;
  RMem(Cst.LHANDID+$08,@Result,2);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.LLiftedID : Cardinal;
begin
  RMem(Cst.LLIFTEDID,@Result,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.LLiftedType : Cardinal;
begin
  Result:=0;
  RMem(Cst.LLIFTEDID-Cst.BLLIFTEDTYPE,@Result,2);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.LLiftedKind : Cardinal;
begin
  RMem(Cst.LLIFTEDID+Cst.BLLIFTEDKIND,@Result,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.SysMsg : AnsiString;
var
  i : Integer;
  c : Cardinal;
  s : AnsiString;
begin
  VarRes:=False;
  Result:='';
  RMem(Cst.SYSMSG,@c,4);
  if c=0 then Exit;

  // read SysMsg string
  RMem(c+Cst.BSYSMSGSTR,@c,4);
  SetLength(s,1024);
  RMem(c,@s[1],1024);

  // remove unicode (should be changed for asian uo.dll support)
  if s[2]=#0 then
    for i:=512 downto 1 do
      Delete(s,i*2,1);

  Result:=PAnsiChar(s);
  VarRes:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.BackpackID : Cardinal;
var
  ID      : Cardinal;
  Addr    : Cardinal;
  c,d     : Cardinal;
  w       : Word;
  b       : Byte;
begin
  VarRes:=True;
  Result:=BPID;                 // read BPID from cache
  RMem(Cst.CHARPTR,@Addr,4);    // get char address
  RMem(Addr+Cst.BITEMID,@ID,4); // get CharID
  if ID=BPIDChar then Exit;     // same CharID? we're done!
  Result:=0;

  c:=Addr;
  repeat
    RMem(c+Cst.BITEMID+$0C,@c,4); // load next address
    if c=0 then Break;
    RMem(c+Cst.BITEMID+$04,@d,4); // get container
    if d<>Addr then Continue; // is char direct parent of item? skip otherwise!

    RMem(c+Cst.BITEMTYPE,@w,2);
    if (w<>3701) and (w<>2482) then Continue; // item must be of type backpack
    RMem(c+Cst.BITEMID+Cst.BITEMSLOT,@b,1);
    if b<>21 then Continue; // item must fit in slot 21

    RMem(c+Cst.BITEMID,@Result,4);
    BPID:=Result; // cache BPID
    BPIDChar:=ID; // cache CharID
    Exit;
  until False;
  VarRes:=False;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CursorX : Integer;
var
  Pos : TPoint;
begin
  GetCursorPos(Pos);
  ScreenToClient(UOSel.HWnd,Pos);
  if Pos.X>9999 then Result:=-1
  else Result:=Pos.X;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.CursorY : Integer;
var
  Pos : TPoint;
begin
  GetCursorPos(Pos);
  ScreenToClient(UOSel.HWnd,Pos);
  if Pos.Y>9999 then Result:=-1
  else Result:=Pos.Y;
end;

////////////////////////////////////////////////////////////////////////////////
// Properties //////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function TUOVar.ReadLShard : Cardinal;
var
  c : Cardinal;
begin
  RMem(Cst.LSHARD,@c,4);
  Result:=c;
end;

procedure TUOVar.WriteLShard(Value : Cardinal);
begin
  WMem(Cst.LSHARD,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadContPosX : Integer;
var
  c : Cardinal;
begin
  Result:=0;
  if GetContAddr(c) then
    RMem(c+Cst.BCONTX,@Result,4);
end;

procedure TUOVar.WriteContPosX(Value : Integer);
var
  c : Cardinal;
begin
  if GetContAddr(c) then
    WMem(c+Cst.BCONTX,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadContPosY : Integer;
var
  c : Cardinal;
begin
  Result:=0;
  if GetContAddr(c) then
    RMem(c+Cst.BCONTX+$04,@Result,4);
end;

procedure TUOVar.WriteContPosY(Value : Integer);
var
  c : Cardinal;
begin
  if GetContAddr(c) then
    WMem(c+Cst.BCONTX+$04,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadNextCPosX : Integer;
var
  i : Integer;
begin
  RMem(Cst.NEXTCPOS+$04,@i,4);
  Result:=i+16;
end;

procedure TUOVar.WriteNextCPosX(Value : Integer);
begin
  Value:=Value-16;
  WMem(Cst.NEXTCPOS+$04,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadNextCPosY : Integer;
var
  i : Integer;
begin
  RMem(Cst.NEXTCPOS+$00,@i,4);
  Result:=i+16;
end;

procedure TUOVar.WriteNextCPosY(Value : Integer);
begin
  Value:=Value-16;
  WMem(Cst.NEXTCPOS+$00,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadTargCurs : Boolean;
begin
  Result:=False;
  RMem(Cst.TARGETCURS,@Result,1);
end;

procedure TUOVar.WriteTargCurs(Value : Boolean);
var
  c : Cardinal;
begin
  VirtualProtectEx(UOSel.HProc,Pointer($400000),$1000,PAGE_EXECUTE_READWRITE,@c);
  c:=$400500;

  // In older clients, you could simply set LTargCurs to 1 and the client would
  // accept clicks and set LTargetID accordingly. A change in later clients made
  // it necessary to install a custom click handler (just for the next click, is
  // overwritten by normal client-initiated clicks). To keep the handler simple,
  // more complex ground clicks are ignored (bad luck if you depended on this).

  with AsmStr do
  begin
    Init(c);
    AddHex('56');               // push esi
    AddHex('8B742408');         // mov esi, [esp+08] ; get clicked object
    AddHex('85F6');             // test esi, esi     ; item or ground?
    AddHex('74');               // je W1
      AddRelB('W1');            //
    AddHex('8B06');             // mov eax, [esi]    ; access object
    AddHex('FF502C');           // call [eax+2C]     ; isClickable()
    AddHex('85C0');             // test eax, eax
    AddHex('74');               // je W1
      AddRelB('W1');            //
    AddHex('8B86');             // mov eax, [esi+BITEMID]
      AddCVal(Cst.BITEMID);     //
    AddHex('A3');               // mov [LTargetID], eax
      AddCVal(Cst.LHANDID+$18); //
    AddHex('31C0');             // xor eax, eax
    AddHex('A3');               // mov [TargCurs], eax
      AddCVal(Cst.TARGETCURS);  //
    AddHex('40');               // inc eax
    AddHex('A3');               // mov [LTargetKind], eax
      AddCVal(Cst.LHANDID+      //
      Cst.BLTARGTILE+$04);      //
    AddLabel('W1');             // W1:
    AddHex('5E');               // pop esi
    AddHex('C3');               // ret
    if not Finalize then Exit;
    WMem(c,@Data[1],Length(Data));
  end;

  WMem(Cst.TARGETCURS+Cst.BTARGPROC,@c,4);
  WMem(Cst.TARGETCURS,@Value,1);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadCliXRes : Cardinal;
begin
  RMem(Cst.CLIXRES,@Result,4);
end;

procedure TUOVar.WriteCliXRes(Value : Cardinal);
begin
  WMem(Cst.CLIXRES,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadCliYRes : Cardinal;
begin
  RMem(Cst.CLIXRES+$04,@Result,4);
end;

procedure TUOVar.WriteCliYRes(Value : Cardinal);
begin
  WMem(Cst.CLIXRES+$04,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadCliLeft : Cardinal;
begin
  RMem(Cst.CLILEFT,@Result,4);
end;

procedure TUOVar.WriteCliLeft(Value : Cardinal);
begin
  WMem(Cst.CLILEFT,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadCliTop : Cardinal;
begin
  RMem(Cst.CLILEFT+$04,@Result,4);
end;

procedure TUOVar.WriteCliTop(Value : Cardinal);
begin
  WMem(Cst.CLILEFT+$04,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadLastSpell : Cardinal;
begin
  Result:=0;
  RMem(Cst.LHANDID+$10+$00,@Result,1);
end;

procedure TUOVar.WriteLastSpell(Value : Cardinal);
begin
  WMem(Cst.LHANDID+$10+$00,@Value,1);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadLastSkill : Cardinal;
begin
  Result:=0;
  RMem(Cst.LHANDID+$10+$04,@Result,1);
end;

procedure TUOVar.WriteLastSkill(Value : Cardinal);
begin
  WMem(Cst.LHANDID+$10+$04,@Value,1);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadLTargetKind : Cardinal;
begin
  Result:=0;
  RMem(Cst.LHANDID+Cst.BLTARGTILE+$04,@Result,1);
end;

procedure TUOVar.WriteLTargetKind(Value : Cardinal);
begin
  WMem(Cst.LHANDID+Cst.BLTARGTILE+$04,@Value,1);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadLTargetTile : Cardinal;
begin
  Result:=0;
  RMem(Cst.LHANDID+Cst.BLTARGTILE,@Result,2);
end;

procedure TUOVar.WriteLTargetTile(Value : Cardinal);
begin
  WMem(Cst.LHANDID+Cst.BLTARGTILE,@Value,2);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadLTargetX : Integer;
var
  i : SmallInt;
begin
  RMem(Cst.LHANDID+Cst.BLTARGX+$00,@i,2);
  Result:=i;
end;

procedure TUOVar.WriteLTargetX(Value : Integer);
begin
  WMem(Cst.LHANDID+Cst.BLTARGX+$00,@Value,2);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadLTargetY : Integer;
var
  i : SmallInt;
begin
  RMem(Cst.LHANDID+Cst.BLTARGX+$02,@i,2);
  Result:=i;
end;

procedure TUOVar.WriteLTargetY(Value : Integer);
begin
  WMem(Cst.LHANDID+Cst.BLTARGX+$02,@Value,2);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadLTargetZ : Integer;
var
  i : SmallInt;
begin
  RMem(Cst.LHANDID+Cst.BLTARGX+$04,@i,2);
  Result:=i;
end;

procedure TUOVar.WriteLTargetZ(Value : Integer);
begin
  WMem(Cst.LHANDID+Cst.BLTARGX+$04,@Value,2);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadLTargetID : Cardinal;
begin
  RMem(Cst.LHANDID+$18,@Result,4);
end;

procedure TUOVar.WriteLTargetID(Value : Cardinal);
begin
  WMem(Cst.LHANDID+$18,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadLHandID : Cardinal;
begin
  RMem(Cst.LHANDID,@Result,4);
end;

procedure TUOVar.WriteLHandID(Value : Cardinal);
begin
  WMem(Cst.LHANDID,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadRHandID : Cardinal;
begin
  RMem(Cst.LHANDID+$04,@Result,4);
end;

procedure TUOVar.WriteRHandID(Value : Cardinal);
begin
  WMem(Cst.LHANDID+$04,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadLObjectID : Cardinal;
begin
  RMem(Cst.LHANDID+$0C,@Result,4);
end;

procedure TUOVar.WriteLObjectID(Value : Cardinal);
begin
  WMem(Cst.LHANDID+$0C,@Value,4);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOVar.ReadCliTitle : AnsiString;
var
  Buf : array[0..8191] of Byte;
begin
  GetWindowText(UOSel.HWnd,@Buf,SizeOf(Buf));
  Result:=PAnsiChar(@Buf);
end;

procedure TUOVar.WriteCliTitle(Value : AnsiString);
begin
  SetWindowText(UOSel.HWnd,PAnsiChar(Value));
end;

////////////////////////////////////////////////////////////////////////////////
end.
