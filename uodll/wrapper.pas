unit wrapper;
interface
uses SysUtils, stack, tables, uoselector, uovariables, uocommands, uoevents,
     global;

type
  TDelayProc    = function(Duration : Cardinal; Info : Pointer) : Boolean; stdcall;
  TUOWrap       = class(TObject)
  private
    UOSel       : TUOSel;
    UOVar       : TUOVar;
    UOCmd       : TUOCmd;
    UOEvent     : TUOEvent;
    DelayInfo   : Pointer;
    DelayProc   : TDelayProc;
    function    MGet(Name : AnsiString) : Integer;
    function    MSet(Name : AnsiString) : Integer;
    function    MCall(Name : AnsiString) : Integer;
    function    MType(Name : AnsiString) : Integer;
    function    MDelay(Duration : Cardinal) : Boolean;
  public
    Stack       : TStack;
    constructor Create;
    procedure   Free;
    function    Query : Integer;
    function    Execute : Integer;
  end;

implementation
////////////////////////////////////////////////////////////////////////////////

const
  RES_OK       =  0;
  ERR_NOTFOUND = -1;
  ERR_WRONGPAR = -2;
  ERR_GENERIC  = -3;

  ErrStrings : array[-3..0] of PAnsiChar = (
    'ERR_GENERIC',  // -3
    'ERR_WRONGPAR', // -2
    'ERR_NOTFOUND', // -1
    'RES_OK'        //  0
  );

  InitString : PAnsiChar =
    ' local exec = ...'+
    ' local function ge(...) return exec("Get",...) end'+
    ' local function se(...) return exec("Set",...) end'+
    ' local function ca(...) return exec("Call",...) end'+
    ' local function ty(...) return exec("Type",...) end'+
    ' local function he(...) return exec("Help",...) end'+
    ' local mt = {'+
    '   __index = function(t,k)'+
    '     if ty(k)==3 then'+
    '       return function(...) return ca(k,...) end'+
    '     end'+
    '     return ge(k)'+
    '   end,'+
    '   __newindex = function(t,k,v)'+
    '     se(k,v)'+
    '   end,'+
    '   __help = he,'+
    ' }'+
    ' UO = setmetatable({},mt)'+
    ' return nil';

////////////////////////////////////////////////////////////////////////////////
var
  UOTbl : array[0..139] of TItem = (
    (N: 'AR'          ;T: RO ;C: 206 ;P: 'n'        ),
    (N: 'BackpackID'  ;T: RO ;C: 224 ;P: 'n'        ),
    (N: 'BlockInfo'   ;T: ME ;C: 041 ;P: 'nnnnn'    ),
    (N: 'CR'          ;T: RO ;C: 212 ;P: 'n'        ),
    (N: 'CharDir'     ;T: RO ;C: 207 ;P: 'n'        ),
    (N: 'CharID'      ;T: RO ;C: 204 ;P: 'n'        ),
    (N: 'CharName'    ;T: RO ;C: 240 ;P: 's'        ),
    (N: 'CharPosX'    ;T: RO ;C: 246 ;P: 'n'        ),
    (N: 'CharPosY'    ;T: RO ;C: 232 ;P: 'n'        ),
    (N: 'CharPosZ'    ;T: RO ;C: 223 ;P: 'n'        ),
    (N: 'CharStatus'  ;T: RO ;C: 237 ;P: 's'        ),
    (N: 'CharType'    ;T: RO ;C: 201 ;P: 'n'        ),
    (N: 'CliCnt'      ;T: RO ;C: 247 ;P: 'n'        ),
    (N: 'CliDrag'     ;T: ME ;C: 012 ;P: 'n'        ),
    (N: 'CliLang'     ;T: RO ;C: 214 ;P: 's'        ),
    (N: 'CliLeft'     ;T: RW ;C: 199 ;P: 'n'        ),
    (N: 'CliLogged'   ;T: RO ;C: 211 ;P: 'b'        ),
    (N: 'CliNr'       ;T: RW ;C: 198 ;P: 'n'        ),
    (N: 'CliTitle'    ;T: RW ;C: 177 ;P: 's'        ),
    (N: 'CliTop'      ;T: RW ;C: 197 ;P: 'n'        ),
    (N: 'CliVer'      ;T: RO ;C: 205 ;P: 's'        ),
    (N: 'CliXRes'     ;T: RW ;C: 196 ;P: 'n'        ),
    (N: 'CliYRes'     ;T: RW ;C: 195 ;P: 'n'        ),
    (N: 'Click'       ;T: ME ;C: 003 ;P: 'nnnbbbbb' ),
     (N: 'Click'      ;T: ME ;C: 045 ;P: 'nn'       ),
     (N: 'Click'      ;T: ME ;C: 046 ;P: 'nnn'      ),
     (N: 'Click'      ;T: ME ;C: 047 ;P: 'nnnb'     ),
     (N: 'Click'      ;T: ME ;C: 048 ;P: 'nnnbbb'   ),
     (N: 'Click'      ;T: ME ;C: 049 ;P: 'nnbbbb'   ), // deprecated
    (N: 'CloseClient' ;T: ME ;C: 051 ;P: ''         ),
    (N: 'ContID'      ;T: RO ;C: 229 ;P: 'n'        ),
    (N: 'ContKind'    ;T: RO ;C: 238 ;P: 'n'        ),
    (N: 'ContName'    ;T: RO ;C: 220 ;P: 's'        ),
    (N: 'ContPosX'    ;T: RW ;C: 194 ;P: 'n'        ),
    (N: 'ContPosY'    ;T: RW ;C: 193 ;P: 'n'        ),
    (N: 'ContSizeX'   ;T: RO ;C: 213 ;P: 'n'        ),
    (N: 'ContSizeY'   ;T: RO ;C: 216 ;P: 'n'        ),
    (N: 'ContTop'     ;T: ME ;C: 033 ;P: 'n'        ),
    (N: 'ContType'    ;T: RO ;C: 236 ;P: 'n'        ),
    (N: 'CursKind'    ;T: RO ;C: 251 ;P: 'n'        ),
    (N: 'CursorX'     ;T: RO ;C: 253 ;P: 'n'        ),
    (N: 'CursorY'     ;T: RO ;C: 254 ;P: 'n'        ),
    (N: 'Custom'      ;T: ME ;C: 038 ;P: 's'        ),
    (N: 'Dex'         ;T: RO ;C: 202 ;P: 'n'        ),
    (N: 'Drag'        ;T: ME ;C: 015 ;P: 'nn'       ),
     (N:'Drag'        ;T: ME ;C: 015 ;P: 'n'        ),
    (N: 'DropC'       ;T: ME ;C: 016 ;P: 'nnn'      ),
     (N:'DropC'       ;T: ME ;C: 016 ;P: 'n'        ),
    (N: 'DropG'       ;T: ME ;C: 017 ;P: 'nnn'      ),
     (N:'DropG'       ;T: ME ;C: 017 ;P: 'nn'       ),
    (N: 'DropPD'      ;T: ME ;C: 028 ;P: ''         ),
    (N: 'ER'          ;T: RO ;C: 221 ;P: 'n'        ),
    (N: 'EnemyHits'   ;T: RO ;C: 249 ;P: 'n'        ),
    (N: 'EnemyID'     ;T: RO ;C: 241 ;P: 'n'        ),
    (N: 'Equip'       ;T: ME ;C: 037 ;P: '*'        ),
    (N: 'ExMsg'       ;T: ME ;C: 009 ;P: 'nnns'     ),
     (N:'ExMsg'       ;T: ME ;C: 039 ;P: 'ns'       ),
    (N: 'FR'          ;T: RO ;C: 225 ;P: 'n'        ),
    (N: 'FilterItems' ;T: ME ;C: 042 ;P: 'nn'       ),
     (N:'FilterItems' ;T: ME ;C: 043 ;P: 'n'        ),
    (N: 'Followers'   ;T: RO ;C: 203 ;P: 'n'        ),
    (N: 'GetCont'     ;T: ME ;C: 032 ;P: 'n'        ),
    (N: 'GetItem'     ;T: ME ;C: 024 ;P: 'n'        ),
    (N: 'GetJournal'  ;T: ME ;C: 027 ;P: 'n'        ),
    (N: 'GetPix'      ;T: ME ;C: 008 ;P: 'nn'       ),
    (N: 'GetShop'     ;T: ME ;C: 034 ;P: ''         ),
    (N: 'GetSkill'    ;T: ME ;C: 007 ;P: 's'        ),
    (N: 'Gold'        ;T: RO ;C: 242 ;P: 'n'        ),
    (N: 'HideItem'    ;T: ME ;C: 025 ;P: 'n'        ),
    (N: 'Hits'        ;T: RO ;C: 208 ;P: 'n'        ),
    (N: 'IgnoreCont'  ;T: ME ;C: 052 ;P: 's'        ),
     (N:'IgnoreCont'  ;T: ME ;C: 053 ;P: 'n'        ),
    (N: 'Int'         ;T: RO ;C: 234 ;P: 'n'        ),
    (N: 'Key'         ;T: ME ;C: 004 ;P: 'sbbb'     ),
     (N:'Key'         ;T: ME ;C: 004 ;P: 's'        ),
    (N: 'LHandID'     ;T: RW ;C: 192 ;P: 'n'        ),
    (N: 'LLiftedID'   ;T: RO ;C: 217 ;P: 'n'        ),
    (N: 'LLiftedKind' ;T: RO ;C: 230 ;P: 'n'        ),
    (N: 'LLiftedType' ;T: RO ;C: 200 ;P: 'n'        ),
    (N: 'LObjectID'   ;T: RW ;C: 191 ;P: 'n'        ),
    (N: 'LObjectType' ;T: RO ;C: 252 ;P: 'n'        ),
    (N: 'LShard'      ;T: RW ;C: 190 ;P: 'n'        ),
    (N: 'LSkill'      ;T: RW ;C: 189 ;P: 'n'        ),
    (N: 'LSpell'      ;T: RW ;C: 188 ;P: 'n'        ),
    (N: 'LTargetID'   ;T: RW ;C: 187 ;P: 'n'        ),
    (N: 'LTargetKind' ;T: RW ;C: 186 ;P: 'n'        ),
    (N: 'LTargetTile' ;T: RW ;C: 185 ;P: 'n'        ),
    (N: 'LTargetX'    ;T: RW ;C: 184 ;P: 'n'        ),
    (N: 'LTargetY'    ;T: RW ;C: 183 ;P: 'n'        ),
    (N: 'LTargetZ'    ;T: RW ;C: 182 ;P: 'n'        ),
    (N: 'Luck'        ;T: RO ;C: 231 ;P: 'n'        ),
    (N: 'Macro'       ;T: ME ;C: 014 ;P: 'nns'      ),
     (N:'Macro'       ;T: ME ;C: 014 ;P: 'nn'       ),
    (N: 'Mana'        ;T: RO ;C: 215 ;P: 'n'        ),
    (N: 'MaxDmg'      ;T: RO ;C: 226 ;P: 'n'        ),
    (N: 'MaxFol'      ;T: RO ;C: 209 ;P: 'n'        ),
    (N: 'MaxHits'     ;T: RO ;C: 222 ;P: 'n'        ),
    (N: 'MaxMana'     ;T: RO ;C: 233 ;P: 'n'        ),
    (N: 'MaxStam'     ;T: RO ;C: 210 ;P: 'n'        ),
    (N: 'MaxStats'    ;T: RO ;C: 235 ;P: 'n'        ),
    (N: 'MaxWeight'   ;T: RO ;C: 239 ;P: 'n'        ),
    (N: 'MinDmg'      ;T: RO ;C: 244 ;P: 'n'        ),
    (N: 'Move'        ;T: ME ;C: 005 ;P: 'nnnn'     ),
     (N:'Move'        ;T: ME ;C: 005 ;P: 'nnn'      ),
     (N:'Move'        ;T: ME ;C: 005 ;P: 'nn'       ),
    (N: 'Msg'         ;T: ME ;C: 006 ;P: 's'        ),
    (N: 'NextCPosX'   ;T: RW ;C: 181 ;P: 'n'        ),
    (N: 'NextCPosY'   ;T: RW ;C: 180 ;P: 'n'        ),
    (N: 'OpenClient'  ;T: ME ;C: 050 ;P: 'b'        ),
    (N: 'PR'          ;T: RO ;C: 248 ;P: 'n'        ),
    (N: 'Pathfind'    ;T: ME ;C: 011 ;P: 'nnn'      ),
     (N: 'Pathfind'   ;T: ME ;C: 011 ;P: 'nn'       ),
    (N: 'Popup'       ;T: ME ;C: 036 ;P: 'nnn'      ),
     (N: 'Popup'      ;T: ME ;C: 036 ;P: 'n'        ),
    (N: 'Property'    ;T: ME ;C: 010 ;P: 'n'        ),
    (N: 'RHandID'     ;T: RW ;C: 179 ;P: 'n'        ),
    (N: 'RenamePet'   ;T: ME ;C: 020 ;P: 'ns'       ),
    (N: 'ScanItems'   ;T: ME ;C: 023 ;P: 'b'        ),
     (N:'ScanItems'   ;T: ME ;C: 044 ;P: ''         ),
    (N: 'ScanJournal' ;T: ME ;C: 026 ;P: 'n'        ),
    (N: 'SetShop'     ;T: ME ;C: 035 ;P: 'nn'       ),
    (N: 'Sex'         ;T: RO ;C: 243 ;P: 'n'        ),
    (N: 'Shard'       ;T: RO ;C: 218 ;P: 's'        ),
    (N: 'SkillLock'   ;T: ME ;C: 018 ;P: 'sn'       ),
    (N: 'Stamina'     ;T: RO ;C: 245 ;P: 'n'        ),
    (N: 'StatBar'     ;T: ME ;C: 040 ;P: 'n'        ),
    (N: 'StatLock'    ;T: ME ;C: 019 ;P: 'sn'       ),
    (N: 'Str'         ;T: RO ;C: 227 ;P: 'n'        ),
    (N: 'SysMessage'  ;T: ME ;C: 013 ;P: 'sn'       ),
     (N:'SysMessage'  ;T: ME ;C: 013 ;P: 's'        ),
    (N: 'SysMsg'      ;T: RO ;C: 219 ;P: 's'        ),
    (N: 'TP'          ;T: RO ;C: 250 ;P: 'n'        ),
    (N: 'TargCurs'    ;T: RW ;C: 178 ;P: 'b'        ),
    (N: 'TileCnt'     ;T: ME ;C: 030 ;P: 'nnn'      ),
     (N:'TileCnt'     ;T: ME ;C: 030 ;P: 'nn'       ),
    (N: 'TileGet'     ;T: ME ;C: 031 ;P: 'nnnn'     ),
     (N:'TileGet'     ;T: ME ;C: 031 ;P: 'nnn'      ),
    (N: 'TileInit'    ;T: ME ;C: 029 ;P: 'b'        ),
     (N:'TileInit'    ;T: ME ;C: 029 ;P: ''         ),
    (N: 'Weight'      ;T: RO ;C: 228 ;P: 'n'        )
  );

  Commands : array[0..10] of TItem = (
    (N: 'Call'        ;T: CM ;C: 003 ;P: 's*'       ),
    (N: 'Get'         ;T: CM ;C: 001 ;P: 's'        ),
    (N: 'GetCommands' ;T: QY ;C: 003 ;P: ''         ),
    (N: 'GetError'    ;T: QY ;C: 004 ;P: 'n'        ),
    (N: 'GetFeatures' ;T: QY ;C: 005 ;P: ''         ),
    (N: 'GetInit'     ;T: QY ;C: 006 ;P: ''         ),
    (N: 'GetKeywords' ;T: QY ;C: 007 ;P: ''         ),
    (N: 'Help'        ;T: CM ;C: 005 ;P: 's'        ),
    (N: 'Set'         ;T: CM ;C: 002 ;P: 's?'       ),
    (N: 'SetDelay'    ;T: QY ;C: 001 ;P: 'pp'       ),
    (N: 'Type'        ;T: CM ;C: 004 ;P: 's'        )
  );

////////////////////////////////////////////////////////////////////////////////
constructor TUOWrap.Create;
begin
  inherited Create;
  Stack:=TStack.Create;
  UOSel:=TUOSel.Create;
  UOVar:=TUOVar.Create(UOSel);
  UOCmd:=TUOCmd.Create(UOSel,UOVar,@MDelay);
  UOEvent:=TUOEvent.Create(UOSel,UOVar,@MDelay);
  DelayProc:=nil;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOWrap.Free;
begin
  UOEvent.Free;
  UOCmd.Free;
  UOVar.Free;
  UOSel.Free;
  Stack.Free;
  inherited Free;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOWrap.MGet(Name : AnsiString) : Integer;
// gets the value of a variable
var
  Res : TFindRes;
begin
  Result:=ERR_NOTFOUND;
  if Find_First(Res,Name,@UOTbl,High(UOTbl)+1) then
  repeat
    Result:=ERR_WRONGPAR;
    if UOTbl[Res.Idx].T and (RO+RW) = 0 then Continue;
    Result:=RES_OK;

    with Stack,UOVar,UOSel do
    case UOTbl[Res.Idx].C of
      177: PushStrVal(PAnsiChar(CliTitle));
      178: PushBoolean(TargCurs);
      179: PushInteger(RHandID);
      180: PushInteger(NextCPosY);
      181: PushInteger(NextCPosX);
      182: PushInteger(LTargetZ);
      183: PushInteger(LTargetY);
      184: PushInteger(LTargetX);
      185: PushInteger(LTargetTile);
      186: PushInteger(LTargetKind);
      187: PushInteger(LTargetID);
      188: PushInteger(LSpell);
      189: PushInteger(LSkill);
      190: PushInteger(LShard);
      191: PushInteger(LObjectID);
      192: PushInteger(LHandID);
      193: PushInteger(ContPosY);
      194: PushInteger(ContPosX);
      195: PushInteger(CliYRes);
      196: PushInteger(CliXRes);
      197: PushInteger(CliTop);
      198: PushInteger(Nr);
      199: PushInteger(CliLeft);
      200: PushInteger(LLiftedType);
      201: PushInteger(CharType);
      202: PushInteger(Dex);
      203: PushInteger(Followers);
      204: PushInteger(CharID);
      205: PushStrVal(PAnsiChar(CliVer));
      206: PushInteger(AR);
      207: PushInteger(CharDir);
      208: PushInteger(Hits);
      209: PushInteger(MaxFol);
      210: PushInteger(MaxStam);
      211: PushBoolean(CliLogged);
      212: PushInteger(CR);
      213: PushInteger(ContSizeX);
      214: PushStrVal(PAnsiChar(CliLang));
      215: PushInteger(Mana);
      216: PushInteger(ContSizeY);
      217: PushInteger(LLiftedID);
      218: PushStrVal(PAnsiChar(Shard));
      219: PushStrVal(PAnsiChar(SysMsg));
      220: PushStrVal(PAnsiChar(ContName));
      221: PushInteger(ER);
      222: PushInteger(MaxHits);
      223: PushInteger(CharPosZ);
      224: PushInteger(BackpackID);
      225: PushInteger(FR);
      226: PushInteger(MaxDmg);
      227: PushInteger(Str);
      228: PushInteger(Weight);
      229: PushInteger(ContID);
      230: PushInteger(LLiftedKind);
      231: PushInteger(Luck);
      232: PushInteger(CharPosY);
      233: PushInteger(MaxMana);
      234: PushInteger(Int);
      235: PushInteger(MaxStats);
      236: PushInteger(ContType);
      237: PushStrVal(PAnsiChar(CharStatus));
      238: PushInteger(ContKind);
      239: PushInteger(MaxWeight);
      240: PushStrVal(PAnsiChar(CharName));
      241: PushInteger(EnemyID);
      242: PushInteger(Gold);
      243: PushInteger(Sex);
      244: PushInteger(MinDmg);
      245: PushInteger(Stamina);
      246: PushInteger(CharPosX);
      247: PushInteger(Cnt);
      248: PushInteger(PR);
      249: PushInteger(EnemyHits);
      250: PushInteger(TP);
      251: PushInteger(CursKind);
      252: PushInteger(LObjectType);
      253: PushInteger(CursorX);
      254: PushInteger(CursorY);
      else Result:=ERR_GENERIC;
    end;

    Break;
  until not Find_Next(Res);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOWrap.MSet(Name : AnsiString) : Integer;
// sets the value of a variable
var
  Res : TFindRes;
begin
  Result:=ERR_NOTFOUND;
  if Find_First(Res,Name,@UOTbl,High(UOTbl)+1) then
  repeat
    Result:=ERR_WRONGPAR;
    if UOTbl[Res.Idx].T and RW = 0 then Continue;
    if not ParComp(Stack,3,UOTbl[Res.Idx].P) then Continue;
    Result:=RES_OK;

    with Stack,UOVar,UOSel do
    case UOTbl[Res.Idx].C of
      177: CliTitle:=GetString(3);
      178: TargCurs:=GetBoolean(3);
      179: RHandID:=GetInteger(3);
      180: NextCPosY:=GetInteger(3);
      181: NextCPosX:=GetInteger(3);
      182: LTargetZ:=GetInteger(3);
      183: LTargetY:=GetInteger(3);
      184: LTargetX:=GetInteger(3);
      185: LTargetTile:=GetInteger(3);
      186: LTargetKind:=GetInteger(3);
      187: LTargetID:=GetInteger(3);
      188: LSpell:=GetInteger(3);
      189: LSkill:=GetInteger(3);
      190: LShard:=GetInteger(3);
      191: LObjectID:=GetInteger(3);
      192: LHandID:=GetInteger(3);
      193: ContPosY:=GetInteger(3);
      194: ContPosX:=GetInteger(3);
      195: CliYRes:=GetInteger(3);
      196: CliXRes:=GetInteger(3);
      197: CliTop:=GetInteger(3);
      198: SelectClient(GetInteger(3));
      199: CliLeft:=GetInteger(3);
      else Result:=ERR_GENERIC;
    end;

    Break;
  until not Find_Next(Res);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOWrap.MCall(Name : AnsiString) : Integer;
// calls a function
var
  Res : TFindRes;
  i   : Integer;
  s   : AnsiString;
begin
  Result:=ERR_NOTFOUND;
  if Find_First(Res,Name,@UOTbl,High(UOTbl)+1) then
  repeat
    Result:=ERR_WRONGPAR;
    if UOTbl[Res.Idx].T and ME = 0 then Continue;
    if not ParComp(Stack,3,UOTbl[Res.Idx].P) then Continue;
    Result:=RES_OK;

    with UOEvent,UOCmd,Stack do
    case UOTbl[Res.Idx].C of
      003: Click(GetInteger(3),GetInteger(4),GetInteger(5),GetBoolean(6),GetBoolean(7),GetBoolean(8),GetBoolean(9),GetBoolean(10));
      004: Key(GetString(3),GetBoolean(4),GetBoolean(5),GetBoolean(6));
      005: PushBoolean(Move(GetInteger(3),GetInteger(4),GetInteger(5),GetInteger(6)));
      006: Msg(GetString(3));
      007: begin
             GetSkill(GetString(3));
             PushInteger(SkillNorm);
             PushInteger(SkillReal);
             PushInteger(SkillCap);
             PushInteger(SkillLock);
           end;
      008: PushInteger(GetPix(GetInteger(3),GetInteger(4)));
      009: ExMsg(GetInteger(3),GetInteger(4),GetInteger(5),GetString(6));
      010: begin
             EvProperty(GetInteger(3));
             PushStrVal(PAnsiChar(PropStr1));
             PushStrVal(PAnsiChar(PropStr2));
           end;
      011: if GetTop=3 then Pathfind(GetInteger(3),GetInteger(4),GetInteger(5))
           else Pathfind(GetInteger(3),GetInteger(4),UOVar.CharPosZ);
      012: Drag(GetInteger(3));
      013: SysMessage(GetString(3),GetInteger(4));
      014: Macro(GetInteger(3),GetInteger(4),GetString(5));
      015: if GetTop=4 then ExEv_Drag(GetInteger(3),GetInteger(4))
           else ExEv_Drag(GetInteger(3));
      016: if GetTop=5 then ExEv_DropC(GetInteger(3),GetInteger(4),GetInteger(5))
           else ExEv_DropC(GetInteger(3));
      017: if GetTop=5 then ExEv_DropG(GetInteger(3),GetInteger(4),GetInteger(5))
           else ExEv_DropG(GetInteger(3),GetInteger(4));
      018: ExEv_SkillLock(GetString(3),GetInteger(4));
      019: ExEv_StatLock(GetString(3),GetInteger(4));
      020: ExEv_RenamePet(GetInteger(3),GetString(4));
      023: begin
             ScanItems(GetBoolean(3));
             PushInteger(ItemCnt);
           end;
      024: begin
             GetItem(GetInteger(3));
             PushInteger(ItemRes.ItemID);
             PushInteger(ItemRes.ItemType);
             PushInteger(ItemRes.ItemKind);
             PushInteger(ItemRes.ContID);
             PushInteger(ItemRes.ItemX);
             PushInteger(ItemRes.ItemY);
             PushInteger(ItemRes.ItemZ);
             PushInteger(ItemRes.ItemStack);
             PushInteger(ItemRes.ItemRep);
             PushInteger(ItemRes.ItemCol);
           end;
      025: HideItem(GetInteger(3));
      026: begin
             ScanJournal(GetInteger(3));
             PushInteger(JournalRef);
             PushInteger(JournalCnt);
           end;
      027: begin
             GetJournal(GetInteger(3));
             PushStrVal(PAnsiChar(JournalStr));
             PushInteger(JournalCol);
           end;
      028: ExEv_DropPD;
      029: PushBoolean(TileInit(GetBoolean(3)));
      030: if GetTop=5 then PushInteger(TileCnt(GetInteger(3),GetInteger(4),GetInteger(5)))
           else PushInteger(TileCnt(GetInteger(3),GetInteger(4)));
      031: begin
             if GetTop=6 then TileGet(GetInteger(3),GetInteger(4),GetInteger(5),GetInteger(6))
             else TileGet(GetInteger(3),GetInteger(4),GetInteger(5));
             PushInteger(TileType);
             PushInteger(TileZ);
             PushStrVal(PAnsiChar(TileName));
             PushInteger(TileFlags);
           end;
      032: if GetCont(GetInteger(3)) then
           begin
             PushStrVal(PAnsiChar(ContName));
             PushInteger(ContX);
             PushInteger(ContY);
             PushInteger(ContSX);
             PushInteger(ContSY);
             PushInteger(ContKind);
             PushInteger(ContID);
             PushInteger(ContType);
             PushInteger(ContHP);
           end
           else PushNil;
      033: ContTop(GetInteger(3));
      034: if GetShopInfo then
           begin
             PushBoolean(True);
             PushInteger(ShopPos);
             PushInteger(ShopCnt);
             PushInteger(ShopID);
             PushInteger(ShopType);
             PushInteger(ShopMax);
             PushInteger(ShopPrice);
             PushStrVal(PAnsiChar(ShopName));
           end
           else PushBoolean(False);
      035: PushBoolean(SetShopItem(GetInteger(3),GetInteger(4)));
      036: ExEv_PopUp(GetInteger(3),GetInteger(4),GetInteger(5));
      037: begin
             s:='';
             for i:=3 to GetTop do
               if GetType(i)=T_NUMBER then
                 s:=s+NumStr(GetInteger(i),4,False);
             if s<>'' then SendPacket(#236+
               NumStr(Length(s)+4,2,True)+
               NumStr(Length(s) div 4,1,True)+
               s);
           end;
      038: begin
             SetString(s,GetLString(3,i),i);
             SendPacket(s);
           end;
      039: ExMsg(GetInteger(3),3,0,GetString(4));
      040: StatBar(GetInteger(3));
      041: begin
             BlockInfo(GetInteger(3),GetInteger(4),GetInteger(5),GetInteger(6),GetInteger(7));
             PushLStrVal(PAnsiChar(BlockStr),Length(BlockStr));
           end;
      042: begin
             FilterItems(GetInteger(3),GetInteger(4));
             PushInteger(ItemCnt);
           end;
      043: begin
             FilterItems(GetInteger(3));
             PushInteger(ItemCnt);
           end;
      044: begin
             ScanItems;
             PushInteger(ItemCnt);
           end;
      045: Click(GetInteger(3),GetInteger(4));
      046: Click(GetInteger(3),GetInteger(4),GetInteger(5));
      047: Click(GetInteger(3),GetInteger(4),GetInteger(5),GetBoolean(6));
      048: Click(GetInteger(3),GetInteger(4),GetInteger(5),GetBoolean(6),GetBoolean(7),GetBoolean(8));
      049: Click(GetInteger(3),GetInteger(4),1,GetBoolean(5),GetBoolean(6),GetBoolean(7),True,GetBoolean(8)); //deprecated, for compatibility only!
      050: PushBoolean(OpenClient(GetBoolean(3)));
      051: CloseClient;
      052: UOVar.IgnoreCont(GetString(3));
      053: UOVar.IgnoreCont(GetInteger(3));
      else Result:=ERR_GENERIC;
    end;

    Break;
  until not Find_Next(Res);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOWrap.MType(Name : AnsiString) : Integer;
// returns type information
var
  Res : TFindRes;
  i   : Integer;
begin
  Result:=ERR_NOTFOUND;
  if not Find_First(Res,Name,@UOTbl,High(UOTbl)+1) then Exit;
  Result:=RES_OK;

  case UOTbl[Res.Idx].T of
    RO: i:=1;
    RW: i:=2;
    ME: i:=3;
    EV: i:=4;
    CM: i:=5;
    QY: i:=6;
  end;

  Stack.PushInteger(i);
  repeat
    Stack.PushStrRef(UOTbl[Res.Idx].P);
  until not Find_Next(Res);
end;

////////////////////////////////////////////////////////////////////////////////
function TUOWrap.MDelay(Duration : Cardinal) : Boolean;
// wraps caller-provided wait handler
var
  save : TStack;
begin
  // call sleep directly if no handler is assigned (yet)
  if not Assigned(DelayProc) then
  begin
    Sleep(Duration);
    Result:=True;
    Exit;
  end;

  // save stack before passing execution to handler and restore afterwards
  save:=Stack;
  Stack:=TStack.Create;
  Result:=DelayProc(Duration,DelayInfo);
  Stack.Free;
  Stack:=save;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOWrap.Query : Integer;
// returns or sets meta information about the DLL
var
  Res : TFindRes;
  i   : Integer;
begin
  Stack.Mark;
  Result:=ERR_NOTFOUND;
  if Find_First(Res,Stack.GetString(1),@Commands,High(Commands)+1) then
  repeat
    Result:=ERR_WRONGPAR;
    if Commands[Res.Idx].T and QY = 0 then Continue;
    if not ParComp(Stack,2,Commands[Res.Idx].P) then Continue;
    Result:=RES_OK;

    case Commands[Res.Idx].C of
      001: begin
             DelayProc:=TDelayProc(Stack.GetPointer(2));
             DelayInfo:=Stack.GetPointer(3);
           end;
      003: for i:=0 to High(Commands) do
             if Commands[i].T=CM then
               Stack.PushStrRef(Commands[i].N);
      004: begin
             i:=Stack.GetInteger(2);
             if (i<Low(ErrStrings))or(i>0) then
               Stack.PushStrRef('ERR_UNKNOWNERR')
             else Stack.PushStrRef(ErrStrings[i]);
           end;
      005: Stack.PushStrRef('CEIKD');
      006: Stack.PushStrRef(InitString);
      007: begin
             Stack.PushStrRef(UOTbl[0].N);
             for i:=1 to High(UOTbl) do
               if UOTbl[i].N<>UOTbl[i-1].N then
                 Stack.PushStrRef(UOTbl[i].N);
           end;
      else Result:=ERR_GENERIC;
    end;

    Break;
  until not Find_Next(Res);
  Stack.Clean;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOWrap.Execute : Integer;
// executes a command (get/set/call/type)
var
  Res  : TFindRes;
  Name : AnsiString;
begin
  Stack.Mark;
  Result:=ERR_NOTFOUND;
  if Find_First(Res,Stack.GetString(1),@Commands,High(Commands)+1) then
  repeat
    Result:=ERR_WRONGPAR;
    if Commands[Res.Idx].T and CM = 0 then Continue;
    if not ParComp(Stack,2,Commands[Res.Idx].P) then Continue;
    Result:=RES_OK;

    Name:=Stack.GetString(2);
    case Commands[Res.Idx].C of
      001: Result:=MGet(Name);
      002: Result:=MSet(Name);
      003: Result:=MCall(Name);
      004: Result:=MType(Name);
      005: Result:=RES_OK; // MHelp (unused)
      else Result:=ERR_GENERIC;
    end;

    Break;
  until not Find_Next(Res);
  Stack.Clean;
end;

////////////////////////////////////////////////////////////////////////////////
end.
