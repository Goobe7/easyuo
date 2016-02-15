unit uoevents;
interface
uses Windows, SysUtils, access, uocommands, uovariables, uoselector, uoclidata,
     asmstring;

type
  TDelayFunc     = function(Duration : Cardinal) : Boolean of object;
  TUOEvent       = class(TObject)
  private
    AsmStr       : TAsmStr;
    UOSel        : TUOSel;
    UOVar        : TUOVar;
    Cst          : TCstDB;
    Delay        : TDelayFunc;
    InitWnd      : Cardinal;
    DragID       : Cardinal;
    function     StdDelay(Duration : Cardinal) : Boolean;
    procedure    RMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
    procedure    WMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
    procedure    InitEvents;
    function     WaitTillProcessed : Boolean;
    function     FindID(ID : Cardinal) : Cardinal;
    function     GetFormStr(Addr : Cardinal) : AnsiString;
  public
    PropStr1     : AnsiString;
    PropStr2     : AnsiString;
    BlockStr     : AnsiString;
    constructor  Create(UOS : TUOSel; UOV : TUOVar; DFunc : TDelayFunc);
    destructor   Destroy; override;
    procedure    SysMessage(Msg : AnsiString; Col : Cardinal = 0);
    procedure    StatBar(ID : Cardinal);
    procedure    ContTop(Index : Integer);
    procedure    Drag(ID : Cardinal);
    procedure    ExMsg(ID,Font,Color : Cardinal; Msg : AnsiString);
    procedure    Pathfind(X,Y : Cardinal; Z : Integer = $7FFFFFFF);
    procedure    Macro(Par1, Par2 : Cardinal; Str : AnsiString = '');
    function     EvProperty(ID : Cardinal) : Boolean;
    function     BlockInfo(X,Y,Z,W,H : Integer) : Boolean;
    procedure    SendPacket(Packet : AnsiString);
    procedure    ExEv_Drag(ID : Cardinal; Amount : Cardinal = 1);
    procedure    ExEv_DropC(ContID : Cardinal; X : Integer = -1; Y : Integer = -1);
    procedure    ExEv_DropG(X,Y : Cardinal; Z : Integer = $7FFFFFFF);
    procedure    ExEv_DropPD;
    procedure    ExEv_SkillLock(SkillStr : AnsiString; Lock : Cardinal);
    procedure    ExEv_StatLock(Stat : AnsiString; Lock : Cardinal);
    procedure    ExEv_RenamePet(ID : Cardinal; Name : AnsiString);
    procedure    ExEv_PopUp(ID : Cardinal; X,Y : Integer);
  end;

implementation
const
  BaseAddr = $400600; // target address for handler

////////////////////////////////////////////////////////////////////////////////
constructor TUOEvent.Create(UOS : TUOSel; UOV : TUOVar; DFunc : TDelayFunc);
begin
  inherited Create;
  AsmStr:=TAsmStr.Create;
  UOSel:=UOS;
  Cst:=UOSel.CstDB;
  UOVar:=UOV;

  // assign standard delay function if none is provided
  if DFunc=nil then Delay:=@StdDelay
  else Delay:=DFunc;

  InitWnd:=0;
  DragID:=0;
end;

////////////////////////////////////////////////////////////////////////////////
destructor TUOEvent.Destroy;
begin
  AsmStr.Free;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////
// Utility Functions ///////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function TUOEvent.StdDelay(Duration : Cardinal) : Boolean;
// waits a fixed amount of time and returns (replace with your own callback!)
begin
  Sleep(Duration);
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.RMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
// read from memory of currently selected client
begin
  ZeroMemory(Buf,Length);
  if UOSel.Nr>0 then
    ReadMem(UOSel.HProc,MemPos,Buf,Length);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.WMem(MemPos : Cardinal; Buf : PAnsiChar; Length : Cardinal);
// write to memory of currently selected client
begin
  if UOSel.Nr>0 then
    WriteMem(UOSel.HProc,MemPos,Buf,Length);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.InitEvents;
// initializes event handler
var
  c : Cardinal;
begin
  if InitWnd=UOSel.HWnd then Exit; // cancel if already initialized
  InitWnd:=UOSel.HWnd;

  // unprotect client exe header
  VirtualProtectEx(UOSel.HProc,Pointer($400000),$1000,PAGE_EXECUTE_READWRITE,@c);

  // write event handler
  with AsmStr do
  begin
    Init(BaseAddr-4);
    AddWVal($0000);           // dw 0000
    AddLabel('Flag2');        // Flag2:
    AddWVal($0000);           // dw 0000
    AddHex('68');             // push EOLDDIR
      AddCVal(Cst.EOLDDIR);   //
    AddHex('66833D');         // cmp word ptr [Flag2], 0000
      AddOffs('Flag2');       //
      AddHex('00');           //
    AddHex('75');             // jne W1
      AddRelB('W1');          //
    AddHex('C3');             // ret
    AddLabel('W1');           // W1:
    AddHex('66C705');         // mov word ptr [Flag2], 0000
      AddOffs('Flag2');       //
      AddHex('0000');         //
    AddHex('90909090909090'); // nop (7x)
    Finalize;
    WMem(BaseAddr-4,@Data[1],Length(Data));
  end;

  // write redirection
  with AsmStr do
  begin
    Init(Cst.EREDIR);
    AddHex('E8');             // call BaseAddr
      AddRelA(BaseAddr);      //
    Finalize;
    WMem(Cst.EREDIR,@Data[1],Length(Data));
  end;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOEvent.WaitTillProcessed : Boolean;
// waits up to 500ms for request to complete
var
  i : Integer;
  w : Word;
begin
  Result:=False;
  for i:=1 to 20 do
  begin
    if not Delay(25) then Exit;
    RMem(BaseAddr-2,@w,2); // read flag
    if w>0 then Continue;
    Result:=True;
    Break;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOEvent.FindID(ID : Cardinal) : Cardinal;
// finds item structure by ID
var
  c,d : Cardinal;
begin
  Result:=0;
  c:=Cst.CHARPTR-Cst.BITEMID-$C;
  repeat
    RMem(c+Cst.BITEMID+$C,@c,4); // next address
    if c=0 then Exit;
    RMem(c+Cst.BITEMID,@d,4); // read ID
  until d=ID;
  Result:=c;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOEvent.GetFormStr(Addr : Cardinal) : AnsiString;
// sanitizes property strings
var
  i   : Integer;
  a,b : Integer;
  s   : AnsiString;
begin
  SetLength(s,8192);
  RMem(Addr,@s[1],8192);

  // remove unicode (should be changed for asian uo.dll support)
  for i:=1 to 4096 do
    s[i]:=s[2*i-1];
  s:=PAnsiChar(s);

  // replace control characters
  for i:=1 to Length(s) do
    if s[i]<#32 then s[i]:=#32;

  // insert CRLF
  repeat
    i:=Pos('<BR>',s);
    if i<1 then Break;
    Delete(s,i,4);
    Insert(#13#10,s,i);
  until False;

  // remove tags
  repeat
    a:=Pos('<',s);
    b:=Pos('>',s);
    if (a=0) or (b=0) then Break;
    Delete(s,a,b-a+1);
  until False;

  Result:=s;
end;

////////////////////////////////////////////////////////////////////////////////
// Event Functions /////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TUOEvent.SysMessage(Msg : AnsiString; Col : Cardinal = 0);
// prints a note in the lower left area of the client screen
begin
  InitEvents;

  with AsmStr do
  begin
    Init(BaseAddr+$20);
    AddHex('68');               // push Data
      AddOffs('Data');          //
    AddHex('6A03');             // push 03
    AddHex('68');               // push Col
      AddCVal(Col);             //
    AddHex('E8');               // call ESYSMSGADDR
      AddRelA(Cst.ESYSMSGADDR); //
    AddHex('83C40C');           // add esp, 12
    AddHex('C3');               // ret
    AddLabel('Data');           //
    AddBin(Msg+#0);             // db ...
    Finalize;
    WMem(BaseAddr+$20,@Data[1],Length(Data));
  end;

  WMem(BaseAddr-2,#1#0,2);
  WaitTillProcessed;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.StatBar(ID : Cardinal);
// opens the status bar of a creature
var
  c : Cardinal;
begin
  if Cst.ESTATBAR=0 then Exit;
  c:=FindID(ID);
  if c=0 then Exit; // ID on screen?
  InitEvents;

  with AsmStr do
  begin
    Init(BaseAddr+$20);
    AddHex('B9');            // mov ecx, NPCPtr
      AddOffs('NPCPtr');     //
    AddHex('83E94C');        // sub ecx, 4C
    AddHex('E9');            // jmp ESTATBAR
      AddRelA(Cst.ESTATBAR); //
    AddLabel('NPCPtr');      // NPCPtr:
    AddCVal(c);              // dd c
    Finalize;
    WMem(BaseAddr+$20,@Data[1],Length(Data));
  end;

  WMem(BaseAddr-2,#01#00,2);
  WaitTillProcessed;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.ContTop(Index : Integer);
// places the specified container on top
var
  c : Cardinal;
begin
  if Cst.ECONTTOP=0 then Exit;
  if not UOVar.GetContAddr(c,Index) then Exit;
  InitEvents;

  with AsmStr do
  begin
    Init(BaseAddr+$20);
    AddHex('B9');            // mov ecx, GumpPtr
      AddCVal(c);            //
    AddHex('6A00');          // push dword ptr 0
    AddHex('E8');            // call ECONTTOP
      AddRelA(Cst.ECONTTOP); //
    AddHex('C3');            // ret
    Finalize;
    WMem(BaseAddr+$20,@Data[1],Length(Data));
  end;

  WMem(BaseAddr-2,#01#00,2);
  WaitTillProcessed;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.Drag(ID : Cardinal);
// drags the specified item
var
  c : Cardinal;
begin
  if UOVar.LLiftedKind>0 then Exit;
  c:=FindID(ID);
  if c=0 then Exit;
  InitEvents;
  DragID:=ID; // remember ID

  with AsmStr do
  begin
    Init(BaseAddr+$20);
    AddHex('6A00');           // push dword ptr 0
    AddHex('68');             // push ItemPtr
      AddCVal(c);             //
    AddHex('E8');             // call EDRAGADDR
      AddRelA(Cst.EDRAGADDR); //
    AddHex('83C408');         // add esp, 8
    AddHex('C3');             // ret
    Finalize;
    WMem(BaseAddr+$20,@Data[1],Length(Data));
  end;

  WMem(BaseAddr-2,#1#0,2);
  WaitTillProcessed;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.ExMsg(ID,Font,Color : Cardinal; Msg : AnsiString);
// prints a message near the specified ID
var
  c : Cardinal;
begin
  c:=FindID(ID);
  if c=0 then Exit;
  InitEvents;

  with AsmStr do
  begin
    Init(BaseAddr+$20);
    AddHex('B9');              // mov ecx, ItemPtr
      AddCVal(c);              //
    AddHex('6A00');            // push dword ptr 0
    AddHex('6A00');            // push dword ptr 0
    AddHex('68');              // push Font
      AddCVal(Font);           //
    AddHex('68');              // push Color
      AddCVal(Color);          //
    AddHex('68');              // push MsgPtr
      AddOffs('Msg');          //
    AddHex('E8');              // call EEXMSGADDR
      AddRelA(Cst.EEXMSGADDR); //
    AddHex('C3');              // ret
    AddLabel('Msg');           // Msg:
    AddBin(Msg+#0);            // db ...
    Finalize;
    WMem(BaseAddr+$20,@Data[1],Length(Data));
  end;

  WMem(BaseAddr-2,#01#00,2);
  WaitTillProcessed;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.Pathfind(X,Y : Cardinal; Z : Integer = $7FFFFFFF);
// finds a path and moves the character to the specified coordinates
begin
  InitEvents;
  if Z=$7FFFFFFF then Z:=UOVar.CharPosZ;

  with AsmStr do
  begin
    Init(BaseAddr+$20);
    AddHex('6A00');               // push dword ptr 0
    AddHex('6A00');               // push dword ptr 0
    AddHex('68');                 // push W1
      AddOffs('W1');              //
    AddHex('51');                 // push ecx

    if Cst.FPATHFINDVER<1 then
    begin                         // ; only required for older clients:
      AddHex('53');               // push ebx
      AddHex('55');               // push ebp
      AddHex('56');               // push esi
      AddHex('57');               // push edi
    end;

    AddHex('B8');                 // mov eax, Pos
      AddOffs('Pos');             //
    AddHex('83E824');             // sub eax, 24h
    AddHex('E9');                 // jmp EPATHFINDADDR
      AddRelA(Cst.EPATHFINDADDR); //
    AddLabel('W1');               // W1:
    AddHex('C3');                 // ret
    AddLabel('Pos');              // Pos:
    AddWVal(X);                   // dw X
    AddWVal(Y);                   // dw Y
    AddWVal(Z);                   // dw Z
    Finalize;
    WMem(BaseAddr+$20,@Data[1],Length(Data));
  end;

  WMem(BaseAddr-2,#01#00,2);
  WaitTillProcessed;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.Macro(Par1, Par2 : Cardinal; Str : AnsiString = '');
// causes the client to execute a macro action
var
  s : AnsiString;
begin
  InitEvents;

  // convert ansi string to wide (should be changed for asian uo.dll support)
  SetLength(s,512);
  ZeroMemory(@s[1],Length(s));
  StringToWideChar(Str,@s[1],Length(s));

  // correct mapping to adjust for changes to spell casting over the years
  if (Par1=15) and (Cst.FMACROMAP>0) then
  begin
    if Par2<100 then Inc(Par2); // magery spells used to be zero-based
    if Par2 in [145..150,245..252] then
      Par2:=Par2+256; // Bushido and Ninja were moved up by 256
  end;

  with AsmStr do
  begin
    Init(BaseAddr+$20);
    AddHex('6A00');             // push dword ptr 0
    AddHex('68');               // push Struc
      AddOffs('Struc');         //
    AddHex('E8');               // call EMACROADDR
      AddRelA(Cst.EMACROADDR);  //
    AddHex('83C408');           // add esp, 8
    AddHex('C3');               // ret
    AddLabel('Struc');          // Struc:
    AddBin(#80#0#0#0#0#0#25#0); // db ... ; some magic string?
    AddBin(#0#0#0#0#1#0#0#0);   // db ...
    AddBin(#0#0#0#0#1#0#0#0);   // db ...
    AddCVal(Par1);              // dd Par1
    AddCVal(Par2);              // dd Par2
    AddOffs('Str');             // dd StrPtr
    AddLabel('Str');            // Str:
    AddBin(s);                  // db ...
    Finalize;
    WMem(BaseAddr+$20,@Data[1],Length(Data));
  end;

  WMem(BaseAddr-2,#01#00,2);
  WaitTillProcessed;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOEvent.EvProperty(ID : Cardinal) : Boolean;
// returns an item's property info
var
  i   : Integer;
  w   : Word;
  c,d : Cardinal;
begin
  Result:=False;
  PropStr1:='';
  PropStr2:='';
  if Cst.FEVPROPERTY=0 then Exit;
  InitEvents;

  for i:=1 to 200 do
  begin

    // check status (but avoid delay in first iteration)
    if i>1 then
    begin
      if not Delay(10) then Exit;
      RMem(BaseAddr-4,@w,2); // get status
      if w=2 then Continue; // not yet processed?
      if w=0 then // success?
      begin
        RMem(BaseAddr+$A0,@c,4);
        RMem(BaseAddr+$A4,@d,4);
        PropStr1:=GetFormStr(c);
        if PropStr1<>'' then
          PropStr2:=GetFormStr(d);
        Result:=True;
        Exit;
      end;
    end;

    with AsmStr do
    begin
      Init(BaseAddr+$20);
      AddHex('B9');                  // mov ecx, ID
        AddCVal(ID);                 //
      AddHex('51');                  // push ecx
      AddHex('51');                  // push ecx
      AddHex('E8');                  // call EITEMCHECKADDR
        AddRelA(Cst.EITEMCHECKADDR); //
      AddHex('59');                  // pop ecx
      AddHex('59');                  // pop ecx
      AddHex('84C0');                // test al, al ; is item in cache?
      AddHex('75');                  // jne N1
        AddRelB('N1');               //
      AddHex('51');                  // push ecx
      AddHex('6A01');                // push dword ptr 1
      AddHex('68');                  // push NamePtr
        AddCVal(BaseAddr+$A0);       //
      AddHex('890D');                // mov [EITEMPROPID], ecx
        AddCVal(Cst.EITEMPROPID);    //
      AddHex('E8');                  // call EITEMNAMEADDR
        AddRelA(Cst.EITEMNAMEADDR);  //
      AddHex('6A01');                // push dword ptr 1
      AddHex('68');                  // push PropPtr
        AddCVal(BaseAddr+$A4);       //
      AddHex('B9');                  // mov ecx, TempBufPtr ; Addr-B8
        AddCVal(BaseAddr+$C0-$B8);   //
      AddHex('E8');                  // call EITEMPROPADDR
        AddRelA(Cst.EITEMPROPADDR);  //
      AddHex('59');                  // pop ecx
      AddHex('66C705');              // mov word ptr [BaseAddr-4], 0
        AddCVal(BaseAddr-4);         //
        AddWVal(0);                  //
      AddHex('C3');                  // ret
      AddLabel('N1');                // N1:

      // request item from server only in first iteration
      if i=1 then
      begin
        AddHex('51');                // push ecx
        AddHex('E8');                // call EITEMREQADDR
          AddRelA(Cst.EITEMREQADDR); //
        AddHex('59');                // pop ecx
      end;

      AddHex('66C705');              // mov word ptr [BaseAddr-4], 1
        AddCVal(BaseAddr-4);         //
        AddWVal(1);                  //
      AddHex('C3');                  // ret
      Finalize;
      WMem(BaseAddr+$20,@Data[1],Length(Data));
    end;

    // write flags (ack-flag, req-flag)
    WMem(BaseAddr-4,#02#00#01#00,4);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
function TUOEvent.BlockInfo(X,Y,Z,W,H : Integer) : Boolean;
// returns height data for the specified map area
begin
  Result:=False;
  BlockStr:='';
  if W*H>32*32 then Exit; // max. buffer size is 2*400h
  InitEvents;

  with AsmStr do
  begin
    Init(BaseAddr+$20);
    AddHex('BA');             // mov edx, Buffer
      AddCVal(BaseAddr+$A0);  //
    AddHex('81C2');           // add edx, 2*W*H
      AddCVal(2*W*H);         //
    AddHex('BF');             // mov edi, H
      AddCVal(H);             //
    AddLabel('N1');           // N1:
    AddHex('4F');             // dec edi
    AddHex('78');             // js End
      AddRelB('End');         //
    AddHex('BE');             // mov esi, W
      AddCVal(W);             //
    AddLabel('N2');           // N2:
    AddHex('4E');             // dec esi
    AddHex('78');             // js N1
      AddRelB('N1');          //
    AddHex('60');             // pushad
    AddHex('68');             // push ResPtr ; can't write directly (4 bytes!)
      AddOffs('Res');         //
    AddHex('6A00');           // push dword ptr 0
    AddHex('68');             // push Z
      AddCVal(Z);             //
    AddHex('B8');             // mov eax, Y
      AddCVal(Y);             //
    AddHex('01F8');           // add eax, edi
    AddHex('50');             // push eax
    AddHex('B8');             // mov eax, X
      AddCVal(X);             //
    AddHex('01F0');           // add eax, esi
    AddHex('50');             // push eax
    AddHex('E8');             // call BLOCKINFO
      AddRelA(Cst.BLOCKINFO); //
    AddHex('83C414');         // add esp, 14h
    AddHex('61');             // popad
    AddHex('83EA02');         // sub edx, 2
    AddHex('66A1');           // mov ax, [Res]
      AddOffs('Res');         //
    AddHex('668902');         // mov [edx], ax
    AddHex('EB');             // jmp short N2
      AddRelB('N2');          //
    AddLabel('End');          // End:
    AddHex('C3');             // ret
    AddLabel('Res');          // Res:
    AddCVal($00000000);       // dd ...
    Finalize;
    WMem(BaseAddr+$20,@Data[1],Length(Data));
  end;

  WMem(BaseAddr-2,#01#00,2);
  if not WaitTillProcessed then Exit;

  // read result
  SetLength(BlockStr,W*H*2);
  RMem(BaseAddr+$A0,@BlockStr[1],W*H*2);

  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.SendPacket(Packet : AnsiString);
// sends a packet to the server
begin
  InitEvents;

  with AsmStr do
  begin
    Init(BaseAddr+$20);

    // old client compatibility
    if Cst.ESENDLEN>0 then
    begin
      AddHex('C705');            // mov dword ptr [ESENDLEN], length
        AddCVal(Cst.ESENDLEN);   //
        AddCVal(Length(Packet)); //
      AddHex('8B0D');            // mov ecx, [ESENDECX]
        AddCVal(Cst.ESENDECX);   //
    end;

    AddHex('68');                // push Packet
      AddOffs('Packet');         //
    AddHex('E8');                // call ESENDPACKET
      AddRelA(Cst.ESENDPACKET);  //
    AddHex('83C404');            // add esp, 4
    AddHex('C3');                // ret
    AddLabel('Packet');          // Packet:
    AddBin(Packet);              // db ...
    Finalize;
    WMem(BaseAddr+$20,@Data[1],Length(Data));
  end;

  WMem(BaseAddr-2,#01#00,2);
  WaitTillProcessed;
end;

////////////////////////////////////////////////////////////////////////////////
// ExEvent Functions ///////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TUOEvent.ExEv_Drag(ID : Cardinal; Amount : Cardinal = 1);
// drags an item
var
  c : Cardinal;
begin
  c:=FindID(ID);
  if c=0 then Exit; // check ID
  DragID:=ID; // remember ID

  SendPacket(
    #7+                      // Drag
    NumStr(ID,4,False)+      // ID
    NumStr(Amount,2,False)); // Amount
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.ExEv_DropC(ContID : Cardinal; X : Integer = -1; Y : Integer = -1);
// drops an item into a container
var
  Fix : AnsiString;
begin
  Fix:=#0;
  if Cst.BPACKETVER>0 then
    Fix:=#0#255; // packet was changed in later clients

  if (X<0)or(Y<0) then // -1/-1 = drop somewhere randomly
  begin
    X:=-1;
    Y:=-1;
  end;

  SendPacket(
    #8+                      // Drop
    NumStr(DragID,4,False)+  // ID
    NumStr(X,2,False)+       // X
    NumStr(Y,2,False)+       // Y
    Fix+                     // Z
    NumStr(ContID,4,False)); // ContID
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.ExEv_DropG(X,Y : Cardinal; Z : Integer = $7FFFFFFF);
// drops an item onto the ground
var
  Fix : AnsiString;
begin
  if Z=$7FFFFFFF then Z:=UOVar.CharPosZ;

  Fix:=AnsiChar(Z);
  if Cst.BPACKETVER>0 then // packet was changed in later clients
    Fix:=Fix+AnsiChar(Hi(Z));

  SendPacket(
    #8+                     // Drop
    NumStr(DragID,4,False)+ // ID
    NumStr(X,2,False)+      // X
    NumStr(Y,2,False)+      // Y
    Fix+                    // Z
    #255#255#255#255);      //
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.ExEv_DropPD;
// drops an item onto the paperdoll
begin
  SendPacket(
    #19+                           // Drop
    NumStr(DragID,4,False)+        // ID (from earlier use)
    #0+                            // Slot
    NumStr(UOVar.CharID,4,False)); // CharID
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.ExEv_SkillLock(SkillStr : AnsiString; Lock : Cardinal);
// locks skills
var
  i : Integer;
begin
  if Lock>2 then Exit;
  SkillStr:=Copy(LowerCase(SkillStr),1,4);

  // find skill nr (or start with 0 if "all")
  if SkillStr<>'all' then
  begin
    i:=SkillFind(0,High(SkillList),SkillStr,@SkillList);
    if i<0 then Exit;
    i:=SkillList[i].Code;
  end
  else i:=0;

  repeat
    WMem(Cst.SKILLLOCK+i,@Lock,1); // also update client memory
    SendPacket(
      #58+               // SkillLock
      #06#00+            // Len
      NumStr(i,2,False)+ // Skill
      AnsiChar(Lock));   // Lock
    Inc(i);
  until (i>High(SkillList)) or (SkillStr<>'all');
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.ExEv_StatLock(Stat : AnsiString; Lock : Cardinal);
// locks stats
var
  i : Integer;
begin
  if Lock>2 then Exit;
  Stat:=LowerCase(Stat);
  InitEvents;

  i:=-1;
  if Stat='str' then i:=0;
  if Stat='dex' then i:=1;
  if Stat='int' then i:=2;
  if i<0 then Exit;

  SendPacket(
    #191+            // StatLock
    #07#00+          // Len
    #00#26+          // SubCmd
    AnsiChar(i)+     // Stat
    AnsiChar(Lock)); // Lock
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.ExEv_RenamePet(ID : Cardinal; Name : AnsiString);
// renames your pet
var
  i : Integer;
begin
  if Name='' then Exit;
  if FindID(ID)=0 then Exit; // ID on screen?
  for i:=Length(Name) to 29 do Name:=Name+#0; // fill up with zeroes

  SendPacket(
    #117+               // Rename
    NumStr(ID,4,False)+ // ID
    Name);              // Name (len=30)
end;

////////////////////////////////////////////////////////////////////////////////
procedure TUOEvent.ExEv_PopUp(ID : Cardinal; X,Y : Integer);
// displays popup of specified item
var
  c : Cardinal;
begin
  c:=FindID(ID);
  if c=0 then Exit; // ID on screen?

  // The following writes the used parameters into memory, otherwise
  // the client won't know what to do with the server's reply.

  if Cst.POPUPID=0 then Exit; // popups even supported?
  WMem(Cst.POPUPID-8,PAnsiChar(
    NumStr(X,4,True)+NumStr(Y,4,True)+NumStr(ID,4,True)),12);

  SendPacket(
    #191+                // PopUp
    #9#0+                // Len
    #0#19+               // SubCmd
    NumStr(ID,4,False)); // ID
end;

////////////////////////////////////////////////////////////////////////////////
end.
