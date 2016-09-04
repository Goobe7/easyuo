unit main;
interface
uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
     StdCtrls, ExtCtrls, uoselector, uovariables, uocommands, uoevents;

type

  { TMainForm }

  TMainForm = class(TForm)
    CloseClientButton: TButton;
    DragButton: TButton;
    DragEdit: TEdit;
    ExMsgButton: TButton;
    ExMsgEdit: TEdit;
    HideItemButton: TButton;
    HideItemEdit: TEdit;
    KeyButton: TButton;
    KeyEdit: TEdit;
    CommandLabel: TLabel;
    EventLabel: TLabel;
    VariableLabel: TLabel;
    Macro1Edit: TEdit;
    Macro2Edit: TEdit;
    MacroButton: TButton;
    MoveButton: TButton;
    MoveXEdit: TEdit;
    MoveYEdit: TEdit;
    MsgButton: TButton;
    MsgEdit: TEdit;
    OpenClientButton: TButton;
    PathfindButton: TButton;
    PathfindXEdit: TEdit;
    PathfindYEdit: TEdit;
    PopupButton: TButton;
    RefreshTimer: TTimer;
    StatBarButton: TButton;
    SysMessageButton: TButton;
    SysMessageEdit: TEdit;
    VarLabel: TLabel;
    VarScrollBox: TScrollBox;
    procedure ButtonHandler(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
  end;

var
  MainForm  : TMainForm;
  UOSel     : TUOSel;
  UOVar     : TUOVar;
  UOCmd     : TUOCmd;
  UOEvent   : TUOEvent;
  UpdateCnt : Integer = 0;

implementation
{$R *.lfm}

////////////////////////////////////////////////////////////////////////////////
function i2s(i : Integer) : String;
begin
  Result:=IntToStr(i);
end;

////////////////////////////////////////////////////////////////////////////////
function b2s(b : Boolean) : String;
begin
  Result:='False';
  if b then Result:='True';
end;

////////////////////////////////////////////////////////////////////////////////
function s2i(s : String) : Integer;
begin
  Result:=StrToIntDef(s,0);
end;

////////////////////////////////////////////////////////////////////////////////
function UpdateVariables : String;
begin
  Inc(UpdateCnt);
  if UpdateCnt mod 10 = 0 then
  begin
    UOCmd.GetShopInfo;
    UOCmd.ScanJournal(0);
    UOCmd.GetJournal(0);
    UOCmd.ScanItems;
    UOCmd.FilterItems(4,1);
    UOCmd.GetItem(0);
    UOCmd.GetSkill('FOCUS');
    UOEvent.EvProperty(UOVar.CharID);
  end;

  Result:=

  'Character Info:'#13#10+
  '#CHARPOSX: '+i2s(UOVar.CharPosX)+#13#10+
  '#CHARPOSY: '+i2s(UOVar.CharPosY)+#13#10+
  '#CHARPOSZ: '+i2s(UOVar.CharPosZ)+#13#10+
  '#CHARDIR: '+i2s(UOVar.CharDir)+#13#10+
  '#CHARSTATUS: '+UOVar.CharStatus+#13#10+
  '#CHARID: '+i2s(UOVar.CharID)+#13#10+
  '#CHARTYPE: '+i2s(UOVar.CharType)+#13#10+
  '#CHARGHOST: '+b2s(UOVar.CharGhost)+#13#10+
  '#BACKPACKID: '+i2s(UOVar.BackpackID)+#13#10+
  #13#10+

  'Status Bar:'+#13#10+
  '#CHARNAME: '+UOVar.CharName+#13#10+
  '#SEX: '+i2s(UOVar.Sex)+#13#10+
  '#STR: '+i2s(UOVar.Str)+#13#10+
  '#DEX: '+i2s(UOVar.Dex)+#13#10+
  '#INT: '+i2s(UOVar.Int)+#13#10+
  '#HITS: '+i2s(UOVar.Hits)+#13#10+
  '#MAXHITS: '+i2s(UOVar.MaxHits)+#13#10+
  '#STAMINA: '+i2s(UOVar.Stamina)+#13#10+
  '#MAXSTAM: '+i2s(UOVar.MaxStam)+#13#10+
  '#MANA: '+i2s(UOVar.Mana)+#13#10+
  '#MAXMANA: '+i2s(UOVar.MaxMana)+#13#10+
  '#MAXSTATS: '+i2s(UOVar.MaxStats)+#13#10+
  '#LUCK: '+i2s(UOVar.Luck)+#13#10+
  '#WEIGHT: '+i2s(UOVar.Weight)+#13#10+
  '#MAXWEIGHT: '+i2s(UOVar.MaxWeight)+#13#10+
  '#MINDMG: '+i2s(UOVar.MinDmg)+#13#10+
  '#MAXDMG: '+i2s(UOVar.MaxDmg)+#13#10+
  '#GOLD: '+i2s(UOVar.Gold)+#13#10+
  '#FOLLOWERS: '+i2s(UOVar.Followers)+#13#10+
  '#MAXFOL: '+i2s(UOVar.MaxFol)+#13#10+
  '#AR: '+i2s(UOVar.AR)+#13#10+
  '#FR: '+i2s(UOVar.FR)+#13#10+
  '#CR: '+i2s(UOVar.CR)+#13#10+
  '#PR: '+i2s(UOVar.PR)+#13#10+
  '#ER: '+i2s(UOVar.ER)+#13#10+
  '#TP: '+i2s(UOVar.TP)+#13#10+
   #13#10+

  'Container Info:'+#13#10+
  '#NEXTCPOSX: '+i2s(UOVar.NextCPosX)+#13#10+
  '#NEXTCPOSY: '+i2s(UOVar.NextCPosY)+#13#10+
  '#CONTPOSX: '+i2s(UOVar.ContPosX)+#13#10+
  '#CONTPOSY: '+i2s(UOVar.ContPosY)+#13#10+
  '#CONTSIZEX: '+i2s(UOVar.ContSizeX)+#13#10+
  '#CONTSIZEY: '+i2s(UOVar.ContSizeY)+#13#10+
  '#CONTKIND: '+i2s(UOVar.ContKind)+#13#10+
  '#CONTNAME: '+UOVar.ContName+#13#10+
  '#CONTID: '+i2s(UOVar.ContID)+#13#10+
  '#CONTTYPE: '+i2s(UOVar.ContType)+#13#10+
  '#CONTHP: '+i2s(UOCmd.ContHP)+#13#10+
  #13#10+

  'Last Action:'+#13#10+
  '#LOBJECTID: '+i2s(UOVar.LObjectID)+#13#10+
  '#LOBJECTTYPE: '+i2s(UOVar.LObjectType)+#13#10+
  '#LTARGETID: '+i2s(UOVar.LTargetID)+#13#10+
  '#LTARGETX: '+i2s(UOVar.LTargetX)+#13#10+
  '#LTARGETY: '+i2s(UOVar.LTargetY)+#13#10+
  '#LTARGETZ: '+i2s(UOVar.LTargetZ)+#13#10+
  '#LTARGETKIND: '+i2s(UOVar.LTargetKind)+#13#10+
  '#LTARGETTILE: '+i2s(UOVar.LTargetTile)+#13#10+
  '#LLIFTEDID: '+i2s(UOVar.LLiftedID)+#13#10+
  '#LLIFTEDTYPE: '+i2s(UOVar.LLiftedType)+#13#10+
  '#LLIFTEDKIND: '+i2s(UOVar.LLiftedKind)+#13#10+
  '#LSKILL: '+i2s(UOVar.LSkill)+#13#10+
  '#LSPELL: '+i2s(UOVar.LSpell)+#13#10+
   #13#10+

  'Find Item:'+#13#10+
  '#FINDID: '+i2s(UOCmd.ItemRes.ItemID)+#13#10+
  '#FINDTYPE: '+i2s(UOCmd.ItemRes.ItemType)+#13#10+
  '#FINDX: '+i2s(UOCmd.ItemRes.ItemX)+#13#10+
  '#FINDY: '+i2s(UOCmd.ItemRes.ItemY)+#13#10+
  '#FINDZ: '+i2s(UOCmd.ItemRes.ItemZ)+#13#10+
  '#FINDKIND: '+i2s(UOCmd.ItemRes.ItemKind)+#13#10+
  '#FINDSTACK: '+i2s(UOCmd.ItemRes.ItemStack)+#13#10+
  '#FINDREP: '+i2s(UOCmd.ItemRes.ItemRep)+#13#10+
  '#FINDCOL: '+i2s(UOCmd.ItemRes.ItemCol)+#13#10+
  '#FINDCNT: '+i2s(UOCmd.ItemCnt)+#13#10+
   #13#10+

  'Shop Info:'+#13#10+
  '#SHOPCURPOS: '+i2s(UOCmd.ShopPos)+#13#10+
  '#SHOPCNT: '+i2s(UOCmd.ShopCnt)+#13#10+
  '#SHOPITEMTYPE: '+i2s(UOCmd.ShopType)+#13#10+
  '#SHOPITEMID: '+i2s(UOCmd.ShopID)+#13#10+
  '#SHOPITEMNAME: '+UOCmd.ShopName+#13#10+
  '#SHOPITEMPRICE: '+i2s(UOCmd.ShopPrice)+#13#10+
  '#SHOPITEMMAX: '+i2s(UOCmd.ShopMax)+#13#10+
  #13#10+

  'Extended Info:'+#13#10+
  '#SKILL: '+i2s(UOCmd.SkillNorm)+#13#10+
  '#SKILLLOCK: '+i2s(UOCmd.SkillLock)+#13#10+
  '#SKILLCAP: '+i2s(UOCmd.SkillCap)+#13#10+
  '#JOURNAL: '+UOCmd.JournalStr+#13#10+
  '#JCOLOR: '+i2s(UOCmd.JournalCol)+#13#10+
  '#SYSMSG: '+UOVar.SysMsg+#13#10+
  '#TARGCURS: '+b2s(UOVar.TargCurs)+#13#10+
  '#CURSKIND: '+i2s(UOVar.CursKind)+#13#10+
  '#PROPERTY: '+UOEvent.PropStr1+#13#10+
  #13#10+

  'Client Info:'+#13#10+
  '#CLICNT: '+i2s(UOSel.Cnt)+#13#10+
  '#CLINR: '+i2s(UOSel.Nr)+#13#10+
  '#CLILOGGED: '+b2s(UOVar.CliLogged)+#13#10+
  '#CLIXRES: '+i2s(UOVar.CliXRes)+#13#10+
  '#CLIYRES: '+i2s(UOVar.CliYRes)+#13#10+
  '#CLILEFT: '+i2s(UOVar.CliLeft)+#13#10+
  '#CLITOP: '+i2s(UOVar.CliTop)+#13#10+
  '#CLIVER: '+UOVar.CliVer+#13#10+
  '#CLILANG: '+UOVar.CliLang+#13#10+
  '#CLITITLE: '+UOVar.CliTitle+#13#10+
  #13#10+

  'Combat Info:'+#13#10+
  '#LHANDID: '+i2s(UOVar.LHandID)+#13#10+
  '#RHANDID: '+i2s(UOVar.RHandID)+#13#10+
  '#ENEMYHITS: '+i2s(UOVar.EnemyHits)+#13#10+
  '#ENEMYID: '+i2s(UOVar.EnemyID)+#13#10+
  #13#10+

  'Miscellaneous:'+#13#10+
  '#SHARD: '+UOVar.Shard+#13#10+
  '#LSHARD: '+i2s(UOVar.LShard);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.RefreshTimerTimer(Sender: TObject);
begin
  if UOSel.Nr=0 then
  begin
    if UOSel.Cnt=0 then Exit;
    UOSel.SelectClient(1);
  end;
  VarLabel.Caption:=UpdateVariables;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.ButtonHandler(Sender: TObject);
begin
  with UOCmd do
  case TComponent(Sender).Tag of
    01: OpenClient(True);
    02: CloseClient;
    03: HideItem(s2i(HideItemEdit.Text));
    04: Msg(MsgEdit.Text+#13);
    05: Move(s2i(MoveXEdit.Text),s2i(MoveYEdit.Text),2,5000);
    06: Key(KeyEdit.Text,False,False,False);
    07: UOEvent.SysMessage(SysMessageEdit.Text);
    08: UOEvent.ExMsg(UOVar.CharID,0,0,ExMsgEdit.Text);
    09: UOEvent.StatBar(UOVar.CharID);
    10: UOEvent.Drag(s2i(DragEdit.Text));
    11: UOEvent.Pathfind(s2i(PathfindXEdit.Text),s2i(PathfindYEdit.Text));
    12: UOEvent.Macro(s2i(Macro1Edit.Text),s2i(Macro2Edit.Text));
    13: UOEvent.ExEv_PopUp(UOVar.CharID,0,0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
initialization
  UOSel:=TUOSel.Create;
  UOVar:=TUOVar.Create(UOSel);
  UOCmd:=TUOCmd.Create(UOSel,UOVar,nil);
  UOEvent:=TUOEvent.Create(UOSel,UOVar,nil);
finalization
  UOEvent.Free;
  UOCmd.Free;
  UOVar.Free;
  UOSel.Free;
end.

