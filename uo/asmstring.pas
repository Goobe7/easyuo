unit asmstring;
interface
uses Classes;

// TAsmStr allows you to assemble a shellcode string piece by piece. Supports
// use of labels and jumps (also forward declarations).

type
  TAsmStr       = class(TObject)
  private
    RelBList    : TStringList;
    RelCList    : TStringList;
    OffsList    : TStringList;
    LabelList   : TStringList;
    Base        : Cardinal;
    function    FindLabel(s : AnsiString; var c : Cardinal) : Boolean;
  public
    Data        : AnsiString;
    constructor Create;
    destructor  Destroy; override;
    procedure   Init(c : Cardinal);
    procedure   AddBin(s : AnsiString);   
    procedure   AddHex(s : AnsiString);
    procedure   AddCVal(c : Cardinal);    
    procedure   AddOffs(s : AnsiString);  
    procedure   AddLabel(s : AnsiString); 
    procedure   AddRelA(c : Cardinal);    
    procedure   AddRelB(s : AnsiString);  
    procedure   AddRelC(s : AnsiString);  
    function    Finalize : Boolean;       
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
constructor TAsmStr.Create;
begin
  inherited Create;
  RelBList:=TStringList.Create;
  RelCList:=TStringList.Create;
  OffsList:=TStringList.Create;
  LabelList:=TStringList.Create;
  LabelList.Sorted:=True;
end;

////////////////////////////////////////////////////////////////////////////////
destructor TAsmStr.Destroy;
begin
  RelBList.Free;
  RelCList.Free;
  OffsList.Free;
  LabelList.Free;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsmStr.Init(c : Cardinal);
// resets all lists and variables, also sets base offset
begin
  RelBList.Clear;
  RelCList.Clear;
  OffsList.Clear;
  LabelList.Clear;
  Base:=c;
  Data:='';
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsmStr.AddBin(s : AnsiString);
// adds simple binary code
begin
  Data:=Data+s;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsmStr.AddHex(s : AnsiString);
// adds simple code in hex format
var
  i : Integer;
  b : Byte;
begin
  b:=0;
  for i:=1 to Length(s) do
  begin
    b:=b shl 4;
    case s[i] of
      '0'..'9': b:=b + Byte(s[i]) - 48;
      'A'..'F': b:=b + Byte(s[i]) - 55;
      'a'..'f': b:=b + Byte(s[i]) - 87;
    end;
    if i and 1 = 1 then Continue;
    Data:=Data+AnsiChar(b);
    b:=0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsmStr.AddCVal(c : Cardinal);
// adds an absolute cardinal
var
  s : AnsiString;
begin
  s:=#0#0#0#0;
  Move(c,s[1],4); // fill buffer
  Data:=Data+s;   // add buffer
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsmStr.AddOffs(s : AnsiString);
// adds an absolute cardinal that points to a label
begin
  OffsList.AddObject(s,TObject(Length(Data)));
  Data:=Data+#0#0#0#0;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsmStr.AddLabel(s : AnsiString);
// registers a label at the current offset
begin
  LabelList.AddObject(s,TObject(Length(Data)));
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsmStr.AddRelA(c : Cardinal);
// adds a relative cardinal that points to an external address
var
  d : Cardinal;
begin
  Data:=Data+#0#0#0#0; // add buffer first
  d:=Length(Data);
  c:=c-(Base+d);       // calculate delta value between target and next position
  Move(c,Data[d-3],4); // write delta to buffer
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsmStr.AddRelB(s : AnsiString);
// adds a relative byte that points to a label
begin
  RelBList.AddObject(s,TObject(Length(Data)));
  Data:=Data+#0;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsmStr.AddRelC(s : AnsiString);
// adds a relative cardinal that points to a label
begin
  RelCList.AddObject(s,TObject(Length(Data)));
  Data:=Data+#0#0#0#0;
end;

////////////////////////////////////////////////////////////////////////////////
function TAsmStr.FindLabel(s : AnsiString; var c : Cardinal) : Boolean;
// returns label offset
var
  i   : Integer;
begin
  Result:=False;
  if not LabelList.Find(s,i) then Exit;
  c:=Cardinal(LabelList.Objects[i]);
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
function TAsmStr.Finalize : Boolean;
// fills in Offs, RelB and RelC based on registered labels
var
  c   : Cardinal;
  src : Cardinal;
  trg : Cardinal;
begin
  Result:=False; // fail in case of dangling jump without matching label

  while OffsList.Count>0 do
  begin
    if not FindLabel(OffsList[0],c) then Exit;
    trg:=Cardinal(OffsList.Objects[0]);
    src:=Base+c;             // calculate offset
    Move(src,Data[1+trg],4); // fill in offset
    OffsList.Delete(0);
  end;

  while RelBList.Count>0 do
  begin
    if not FindLabel(RelBList[0],c) then Exit;
    trg:=Cardinal(RelBList.Objects[0]);
    src:=c-(trg+1);          // calculate delta
    Move(src,Data[1+trg],1); // fill in delta
    RelBList.Delete(0);
  end;

  while RelCList.Count>0 do
  begin
    if not FindLabel(RelCList[0],c) then Exit;
    trg:=Cardinal(RelCList.Objects[0]);
    src:=c-(trg+4);          // calculate delta
    Move(src,Data[1+trg],4); // fill in delta
    RelCList.Delete(0);
  end;

  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
end.
