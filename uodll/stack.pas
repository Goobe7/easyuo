unit stack;
interface
uses SysUtils, Classes;
const
  T_NIL         = 0; // external types
  T_BOOLEAN     = 1;
  T_POINTER     = 2;
  T_NUMBER      = 3;
  T_STRING      = 4;

type
  TStack        = class(TObject)
  private
    Marker      : Cardinal;
    DT          : TList; // element type
    DA          : TList; // element content or address
    function    GetIndex(Index : Integer; out Res : Integer) : LongBool;
  public
    constructor Create;
    procedure   Free;
    function    GetTop : Integer;
    function    GetType(Index : Integer) : Integer;
    procedure   Insert(Index : Integer);
    procedure   PushNil;
    procedure   PushBoolean(Value : LongBool);
    procedure   PushPointer(Value : Pointer);
    procedure   PushPtrOrNil(Value : Pointer);
    procedure   PushInteger(Value : Integer);
    procedure   PushDouble(Value : Double);
    procedure   PushStrRef(Value : PAnsiChar);
    procedure   PushStrVal(Value : PAnsiChar);
    procedure   PushLStrRef(Value : PAnsiChar; Len : Integer);
    procedure   PushLStrVal(Value : PAnsiChar; Len : Integer);
    procedure   PushValue(Index : Integer);
    function    GetBoolean(Index : Integer) : LongBool;
    function    GetPointer(Index : Integer) : Pointer;
    function    GetInteger(Index : Integer) : Integer;
    function    GetDouble(Index : Integer) : Double;
    function    GetString(Index : Integer) : PAnsiChar;
    function    GetLString(Index : Integer; out Len : Integer) : PAnsiChar;
    procedure   Remove(Index : Integer);
    procedure   SetTop(Index : Integer);
    procedure   Mark;
    procedure   Clean;
    procedure   MoveTo(Target : TStack; Index,Cnt : Integer);
  end;

implementation
////////////////////////////////////////////////////////////////////////////////

type
  TLStr         = record   // This type is used to hold S_LSTRREF and S_LSTRVAL.
    L           : Integer; // In the latter case, the actual string is stored
    P           : Pointer; // beginning at P, so the structure requires >8 bytes.
  end;
  PLStr         = ^TLStr;

const
  S_NIL         = 0; // internal types, stored in DT
  S_BOOLEAN     = 1;
  S_POINTER     = 2;
  S_INTEGER     = 3;
  S_DOUBLE      = 4;
  S_STRREF      = 5;
  S_LSTRREF     = 6;
  S_LSTRVAL     = 7;

////////////////////////////////////////////////////////////////////////////////
constructor TStack.Create;
begin
  inherited Create;
  DT:=TList.Create;
  DA:=TList.Create;
  Marker:=0;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.Free;
begin
  SetTop(0);
  DT.Free;
  DA.Free;
  inherited Free;
end;

////////////////////////////////////////////////////////////////////////////////
function TStack.GetIndex(Index : Integer; out Res : Integer) : LongBool;
// converts logical index (may be negative) to actual index (zero-based)
begin
  if Index>=0 then Res:=Index-1
  else Res:=DT.Count+Index;
  Result:=(Res>=0)and(Res<DT.Count); // return false if index out of bounds
end;

////////////////////////////////////////////////////////////////////////////////
function TStack.GetTop : Integer;
// returns logical index of last item on stack (= number of items)
begin
  Result:=DT.Count;
end;

////////////////////////////////////////////////////////////////////////////////
function TStack.GetType(Index : Integer) : Integer;
// converts internal to external type
var
  i : Integer;
begin
  Result:=T_NIL; // default is nil
  if GetIndex(Index,i) then
  case Cardinal(DT[i]) of
    S_BOOLEAN          : Result:=T_BOOLEAN;
    S_POINTER          : Result:=T_POINTER;
    S_INTEGER,S_DOUBLE : Result:=T_NUMBER;
    S_STRREF,S_LSTRREF,
    S_LSTRVAL          : Result:=T_STRING;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.Insert(Index : Integer);
// insert top item at specified index (actually moves item)
var
  i,t : Integer;
begin
  if not GetIndex(Index,i) then Exit; // target index must be valid
  t:=DT.Count-1;
  DT.Insert(i,DT[t]);
  DA.Insert(i,DA[t]);
  DT.Delete(t+1);
  DA.Delete(t+1);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushNil;
// push NIL to top of stack
begin
  DT.Add(Pointer(S_NIL));
  DA.Add(nil);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushBoolean(Value : LongBool);
// push boolean to top of stack
begin
  DT.Add(Pointer(S_BOOLEAN));
  DA.Add(Pointer(Value));
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushPointer(Value : Pointer);
// push pointer to top of stack
begin
  DT.Add(Pointer(S_POINTER));
  DA.Add(Value);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushPtrOrNil(Value : Pointer);
// push pointer to top of stack, replace with NIL if pointer is zero
begin
  if Value=nil then PushNil
  else PushPointer(Value);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushInteger(Value : Integer);
// push integer to top of stack
begin
  DT.Add(Pointer(S_INTEGER));
  DA.Add(Pointer(Value));
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushDouble(Value : Double);
// push double to top of stack
var
  P : ^Double;
begin
  New(P); // reserve 8 bytes
  P^:=Value;
  DT.Add(Pointer(S_DOUBLE));
  DA.Add(P);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushStrRef(Value : PAnsiChar);
// push string reference to top of stack
begin
  DT.Add(Pointer(S_STRREF));
  DA.Add(Value);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushStrVal(Value : PAnsiChar);
// push string value to top of stack
begin
  PushLStrVal(Value,Length(Value)); // store internally as length-string
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushLStrRef(Value : PAnsiChar; Len : Integer);
// push length-string reference to top of stack
var
  Buf : PLStr;
begin
  New(Buf); // reserve 8 bytes
  Buf^.L:=Len;
  Buf^.P:=Value;
  DT.Add(Pointer(S_LSTRREF));
  DA.Add(Buf);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushLStrVal(Value : PAnsiChar; Len : Integer);
// push length-string value to top of stack
var
  Buf : PLStr;
begin
  GetMem(Buf,Len+5); // reserve space (>8 bytes!)
  Buf^.L:=Len;
  Move(Value^,Buf^.P,Len); // overwrite memory starting at P
  PAnsiChar(Buf)[Len+5]:=#0; // make sure string is zero-terminated
  DT.Add(Pointer(S_LSTRVAL));
  DA.Add(Buf);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.PushValue(Index : Integer);
// push/duplicate item at specified index to top of stack
var
  i : Integer;
begin
  if GetIndex(Index,i) then
  case Cardinal(DT[i]) of
    S_DOUBLE  : PushDouble(Double(DA[i]^));
    S_LSTRREF : with PLStr(DA[i])^ do PushLStrRef(P,L);
    S_LSTRVAL : with PLStr(DA[i])^ do PushLStrVal(@P,L);
    else begin
      DT.Add(DT[i]);
      DA.Add(DA[i]);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
function TStack.GetBoolean(Index : Integer) : LongBool;
// return boolean at index
var
  i : Integer;
begin
  Result:=False; // default is false
  if GetIndex(Index,i) then
    if Cardinal(DT[i])=S_BOOLEAN then
      Result:=LongBool(DA[i]);
end;

////////////////////////////////////////////////////////////////////////////////
function TStack.GetPointer(Index : Integer) : Pointer;
// return pointer at index
var
  i : Integer;
begin
  Result:=nil; // default is nil
  if GetIndex(Index,i) then
    if Cardinal(DT[i])=S_POINTER then
      Result:=DA[i];
end;

////////////////////////////////////////////////////////////////////////////////
function TStack.GetInteger(Index : Integer) : Integer;
// return integer at index
var
  i : Integer;
begin
  Result:=0; // default is 0
  if GetIndex(Index,i) then
  case Cardinal(DT[i]) of
    S_INTEGER : Result:=Integer(DA[i]);
    S_DOUBLE  : Result:=Trunc(Double(DA[i]^));
  end;
end;

////////////////////////////////////////////////////////////////////////////////
function TStack.GetDouble(Index : Integer) : Double;
// return double at index
var
  i : Integer;
begin
  Result:=0; // default is 0
  if GetIndex(Index,i) then
  case Cardinal(DT[i]) of
    S_INTEGER : Result:=Integer(DA[i]);
    S_DOUBLE  : Result:=Double(DA[i]^);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
function TStack.GetString(Index : Integer) : PAnsiChar;
// return string at index
var
  i : Integer;
begin
  Result:=''; // default is ''
  if GetIndex(Index,i) then
  case Cardinal(DT[i]) of
    S_STRREF  : Result:=DA[i];
    S_LSTRREF : with PLStr(DA[i])^ do Result:=P;
    S_LSTRVAL : with PLStr(DA[i])^ do Result:=@P;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
function TStack.GetLString(Index : Integer; out Len : Integer) : PAnsiChar;
// return length-string at index
var
  i : Integer;
begin
  Len:=0; // set defaults
  Result:=PAnsiChar('');
  if GetIndex(Index,i) then
  case Cardinal(DT[i]) of
    S_STRREF  : begin
                  Result:=DA[i];
                  Len:=Length(Result);
                end;
    S_LSTRREF : with PLStr(DA[i])^ do
                begin
                  Result:=P;
                  Len:=L;
                end;
    S_LSTRVAL : with PLStr(DA[i])^ do
                begin
                  Result:=@P;
                  Len:=L;
                end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.Remove(Index : Integer);
// remove item at index
var
  i : Integer;
begin
  if not GetIndex(Index,i) then Exit;
  case Cardinal(DT[i]) of // release memory
    S_DOUBLE  : Dispose(PDouble(DA[i]));
    S_LSTRREF : Dispose(PLStr(DA[i]));
    S_LSTRVAL : FreeMem(DA[i]);
  end;
  DT.Delete(i);
  DA.Delete(i);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.SetTop(Index : Integer);
// grow or shrink stack to specified index
var
  i : Integer;
begin
  GetIndex(Index,i); // ignore result (allow invalid index)
  if i<-1 then i:=-1;
  while DT.Count-1<i do
    PushNil;
  while DT.Count-1>i do
    Remove(-1);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.Mark;
// mark/remember current top index
begin
  Marker:=GetTop;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.Clean;
// remove all elements up to marked
var
  i : Integer;
begin
  for i:=1 to Marker do
    Remove(1);
  Marker:=0;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStack.MoveTo(Target : TStack; Index,Cnt : Integer);
// move items to another stack
var
  i,j : Integer;
begin
  if not GetIndex(Index,i) then Exit;
  for j:=1 to Cnt do
  begin
    if i>=DT.Count then Break; // check cnt
    Target.DT.Add(DT[i]);
    Target.DA.Add(DA[i]);
    DT.Delete(i);
    DA.Delete(i);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
end.
