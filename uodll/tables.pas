unit tables;
interface
uses SysUtils, stack;

type
  TItem         = record
    N           : PAnsiChar;
    T           : Integer;
    C           : Integer;
    P           : PAnsiChar;
  end;
  AItem         = array[0..999] of TItem;
  PAItem        = ^AItem;
  TFindRes      = record
    Tbl         : PAItem;
    Idx         : Integer;
    Cnt         : Integer;
  end;

const
  RO = 01;
  RW = 02;
  ME = 04;
  EV = 08;
  CM = 16;
  QY = 32;

  function Find_First(out Res : TFindRes; Name : String; Table : PAItem; Cnt : Integer) : Boolean;
  function Find_Next(var Res : TFindRes) : Boolean;
  function ParComp(Stack : TStack; Idx : Integer; Mask : PAnsiChar) : Boolean;

implementation

////////////////////////////////////////////////////////////////////////////////
function Find_First(out Res : TFindRes; Name : String; Table : PAItem; Cnt : Integer) : Boolean;
// finds first entry using binary search
var
  a,m,z : Integer;
begin
  Res.Tbl:=Table;
  Res.Idx:=-1;
  Res.Cnt:=Cnt;
  Result:=False;

  // binary search
  a:=0;
  z:=Cnt-1;
  while a<z do
  begin
    m:=a+(z-a)shr 1;
    if CompareStr(Table^[m].N,Name)<0 then a:=m+1
    else z:=m;
  end;

  // find first occurence
  repeat
    if Table^[a].N<>Name then Exit;
    Result:=True;
    Res.Idx:=a;
    Dec(a);
  until a<0;
end;

////////////////////////////////////////////////////////////////////////////////
function Find_Next(var Res : TFindRes) : Boolean;
// loads next entry
begin
  Result:=False;
  Inc(Res.Idx);
  with Res do
    if Idx<Cnt then
      Result:=CompareStr(Tbl^[Idx].N,Tbl^[Idx-1].N)=0;
end;

////////////////////////////////////////////////////////////////////////////////
function ParComp(Stack : TStack; Idx : Integer; Mask : PAnsiChar) : Boolean;
//check if parameters on the stack match the specified string mask
var
  j   : Integer;
begin
  Result:=False;
  Dec(Idx);
  j:=-1;

  repeat
    Inc(Idx);
    Inc(j);

    // handle special characters
    case Mask[j] of
      '?': Continue;
      '*': if Stack.GetTop()>=Idx-1 then Break
           else Exit;
      #0 : if Stack.GetTop()=Idx-1 then Break
           else Exit;
    end;

    // compare parameters
    case Stack.GetType(Idx) of
      T_NIL    : if Mask[j]<>'-' then Exit;
      T_BOOLEAN: if Mask[j]<>'b' then Exit;
      T_NUMBER : if Mask[j]<>'n' then Exit;
      T_STRING : if Mask[j]<>'s' then Exit;
      T_POINTER: if Mask[j]<>'p' then Exit;
    end;

  until False;
  Result:=True;
end;

////////////////////////////////////////////////////////////////////////////////
end.
