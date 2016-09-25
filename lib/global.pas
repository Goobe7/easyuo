unit global;
interface
uses Windows, SysUtils;

  // This unit provides useful functions and is used globally in various projects.

  function ReadMem(PHnd : Cardinal; MemPos : Cardinal; Buf : PAnsiChar; Size : Cardinal) : Boolean;
  function WriteMem(PHnd : Cardinal; MemPos : Cardinal; Buf : PAnsiChar; Size : Cardinal) : Boolean;
  function SearchMem(PHnd : Cardinal; ScanStr : AnsiString; Joker : AnsiChar) : Cardinal;
  function NumStr(Value, Size : Cardinal; Intel : Boolean) : AnsiString;
  function GetVK(KeyStr : AnsiString) : Cardinal;

implementation

////////////////////////////////////////////////////////////////////////////////
function ReadMem(PHnd : Cardinal; MemPos : Cardinal; Buf : PAnsiChar; Size : Cardinal) : Boolean;
// reads the maximum amount of memory possible and zeroes the rest
var
  a : Cardinal; // lower bound for end position (reading here works)
  b : Cardinal; // upper bound for end position (reading here fails)
  m : Cardinal; // median
  c : Cardinal; // bytes read
begin
  Result:=False;
  ZeroMemory(Buf,Size);
  a:=0;
  b:=Size;
  m:=Size;

  // To account for different clients, we sometimes read more memory than
  // necessary. E.g. if the desired block is 200 bytes large, we may read 300
  // bytes. But if the block lies right at the end of a region, we may read into
  // invalid space and ReadProcessMemory fails. So we use binary search to solve
  // this (e.g. we read the 220 bytes that really exist followed by 80 zeroes).

  repeat
    {$HINTS OFF}
    ReadProcessMemory(PHnd,Pointer(MemPos),Buf,m,c);
    {$HINTS ON}
    if c=0 then b:=m
    else begin
      Result:=True;
      a:=m;
    end;
    if b-a<2 then Break;
    m:=a + (b-a) div 2;
  until False;
end;

////////////////////////////////////////////////////////////////////////////////
function WriteMem(PHnd : Cardinal; MemPos : Cardinal; Buf : PAnsiChar; Size : Cardinal) : Boolean;
// simplified arguments and type-casts for convenience
var
  c : Cardinal;
begin
  {$HINTS OFF}
  Result:=WriteProcessMemory(PHnd,Pointer(MemPos),Buf,Size,c);
  {$HINTS ON}
end;

////////////////////////////////////////////////////////////////////////////////
function FindPos(PBuf, PScanStr : Pointer; BufLen, ScanLen : Cardinal; Joker : Byte) : Integer;
// scans a single block and returns position of first occurence (or -1 otherwise),
// a wildcard can be specified to ignore certain characters in the string
var
  i : Integer;
begin
  {$ASMMODE intel}
  asm
    push  edi
    push  esi
    push  ebp
    push  ebx

    mov   eax, -1
    mov   edi, PBuf
    mov   ecx, 0

    mov   edx, edi
    add   edx, BufLen
    sub   edx, ScanLen

    @N1:
    mov   esi, PScanStr
    sub   edi, ecx
    mov   ecx, 0

    cmp   edi, edx
    jg    @W1

    @N2:
    mov   bl, Joker
    cmp   [esi], bl
    jne   @W3
    cmpsb
    jmp   @W2
    @W3:

    cmpsb
    jne   @N1

    @W2:
    inc   ecx
    cmp   ecx, ScanLen
    jne   @N2

    sub   edi, ScanLen
    mov   eax, edi
    sub   eax, PBuf
    @W1:
    mov   i, eax

    pop   ebx
    pop   ebp
    pop   esi
    pop   edi
  end;

  Result:=i;
end;

////////////////////////////////////////////////////////////////////////////////
function SearchMem(PHnd : Cardinal; ScanStr : AnsiString; Joker : AnsiChar) : Cardinal;
// searches the program area for a string
const
  MEM_START = $00400000; // start of code for most windows programs
  MEM_END   = $00680000; // client code usually ends around here
  MEM_BLOCK = $00010000; // 64k single block size
var
  i,j : Integer;
  Buf : array[0..MEM_BLOCK-1] of Byte;
begin
  i:=MEM_START;
  j:=-1;

  // scan all memory blocks for first occurence of ScanStr
  repeat
    if not ReadMem(PHnd,i,@Buf,MEM_BLOCK) then Break;
    j:=FindPos(@Buf,@ScanStr[1],MEM_BLOCK,Length(ScanStr),Byte(Joker));
    if j>=0 then Break;
    i:=i+MEM_BLOCK-Length(ScanStr)+1; // subtract length to account for overlap
  until i>MEM_END;

  Result:=0;
  if j>=0 then Result:=i+j;
end;

////////////////////////////////////////////////////////////////////////////////
function NumStr(Value, Size : Cardinal; Intel : Boolean) : AnsiString;
// type-converts a value into a string for easy concatenation
var
  s : AnsiString;
  c : AnsiChar;
begin
  SetLength(s,Size);
  Move(Value,s[1],Size);

  // switch byte order if format is not little-endian (Intel)
  if not Intel then
  case Size of
    4 : begin
          c:=s[1];
          s[1]:=s[4];
          s[4]:=c;
          c:=s[2];
          s[2]:=s[3];
          s[3]:=c;
        end;
    2 : begin
          c:=s[1];
          s[1]:=s[2];
          s[2]:=c;
        end;
  end;

  Result:=s;
end;

////////////////////////////////////////////////////////////////////////////////
function GetVK(KeyStr : AnsiString) : Cardinal;
// converts string into virtual key code
const
  Keys : array[0..22] of AnsiString = (
    'f10','f11','f12','esc','back','tab','enter','pause','capslock','space',
    'pgup','pgdn','end','home','left','right','up','down','prnscr','insert',
    'delete','numlock','scrolllock');
  Code : array[0..22] of Cardinal = (
    VK_F10,VK_F11,VK_F12,VK_ESCAPE,VK_BACK,VK_TAB,VK_RETURN,VK_PAUSE,
    VK_CAPITAL,VK_SPACE,VK_PRIOR,VK_NEXT,VK_END,VK_HOME,VK_LEFT,VK_RIGHT,
    VK_UP,VK_DOWN,VK_SNAPSHOT,VK_INSERT,VK_DELETE,VK_NUMLOCK,VK_SCROLL);
var
  i : Integer;
  s : AnsiString;
begin
  s:=LowerCase(KeyStr);
  Result:=0;

  // normal key?
  if Length(s)=1 then
    if s[1] in ['a'..'z','0'..'9'] then
      Result:=Byte(s[1])-32;

  // f1..f9?
  if Length(s)=2 then
    if (s[1]='f')and(s[2] in ['1'..'9']) then
      Result:=Byte(VK_F1+Byte(s[2])-49);

  // special key?
  if Result=0 then
    for i:=0 to High(Keys) do
      if s=Keys[i] then
        Result:=Code[i];
end;

////////////////////////////////////////////////////////////////////////////////
end.
