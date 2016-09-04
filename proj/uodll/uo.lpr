library uo;
{$mode objfpc}{$H+}
uses wrapper;

////////////////////////////////////////////////////////////////////////////////
function Version : Integer; stdcall;
begin
  Result:=3;
end;

////////////////////////////////////////////////////////////////////////////////
function Open : Integer; stdcall;
begin
  Result:=Integer(TUOWrap.Create);
end;

////////////////////////////////////////////////////////////////////////////////
procedure Close(Hnd : Integer); stdcall;
begin
  TUOWrap(Hnd).Free;
end;

////////////////////////////////////////////////////////////////////////////////
function Query(Hnd : Integer) : Integer; stdcall;
begin
  Result:=TUOWrap(Hnd).Query;
end;

////////////////////////////////////////////////////////////////////////////////
function Execute(Hnd : Integer) : Integer; stdcall;
begin
  Result:=TUOWrap(Hnd).Execute;
end;

////////////////////////////////////////////////////////////////////////////////
function GetTop(Hnd : Integer) : Integer; stdcall;
begin
  Result:=TUOWrap(Hnd).Stack.GetTop;
end;

////////////////////////////////////////////////////////////////////////////////
function GetType(Hnd,Index : Integer) : Integer; stdcall;
begin
  Result:=TUOWrap(Hnd).Stack.GetType(Index);
end;

////////////////////////////////////////////////////////////////////////////////
procedure Insert(Hnd,Index : Integer); stdcall;
begin
  TUOWrap(Hnd).Stack.Insert(Index);
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushNil(Hnd : Integer); stdcall;
begin
  TUOWrap(Hnd).Stack.PushNil;
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushBoolean(Hnd : Integer; Value : LongBool); stdcall;
begin
  TUOWrap(Hnd).Stack.PushBoolean(Value);
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushPointer(Hnd : Integer; Value : Pointer); stdcall;
begin
  TUOWrap(Hnd).Stack.PushPointer(Value);
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushPtrOrNil(Hnd : Integer; Value : Pointer); stdcall;
begin
  TUOWrap(Hnd).Stack.PushPtrOrNil(Value);
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushInteger(Hnd,Value : Integer); stdcall;
begin
  TUOWrap(Hnd).Stack.PushInteger(Value);
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushDouble(Hnd : Integer; Value : Double); stdcall;
begin
  TUOWrap(Hnd).Stack.PushDouble(Value);
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushStrRef(Hnd : Integer; Value : PChar); stdcall;
begin
  TUOWrap(Hnd).Stack.PushStrRef(Value);
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushStrVal(Hnd : Integer; Value : PChar); stdcall;
begin
  TUOWrap(Hnd).Stack.PushStrVal(Value);
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushLStrRef(Hnd : Integer; Value : PChar; Len : Integer); stdcall;
begin
  TUOWrap(Hnd).Stack.PushLStrRef(Value,Len);
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushLStrVal(Hnd : Integer; Value : PChar; Len : Integer); stdcall;
begin
  TUOWrap(Hnd).Stack.PushLStrVal(Value,Len);
end;

////////////////////////////////////////////////////////////////////////////////
procedure PushValue(Hnd,Index : Integer); stdcall;
begin
  TUOWrap(Hnd).Stack.PushValue(Index);
end;

////////////////////////////////////////////////////////////////////////////////
function GetBoolean(Hnd,Index : Integer) : LongBool; stdcall;
begin
  Result:=TUOWrap(Hnd).Stack.GetBoolean(Index);
end;

////////////////////////////////////////////////////////////////////////////////
function GetPointer(Hnd,Index : Integer) : Pointer; stdcall;
begin
  Result:=TUOWrap(Hnd).Stack.GetPointer(Index);
end;

////////////////////////////////////////////////////////////////////////////////
function GetInteger(Hnd,Index : Integer) : Integer; stdcall;
begin
  Result:=TUOWrap(Hnd).Stack.GetInteger(Index);
end;

////////////////////////////////////////////////////////////////////////////////
function GetDouble(Hnd,Index : Integer) : Double; stdcall;
begin
  Result:=TUOWrap(Hnd).Stack.GetDouble(Index);
end;

////////////////////////////////////////////////////////////////////////////////
function GetString(Hnd,Index : Integer) : PChar; stdcall;
begin
  Result:=TUOWrap(Hnd).Stack.GetString(Index);
end;

////////////////////////////////////////////////////////////////////////////////
function GetLString(Hnd,Index : Integer; var Len : Integer) : PChar; stdcall;
begin
  Result:=TUOWrap(Hnd).Stack.GetLString(Index,Len);
end;

////////////////////////////////////////////////////////////////////////////////
procedure Remove(Hnd,Index : Integer); stdcall;
begin
  TUOWrap(Hnd).Stack.Remove(Index);
end;

////////////////////////////////////////////////////////////////////////////////
procedure SetTop(Hnd,Index : Integer); stdcall;
begin
  TUOWrap(Hnd).Stack.SetTop(Index);
end;

////////////////////////////////////////////////////////////////////////////////
procedure Mark(Hnd : Integer); stdcall;
begin
  TUOWrap(Hnd).Stack.Mark;
end;

////////////////////////////////////////////////////////////////////////////////
procedure Clean(Hnd : Integer); stdcall;
begin
  TUOWrap(Hnd).Stack.Clean;
end;

////////////////////////////////////////////////////////////////////////////////
exports
  Version      ,Version      name '_UOVersion@0',
  Open         ,Open         name '_UOOpen@0',
  Close        ,Close        name '_UOClose@4',
  Query        ,Query        name '_UOQuery@4',
  Execute      ,Execute      name '_UOExecute@4',
  GetTop       ,GetTop       name '_UOGetTop@4',
  GetType      ,GetType      name '_UOGetType@8',
  Insert       ,Insert       name '_UOInsert@8',
  PushNil      ,PushNil      name '_UOPushNil@4',
  PushBoolean  ,PushBoolean  name '_UOPushBoolean@8',
  PushPointer  ,PushPointer  name '_UOPushPointer@8',
  PushPtrOrNil ,PushPtrOrNil name '_UOPushPtrOrNil@8',
  PushInteger  ,PushInteger  name '_UOPushInteger@8',
  PushDouble   ,PushDouble   name '_UOPushDouble@12',
  PushStrRef   ,PushStrRef   name '_UOPushStrRef@8',
  PushStrVal   ,PushStrVal   name '_UOPushStrVal@8',
  PushLStrRef  ,PushLStrRef  name '_UOPushLStrRef@12',
  PushLStrVal  ,PushLStrVal  name '_UOPushLStrVal@12',
  PushValue    ,PushValue    name '_UOPushValue@8',
  GetBoolean   ,GetBoolean   name '_UOGetBoolean@8',
  GetPointer   ,GetPointer   name '_UOGetPointer@8',
  GetInteger   ,GetInteger   name '_UOGetInteger@8',
  GetDouble    ,GetDouble    name '_UOGetDouble@8',
  GetString    ,GetString    name '_UOGetString@8',
  GetLString   ,GetLString   name '_UOGetLString@12',
  Remove       ,Remove       name '_UORemove@8',
  SetTop       ,SetTop       name '_UOSetTop@8',
  Mark         ,Mark         name '_UOMark@4',
  Clean        ,Clean        name '_UOClean@4';

////////////////////////////////////////////////////////////////////////////////
begin
  IsMultiThread:=True; //IMPORTANT: Activates multi-threading support
  Set8087CW($27F);     //IMPORTANT: Disables floating point exceptions
end.
