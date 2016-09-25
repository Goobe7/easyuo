unit main;
{$mode objfpc}{$H+}
interface
uses Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
     StdCtrls, commands, comm;

type
  TMainForm = class(TForm)
    HTTPPostButton: TButton;
    HTTPGetButton: TButton;
    SoundButton: TButton;
    ShutdownButton: TButton;
    ExecuteButton: TButton;
    SetCliTitleButton: TButton;
    WaitButton: TButton;
    HotkeyButton: TButton;
    procedure ExecuteButtonClick(Sender: TObject);
    procedure HTTPGetButtonClick(Sender: TObject);
    procedure OnHotkeyButtonClick(Sender: TObject);
    procedure HTTPPostButtonClick(Sender: TObject);
    procedure SetCliTitleButtonClick(Sender: TObject);
    procedure ShutdownButtonClick(Sender: TObject);
    procedure SoundButtonClick(Sender: TObject);
    procedure WaitButtonClick(Sender: TObject);
  end;

var
  MainForm  : TMainForm;
  Cmd       : TCmd;
  HTTP      : THTTP;

implementation
{$R *.lfm}

////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.WaitButtonClick(Sender: TObject);
begin
  Cmd.Wait(3000,0);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.OnHotkeyButtonClick(Sender: TObject);
var
  s : AnsiString;
begin
  s:=' NOT ';
  if Cmd.OnHotkey('F2',False,False,False) then s:=' ';
  ShowMessage('F2 is currently'+s+'being pressed.');
end;

////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.ShutdownButtonClick(Sender: TObject);
begin
  if MessageBox(Handle, 'Shutdown computer?', 'Confirmation',
    MB_ICONQUESTION+MB_YESNO)=IDYES then Cmd.Shutdown;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.ExecuteButtonClick(Sender: TObject);
begin
  Cmd.Execute('cmd.exe','/c pause',True,0);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SoundButtonClick(Sender: TObject);
begin
  Cmd.Sound;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SetCliTitleButtonClick(Sender: TObject);
begin
  Cmd.SetCliTitle(Handle,IntToStr(Random(1000)));
end;

////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.HTTPGetButtonClick(Sender: TObject);
begin
  HTTP.Get('www.google.com','/search?num=20&q=easyuo+openeuo');
  MessageBox(Handle,PAnsiChar(HTTP.Request),'Request',0);
  MessageBox(Handle,PAnsiChar(HTTP.Response),'Response',0);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.HTTPPostButtonClick(Sender: TObject);
begin
  HTTP.Post('www.google.com','/search','num=20&q=easyuo+openeuo');
  MessageBox(Handle,PAnsiChar(HTTP.Request),'Request',0);
  MessageBox(Handle,PAnsiChar(HTTP.Response),'Response',0);
end;

////////////////////////////////////////////////////////////////////////////////
initialization
  Randomize;
  Cmd:=TCmd.Create(nil);
  HTTP:=THTTP.Create;
finalization
  HTTP.Free;
  Cmd.Free;
end.

