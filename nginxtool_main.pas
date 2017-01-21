unit nginxtool_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormNginxtool }

  TFormNginxtool = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private

  public
    procedure VerboseNginxConfig;
  end;

var
  FormNginxtool: TFormNginxtool;

implementation

uses
  {$ifdef WINDOWS}windows,{$endif} loglistfpc, sockets, RegExpr, process {$ifdef WINDOWS}, JwaPsApi{$endif};

var
  loglist:TLogListFPC;


{$R *.lfm}


procedure GetIPAddr(var buf: array of char; const len: longint);
const
 CN_GDNS_ADDR = '127.0.0.1';
 CN_GDNS_PORT = 53;
var
 s: string;
 sock: longint;
 err: longint;
 HostAddr: TSockAddr;
 l: Integer;
 IPAddr: TInetSockAddr;

begin
 err := 0;
 Assert(len >= 16);

 sock := fpsocket(AF_INET, SOCK_DGRAM, 0);
 assert(sock <> -1);

 IPAddr.sin_family := AF_INET;
 IPAddr.sin_port := htons(CN_GDNS_PORT);
 IPAddr.sin_addr.s_addr := StrToHostAddr(CN_GDNS_ADDR).s_addr;

 if (fpConnect(sock,@IPAddr,SizeOf(IPAddr)) = 0) then
 begin
   try
     l := SizeOf(HostAddr);
     if (fpgetsockname(sock, @HostAddr, @l) = 0) then
     begin
       s := NetAddrToStr(HostAddr.sin_addr);
       StrPCopy(PChar(Buf), s);
     end
     else
     begin
       err:=socketError;
     end;
   finally
     if (CloseSocket(sock) <> 0) then
     begin
       err := socketError;
     end;
   end;
 end
 else
 begin
   err:=socketError;
 end;
end;

{$ifdef WINDOWS}
function checkEnumProcess(const name:string):Boolean;
var
   processids:array[0..1023] of DWord;
   cbNeed, pCount, i:DWord;
   ProcessName:array[0..MAX_PATH-1] of char;
   function checkProcessName(pid:DWORD):Boolean;
   var
     ph:HANDLE;
     pm:HMODULE;
     cbval:DWORD;
   begin
     result:=false;
     ph:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, processids[i]);
     if ph<>0 then begin
       if EnumProcessModules(ph,@pm,sizeof(pm),cbval) then begin
         GetModuleBaseName(ph,pm,ProcessName,sizeof(ProcessName) div SizeOf(char));
         result:=CompareText(name,strpas(ProcessName))=0;
       end;
     end;
     CloseHandle(ph);
   end;
begin
  result:=false;
  if EnumProcesses(@processids[0],sizeof(processids),cbNeed) then begin
    pCount:=cbNeed div sizeof(DWord);
    for i:=0 to pCount-1 do begin
      if processids[i]<>0 then begin
        result:=checkProcessName(processids[i]);
        if result then
           break;
      end;
    end;
  end;
end;
{$endif}

{ TFormNginxtool }

procedure TFormNginxtool.Panel1Click(Sender: TObject);
begin

end;

procedure TFormNginxtool.VerboseNginxConfig;
var
 buf : string;
 fs : TFileStream;
 bufsize, bufloc, bufpos, bufopen : Integer;
 rx : TRegExpr;
 templist : TStringList;
begin
 try
   // read config file
   fs := TFileStream.Create('conf/nginx.conf',fmOpenRead or fmShareDenyNone);
   try
     bufsize := fs.Size;
     if bufsize>65530 then
        bufsize:=65530;
     SetLength(buf,bufsize);
     fs.Read(buf[1],fs.Size);
   finally
     fs.Free;
   end;
 except
   buf:='';
 end;
 // parse config file
 rx := TRegExpr.Create('rtmp\s+\{');
 try
   rx.ModifierI:=true;
   if rx.Exec(buf) then begin
     bufloc := rx.MatchPos[0];
     bufpos := rx.MatchPos[0]+rx.MatchLen[0];
     bufopen:=1;
     while bufopen>0 do begin
       if buf[bufpos]='{' then
          Inc(bufopen)
          else if buf[bufpos]='}' then
               dec(bufopen);
       Inc(bufpos);
       if bufpos>bufsize then
          break;
     end;
     templist:=TStringList.Create;
     try
       buf:=Copy(buf,bufloc,bufpos-bufloc);
       buf:=StringReplace(buf,#9,#32#32#32#32,[rfReplaceAll]);
       templist.Text:=buf;
       for bufpos:=0 to templist.Count-1 do begin
         loglist.AddLog(templist.Strings[bufpos]);
       end;
     finally
       templist.Free;
     end;
   end;
 finally
   rx.Free;
 end;
end;

procedure TFormNginxtool.FormCreate(Sender: TObject);
begin
  loglist:=TLogListFPC.Create(Self);
  loglist.Name:='loglist1';
  loglist.Parent:=Panel1;
  loglist.Align:=alClient;
  loglist.Color:=clWhite;
  loglist.LineLimit:=1000;
end;

procedure TFormNginxtool.Button1Click(Sender: TObject);
var
  myprocess:TProcess;
  i:integer;
begin
  {$ifdef WINDOWS}
  if checkEnumProcess('nginx.exe') then begin
     loglist.AddLog('> Already running!');
     exit;
  end;
  {$endif}
  myprocess:=TProcess.Create(nil);
  try
    myprocess.InheritHandles:=false;
    myprocess.Options:=[];
    myprocess.ShowWindow:=swoShow;
    for i:=1 to GetEnvironmentVariableCount do
      myprocess.Environment.Add(GetEnvironmentString(i));
    myprocess.Executable:='nginx';
    try
      myprocess.Execute;
      loglist.AddLog('> nginx started');
    except
      on e:exception do
         loglist.AddLog('> Error: '+e.Message);
    end;
  finally
    myprocess.Free;
  end;
end;

procedure TFormNginxtool.Button2Click(Sender: TObject);
var
  myprocess:TProcess;
  i : integer;
begin
  {$ifdef WINDOWS}
  if not checkEnumProcess('nginx.exe') then begin
     loglist.AddLog('> nginx is not running');
     exit;
  end;
  {$endif}
  myprocess:=TProcess.Create(nil);
  try
    myprocess.InheritHandles:=false;
    myprocess.Options:=[];
    myprocess.ShowWindow:=swoShow;
    for i:=1 to GetEnvironmentVariableCount do
      myprocess.Environment.Add(GetEnvironmentString(i));
    myprocess.Executable:='nginx';
    myprocess.Parameters.Add('-s');
    myprocess.Parameters.Add('stop');
    try
      myprocess.Execute;
      loglist.AddLog('> nginx stopped');
    except
      on e:exception do
         loglist.AddLog('> Error: '+e.Message);
    end;
  finally
    myprocess.Free;
  end;
end;

procedure TFormNginxtool.FormShow(Sender: TObject);
var
  IPBuf:array[0..254] of char;
begin
  VerboseNginxConfig;
  GetIPAddr(IPBuf,sizeof(IPBuf));
  loglist.AddLog(Format('> IP Address: %s',[IPBuf]));
end;

end.

