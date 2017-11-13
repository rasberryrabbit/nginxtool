unit nginxtool_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, JSONPropStorage, UniqueInstance;

type

  { TFormNginxtool }

  TFormNginxtool = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox_priority: TCheckBox;
    CheckBoxModConf: TCheckBox;
    ComboBox_meta: TComboBox;
    ComboBoxChunk: TComboBox;
    ComboBox_waitvideo: TComboBox;
    ComboBox_waitkey: TComboBox;
    GroupBox1: TGroupBox;
    JSONPropStorage1: TJSONPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Timer1: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBoxModConfClick(Sender: TObject);
    procedure ComboBoxChunkCloseUp(Sender: TObject);
    procedure ComboBoxChunkKeyPress(Sender: TObject; var Key: char);
    procedure ComboBox_metaCloseUp(Sender: TObject);
    procedure ComboBox_waitvideoCloseUp(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    procedure VerboseNginxConfig;
    function CheckSettingChange:Boolean;
  end;

var
  FormNginxtool: TFormNginxtool;

implementation

uses
  {$ifdef WINDOWS}windows,{$endif} loglistfpc, sockets, RegExpr, process {$ifdef WINDOWS}, JwaPsApi{$endif},
  DefaultTranslator;

var
  loglist:TLogListFPC;
  {$ifdef WINDOWS}
  UseAboveNormalProcess:Boolean;
  {$endif}
  checkflag:string = '';
  ppval:DWORD;
  nginx_process_find:Integer=0;


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

procedure ProcessPriority(const name:string; priority:dword);
var
   processids:array[0..1023] of DWord;
   cbNeed, pCount, i:DWord;
   ProcessName:array[0..MAX_PATH-1] of char;
   function ProcessPriorityHandle(pid:DWORD):Boolean;
   var
     ph:HANDLE;
     pm:HMODULE;
     cbval:DWORD;
   begin
     result:=false;
     ph:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_SET_INFORMATION, false, processids[i]);
     if ph<>0 then begin
       if EnumProcessModules(ph,@pm,sizeof(pm),cbval) then begin
         GetModuleBaseName(ph,pm,ProcessName,sizeof(ProcessName) div SizeOf(char));
         if CompareText(name,strpas(ProcessName))=0 then begin
           if not (GetPriorityClass(ph)=priority) then
             if not SetPriorityClass(ph,priority) then begin
               GetLastError;
               loglist.AddLog(Format('Cannot set process priority %d',[ph]));
             end else begin
               Inc(nginx_process_find);
               loglist.AddLog(Format('Set process priority %d',[ph]));
             end;
         end;
       end;
     end;
     CloseHandle(ph);
   end;
begin
  if EnumProcesses(@processids[0],sizeof(processids),cbNeed) then begin
    pCount:=cbNeed div sizeof(DWord);
    for i:=0 to pCount-1 do begin
      if processids[i]<>0 then begin
        ProcessPriorityHandle(processids[i]);
      end;
    end;
  end;
end;
{$endif}

{ TFormNginxtool }

procedure TFormNginxtool.Panel1Click(Sender: TObject);
begin

end;

procedure TFormNginxtool.Timer1Timer(Sender: TObject);
begin
 {$ifdef WINDOWS}
 if CheckBox_priority.Checked then
   ProcessPriority('nginx.exe',ppval);
 {$endif}
 if nginx_process_find>0 then
   Timer1.Enabled:=False;
end;


function checkConfigComment(const s:string):Boolean;
var
  i,len:Integer;
begin
  Result:=False;
  i:=1;
  len:=Length(s);
  while i<=len do begin
    if s[i]>#32 then
      break;
    Inc(i);
  end;
  while i<=len do begin
    if s[i]='#' then begin
      Result:=True;
      break;
    end;
    Inc(i);
  end;
end;

procedure TFormNginxtool.VerboseNginxConfig;
var
 buf, bufrtmp, schunksize, sRtmpMeta : string;
 fs : TFileStream;
 bufsize, bufloc, bufpos, bufopen, rtmpidx, ii, ij : Integer;
 rx, rxrtmp : TRegExpr;
 templist : TStringList;
 workercount, chunksize : Integer;
 has_autopush, chunk_modified : Boolean;
 IPBuf:array[0..254] of char;
begin
 chunk_modified:=False;
 loglist.AddLog('----- nginx config -----');
 schunksize:=Trim(ComboBoxChunk.Text);
 if StrToIntDef(schunksize,0)=0 then begin
   schunksize:='4096';
   ComboBoxChunk.Text:=schunksize;
 end;
 try
   // read config file
   fs := TFileStream.Create('conf/nginx.conf',fmOpenRead or fmShareDenyNone);
   try
     bufsize := fs.Size;
     if bufsize>131072 then
        bufsize:=131072;
     SetLength(buf,bufsize);
     fs.Read(buf[1],bufsize);
   finally
     fs.Free;
   end;
 except
   buf:='';
 end;
   // check 'worker_processes 1;'
   rx := TRegExpr.Create('worker_processes\s+(\d+)\;');
   try
     rx.ModifierI:=True;
     if rx.Exec(buf) then begin
       workercount:=StrToIntDef(rx.Match[1],0);
       if CheckBoxModConf.Checked then begin
         if workercount<>1 then begin
           buf:=Copy(buf,1,rx.MatchPos[1]-1)+'1'+
                Copy(buf,rx.MatchPos[1]+rx.MatchLen[1]);
           chunk_modified:=True;
           loglist.AddLog('worker_processes 1;');
         end;
       end;
       if (not CheckBoxModConf.Checked) or (workercount=1) then
         loglist.AddLog(rx.Match[0]);
     end else begin
       workercount:=0;
       if CheckBoxModConf.Checked then begin
         buf:= 'worker_processes 1;'#10+buf;
         chunk_modified:=True;
         loglist.AddLog('worker_processes 1;');
       end;
     end;
   finally
     rx.Free;
   end;
   // check 'rtmp_auto_push on;'
   rx := TRegExpr.Create('rtmp_auto_push\s+([a-zA-Z]+)\;');
   try
     rx.ModifierI:=True;
     has_autopush:=False;
     if rx.Exec(buf) then begin
       has_autopush:=UpperCase(rx.Match[1])='ON';
       if CheckBoxModConf.Checked then begin
         //if not has_autopush then begin
           buf:=rx.Replace(buf,'',False);
         chunk_modified:=True;
         loglist.AddLog('rtmp_auto_push removed');
       end else
         loglist.AddLog(rx.Match[0]);
     end;
   finally
     rx.Free;
   end;
 if CheckBoxModConf.Checked then begin
   // check 'chunk_size 8192;'
   rx := TRegExpr.Create('rtmp\s+\{.+server\s+\{.+chunk_size\s+(\d+)\;');
   try
     rx.ModifierI:=True;
     if rx.Exec(buf) then begin
       if StrToIntDef(rx.Match[1],0)<>0 then begin
         buf:=Copy(buf,1,rx.MatchPos[1]-1)+schunksize+Copy(buf,rx.MatchPos[1]+rx.MatchLen[1]);
         chunk_modified:=True;
       end;
     end else begin
       rxrtmp:=TRegExpr.Create('rtmp\s+\{[^\{]+server\s+\{\s+[^;]+;');
       try
         rxrtmp.ModifierI:=True;
         if rxrtmp.Exec(buf) then begin
           buf:=Copy(buf,1,rxrtmp.MatchPos[0]+rxrtmp.MatchLen[0]-1)+#10#9#9'chunk_size '+schunksize+';'#10+
                Copy(buf,rxrtmp.MatchPos[0]+rxrtmp.MatchLen[0]);
           chunk_modified:=True;
         end;
       finally
         rxrtmp.Free;
       end;
     end;
   finally
     rx.Free;
   end;
   // insert meta copy
   rx := TRegExpr.Create('meta\s+[^;]+;');
   try
     rx.ModifierI:=True;
     if rx.Exec(buf) then begin
       buf:=rx.Replace(buf,'meta '+ComboBox_meta.Text+';',False);
       chunk_modified:=True;
     end else begin
         rxrtmp:=TRegExpr.Create('rtmp\s+\{[^\{]+server\s+\{\s+[^\{]+application\s+(\S+)\s+\{');
         try
           if rxrtmp.Exec(buf) then begin
             buf:=Copy(buf,1,rxrtmp.MatchPos[0]+rxrtmp.MatchLen[0]-1)+#10#9#9#9'meta '+ComboBox_meta.Text+';'#10+
                  Copy(buf,rxrtmp.MatchPos[0]+rxrtmp.MatchLen[0]);
             chunk_modified:=True;
           end;
         finally
           rxrtmp.Free;
         end;
       end;
   finally
     rx.Free;
   end;
   // wait_video
   rx := TRegExpr.Create('wait_video\s+[^;]+;');
   try
     rx.ModifierI:=True;
     if rx.Exec(buf) then begin
       buf:=rx.Replace(buf,'wait_video '+ComboBox_waitvideo.Text+';',False);
       chunk_modified:=True;
     end else begin
         rxrtmp:=TRegExpr.Create('rtmp\s+\{[^\{]+server\s+\{\s+[^\{]+application\s+(\S+)\s+\{');
         try
           if rxrtmp.Exec(buf) then begin
             buf:=Copy(buf,1,rxrtmp.MatchPos[0]+rxrtmp.MatchLen[0]-1)+#10#9#9#9'wait_video '+ComboBox_waitvideo.Text+';'#10+
                  Copy(buf,rxrtmp.MatchPos[0]+rxrtmp.MatchLen[0]);
             chunk_modified:=True;
           end;
         finally
           rxrtmp.Free;
         end;
       end;
   finally
     rx.Free;
   end;
   // wait_key
   rx := TRegExpr.Create('wait_key\s+[^;]+;');
   try
     rx.ModifierI:=True;
     if rx.Exec(buf) then begin
       buf:=rx.Replace(buf,'wait_key '+ComboBox_waitkey.Text+';',False);
       chunk_modified:=True;
     end else begin
         rxrtmp:=TRegExpr.Create('rtmp\s+\{[^\{]+server\s+\{\s+[^\{]+application\s+(\S+)\s+\{');
         try
           if rxrtmp.Exec(buf) then begin
             buf:=Copy(buf,1,rxrtmp.MatchPos[0]+rxrtmp.MatchLen[0]-1)+#10#9#9#9'wait_key '+ComboBox_waitkey.Text+';'#10+
                  Copy(buf,rxrtmp.MatchPos[0]+rxrtmp.MatchLen[0]);
             chunk_modified:=True;
           end;
         finally
           rxrtmp.Free;
         end;
       end;
   finally
     rx.Free;
   end;
   (*
   // add 'rtmp_auto_push on'
   if (workercount<>1) and (not has_autopush) then begin
     buf:='rtmp_auto_push on;'#10+buf;
     chunk_modified:=True;
     loglist.AddLog('rtmp_auto_push on; added');
   end;
   *)
   // update nginx.conf
   if chunk_modified then begin
     try
       fs:=TFileStream.Create('conf/nginx.conf',fmOpenReadWrite or fmShareDenyNone);
       try
         fs.Write(buf[1],Length(buf));
       finally
         fs.Free;
       end;
     except
       on e:exception do begin
         loglist.AddLog('Fail to write file "conf/nginx.conf"');
       end;
     end;
   end;
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
       bufrtmp:=Copy(buf,bufloc,bufpos-bufloc);
       bufrtmp:=StringReplace(bufrtmp,#9,#32#32#32#32,[rfReplaceAll]);
       templist.Text:=bufrtmp;
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
 GetIPAddr(IPBuf,sizeof(IPBuf));
 loglist.AddLog(Format('> IP Address: %s',[IPBuf]));
end;

function TFormNginxtool.CheckSettingChange: Boolean;
begin
 Result:=False;
 if not Result then
   Result:=CheckBox_priority.Checked<>JSONPropStorage1.ReadBoolean('priority',False);
 if not Result then
   Result:=CheckBoxModConf.Checked<>JSONPropStorage1.ReadBoolean('modify',True);
 if not Result then
   Result:=ComboBoxChunk.Text<>JSONPropStorage1.ReadString('chunk_size',ComboBoxChunk.Text);
 if not Result then
   Result:=ComboBox_meta.ItemIndex<>JSONPropStorage1.ReadInteger('meta',ComboBox_meta.ItemIndex);
 if not Result then
   Result:=ComboBox_waitvideo.ItemIndex<>JSONPropStorage1.ReadInteger('wait_video',ComboBox_waitvideo.ItemIndex);
 if not Result then
   Result:=ComboBox_waitkey.ItemIndex<>JSONPropStorage1.ReadInteger('wait_key',ComboBox_waitkey.ItemIndex);
end;

procedure TFormNginxtool.FormCreate(Sender: TObject);
begin
  loglist:=TLogListFPC.Create(Self);
  loglist.Name:='loglist1';
  loglist.Parent:=Panel1;
  loglist.Align:=alClient;
  loglist.Color:=clWhite;
  loglist.LineLimit:=1000;
  Application.SingleInstanceEnabled:=True;
end;

procedure TFormNginxtool.FormDestroy(Sender: TObject);
begin
  if CheckSettingChange then begin
    JSONPropStorage1.WriteBoolean('priority',CheckBox_priority.Checked);
    JSONPropStorage1.WriteBoolean('modify',CheckBoxModConf.Checked);
    JSONPropStorage1.WriteString('chunk_size',ComboBoxChunk.Text);
    JSONPropStorage1.WriteInteger('meta',ComboBox_meta.ItemIndex);
    JSONPropStorage1.WriteInteger('wait_video',ComboBox_waitvideo.ItemIndex);
    JSONPropStorage1.WriteInteger('wait_key',ComboBox_waitkey.ItemIndex);
    try
      JSONPropStorage1.Save;
    except
    end;
  end;
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
    {$ifdef WINDOWS}
    if CheckBox_priority.Checked then
      if UseAboveNormalProcess then begin
        myprocess.Priority:=ppAboveNormal;
        ppval:=ABOVE_NORMAL_PRIORITY_CLASS;
      end
      else begin
        myprocess.Priority:=ppHigh;
        ppval:=HIGH_PRIORITY_CLASS;
      end;
    {$endif}
    try
      myprocess.Execute;
      nginx_process_find:=0;
      Timer1.Enabled:=True;
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

procedure TFormNginxtool.CheckBoxModConfClick(Sender: TObject);
begin
  if CheckBoxModConf.Checked then
    VerboseNginxConfig;
end;

procedure TFormNginxtool.ComboBoxChunkCloseUp(Sender: TObject);
begin
  CheckBoxModConfClick(nil);
end;

procedure TFormNginxtool.ComboBoxChunkKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then begin
    Key:=#0;
    CheckBoxModConfClick(nil);
  end;
end;

procedure TFormNginxtool.ComboBox_metaCloseUp(Sender: TObject);
begin
  CheckBoxModConfClick(nil);
end;

procedure TFormNginxtool.ComboBox_waitvideoCloseUp(Sender: TObject);
begin
  CheckBoxModConfClick(nil);
end;

procedure TFormNginxtool.FormShow(Sender: TObject);
begin
  try
    JSONPropStorage1.Restore;
    CheckBox_priority.Checked:=JSONPropStorage1.ReadBoolean('priority',False);
    CheckBoxModConf.Checked:=JSONPropStorage1.ReadBoolean('modify',True);
    ComboBoxChunk.Text:=JSONPropStorage1.ReadString('chunk_size',ComboBoxChunk.Text);
    ComboBox_meta.ItemIndex:=JSONPropStorage1.ReadInteger('meta',ComboBox_meta.ItemIndex);
    ComboBox_waitvideo.ItemIndex:=JSONPropStorage1.ReadInteger('wait_video',ComboBox_waitvideo.ItemIndex);
    ComboBox_waitkey.ItemIndex:=JSONPropStorage1.ReadInteger('wait_key',ComboBox_waitkey.ItemIndex);
  except
  end;
  // store values
  JSONPropStorage1.WriteBoolean('priority',CheckBox_priority.Checked);
  JSONPropStorage1.WriteBoolean('modify',CheckBoxModConf.Checked);
  JSONPropStorage1.WriteString('chunk_size',ComboBoxChunk.Text);
  JSONPropStorage1.WriteInteger('meta',ComboBox_meta.ItemIndex);
  JSONPropStorage1.WriteInteger('wait_video',ComboBox_waitvideo.ItemIndex);
  JSONPropStorage1.WriteInteger('wait_key',ComboBox_waitkey.ItemIndex);

  CheckBoxModConf.OnClick:=@CheckBoxModConfClick;
  VerboseNginxConfig;
end;

{$ifdef WINDOWS}
function CheckWinVer:Boolean;
var
  Os:OSVERSIONINFO;
begin
  Result:=False;
  Os.dwOSVersionInfoSize:=sizeof(OSVERSIONINFO);
  if GetVersionEx(Os) then
    Result:=Os.dwMajorVersion>=5;
end;

initialization
  UseAboveNormalProcess:=CheckWinVer;

{$endif}


end.

