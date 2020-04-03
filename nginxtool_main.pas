unit nginxtool_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, JSONPropStorage, Spin, UniqueInstance, uConfigParser;

type

  { TFormNginxtool }

  TFormNginxtool = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox_publishnotify: TCheckBox;
    CheckBox_sessionrelay: TCheckBox;
    CheckBox_idlestm: TCheckBox;
    CheckBox_waitkey: TCheckBox;
    CheckBox_waitvideo: TCheckBox;
    CheckBox_priority: TCheckBox;
    CheckBoxModConf: TCheckBox;
    ComboBox_Record: TComboBox;
    ComboBox_meta: TComboBox;
    ComboBoxChunk: TComboBox;
    EdRtmp1: TEdit;
    EdRtmp2: TEdit;
    EdRtmp3: TEdit;
    GroupBox1: TGroupBox;
    JSONPropStorage1: TJSONPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    SpinEdit_sync: TSpinEdit;
    Timer1: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBoxModConfClick(Sender: TObject);
    procedure CheckBox_ValueChange(Sender: TObject);
    procedure ComboBoxChunkCloseUp(Sender: TObject);
    procedure ComboBoxChunkKeyPress(Sender: TObject; var Key: char);
    procedure ComboBox_ValueChange(Sender: TObject);
    procedure EdRtmpExit(Sender: TObject);
    procedure EdRtmpKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_syncChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    function CheckOnOff(item: TNginxItem; b: Boolean): Boolean;
  public
    procedure SaveOptions;
    procedure VerboseNginxConfig;
    function CheckSettingChange:Boolean;

    procedure NginxLogEndLine;

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
  doUpdatePush: Boolean = False;


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

function boolToInt(b:Boolean):Integer;
begin
  if b then
    Result:=1
    else
      Result:=0;
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


procedure TFormNginxtool.Timer1Timer(Sender: TObject);
begin
 {$ifdef WINDOWS}
 if CheckBox_priority.Checked then
   ProcessPriority('nginx.exe',ppval);
 if nginx_process_find>0 then
   Timer1.Enabled:=False;
 {$else}
   Timer1.Enabled:=False;
 {$endif}
 if not Timer1.Enabled then
   NginxLogEndLine;
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

function boolToOnOff(b:Boolean):string;
begin
 if b then
   Result:='on'
   else
     Result:='off';
end;

function TFormNginxtool.CheckOnOff(item:TNginxItem;b:Boolean):Boolean;
begin
  Result:=False;
  if not SameText(item.Value,boolToOnOff(b)+';') then begin
    item.Value:=boolToOnOff(b)+';';
    Result:=True;
  end;
end;

procedure TFormNginxtool.SaveOptions;
begin
  // store values
  JSONPropStorage1.WriteBoolean('priority', CheckBox_priority.Checked);
  JSONPropStorage1.WriteBoolean('modify', CheckBoxModConf.Checked);
  JSONPropStorage1.WriteString('chunk_size', ComboBoxChunk.Text);
  JSONPropStorage1.WriteInteger('meta', ComboBox_meta.ItemIndex);
  JSONPropStorage1.WriteInteger('wait_video', boolToInt(
    CheckBox_waitvideo.Checked));
  JSONPropStorage1.WriteInteger('wait_key', boolToInt(
    CheckBox_waitkey.Checked));
  JSONPropStorage1.WriteString('record', ComboBox_Record.Text);
  JSONPropStorage1.WriteBoolean('idlestreams', CheckBox_idlestm.Checked);
  JSONPropStorage1.WriteBoolean('sessionrelay', CheckBox_sessionrelay.Checked);
  JSONPropStorage1.WriteBoolean('publishnotify', CheckBox_publishnotify.Checked
    );
  JSONPropStorage1.WriteInteger('sync',SpinEdit_sync.Value);
end;

function StripInt(const s:string):Integer;
var
  i, len:Integer;
  buf:string;
  ch:char;
begin
 len:=Length(s);
 i:=1;
 buf:='';
 while i<=len do begin
   ch:=s[i];
   if ch in ['0'..'9'] then
     buf:=buf+ch
     else
       break;
   Inc(i);
 end;
 Result:=StrToIntDef(buf,0);
end;

procedure TFormNginxtool.VerboseNginxConfig;
var
 schunksize : string;
 workercount : Integer;
 chunk_modified : Boolean;
 IPBuf:array[0..254] of char;

 configpar:TNginxConfigParser;
 item, itemprev:TNginxItem;
 rtmpgrp, itemgrp, itemgrpPrev:TNginxItemGroup;
 itemidx:Integer;
 i,k:Integer;
 dHandle: THANDLE;
begin
 chunk_modified:=False;
 loglist.AddLog('----- nginx config -----');
 schunksize:=Trim(ComboBoxChunk.Text);
 if StrToIntDef(schunksize,0)=0 then begin
   schunksize:='4096';
   ComboBoxChunk.Text:=schunksize;
 end;

 configpar:=TNginxConfigParser.Create;
 try
   // create nil file
   ForceDirectories('conf');
   if not FileExists('conf/nginx.conf') then begin
     dHandle:=FileCreate('conf/nginx.conf');
     FileClose(dHandle);
   end;

   // open
   configpar.Load('conf/nginx.conf');

   // check 'worker_process 1;'
   item:=configpar.ItemList.FindItemName('worker_processes');
   if item<>nil then begin
     workercount:=StrToIntDef(Copy(item.Value,1,Length(item.Value)-1),0);
     if CheckBoxModConf.Checked then
       if workercount<>1 then begin
         item.Value:='1;';
         chunk_modified:=True;
         workercount:=1;
       end;
   end else
   begin
     workercount:=1;
     if CheckBoxModConf.Checked then begin
       item:=configpar.ItemList.InsertNameValue(0,0,'worker_processes','1;');
       chunk_modified:=True;
     end;
   end;
   if item<>nil then
     loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   // rtmp_auto_push - multiworker push
   item:=configpar.ItemList.FindItemName('rtmp_auto_push');
   if item=nil then begin
     item:=configpar.ItemList.InsertNameValue(0,0,'rtmp_auto_push','off;');
     chunk_modified:=True;
   end;
   if CheckBoxModConf.Checked then begin
     if workercount<2 then
       item.Value:='off;'
       else
         item.Value:='on;';
     chunk_modified:=True;
   end;
   loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   // events
   itemgrp:=configpar.ItemList.FindItemGroup('events');
   if itemgrp=nil then begin
     configpar.ItemList.AddNameGroup(0,'events','{');
     itemgrp:=configpar.ItemList.FindItemGroup('events');
     itemgrp.MarkClose;
     chunk_modified:=True;
   end;
   // worker_connections
   item:=itemgrp.FindItemName('worker_connections');
   if item=nil then begin
     itemgrp.AddNameValue(itemgrp.Level+1,'worker_connections','1024;');
     chunk_modified:=True;
   end;

   // ----- rtmp -----
   rtmpgrp:=configpar.ItemList.FindItemGroup('rtmp');
   if rtmpgrp=nil then begin
     configpar.ItemList.AddNameGroup(0,'rtmp','{');
     rtmpgrp:=configpar.ItemList.FindItemGroup('rtmp');
     rtmpgrp.MarkClose;
     chunk_modified:=True;
   end;

   // ----- server -----
   itemgrp:=rtmpgrp.FindItemGroup('server');
   if itemgrp<>nil then begin
     while itemgrp<>nil do begin
       item:=itemgrp.FindItemName('listen');
       if (item<>nil) and (NginxRemoveTrailValue(item.Value)='1935') then
         break;
       itemgrp:=rtmpgrp.FindItemGroupNext(itemgrp,'server');
     end;
   end;
   if itemgrp=nil then begin
     rtmpgrp.AddNameGroup(rtmpgrp.Level+1,'server','{');
     itemgrp:=rtmpgrp.FindItemGroup('server');
     itemgrp.AddNameValue(itemgrp.Level+1,'listen','1935;');
     itemgrp.AddNameValue(itemgrp.Level+1,'chunk_size','4096;');
     itemgrp.MarkClose;
     chunk_modified:=True;
   end;
   // sync
   item:=itemgrp.FindItemName('sync');
   if item=nil then begin
     itemgrp.AddNameValue(itemgrp.Level+1,'sync','300ms;');
     chunk_modified:=True;
   end else if CheckBoxModConf.Checked then
     if SpinEdit_sync.Value<>StripInt(item.Value) then begin
       item.Value:=IntToStr(SpinEdit_sync.Value)+'ms;';
     end;
   // buflen
   item:=itemgrp.FindItemName('buflen');
   if item=nil then begin
     itemgrp.AddNameValue(itemgrp.Level+1,'buflen','1s;');
     chunk_modified:=True;
   end;
   // publish_notify
   item:=itemgrp.FindItemName('publish_notify');
   if item=nil then begin
     itemgrp.AddNameValue(itemgrp.Level+1,'publish_notify',boolToOnOff(CheckBox_publishnotify.Checked)+';');
     chunk_modified:=True;
   end else if CheckBoxModConf.Checked then
     if CheckOnOff(item,CheckBox_publishnotify.Checked) then
       chunk_modified:=True;

   // chunk_size 8192;
   item:=itemgrp.FindItemName('chunk_size');
   if CheckBoxModConf.Checked then begin
     if item<>nil then begin
       if Item.Value<>schunksize+';' then begin
         item.Value:=schunksize+';';
         chunk_modified:=True;
       end;
     end else
     begin
       if itemgrp<>nil then begin
         item:=itemgrp.InsertNameValue(0,itemgrp.Level+1,'chunk_size',schunksize+';');
         chunk_modified:=True;
       end;
     end;
   end;
   if item<>nil then
     loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   // wait_video
   item:=itemgrp.FindItemName('wait_video');
   if CheckBoxModConf.Checked then begin
     if item<>nil then begin
       if CheckOnOff(item,CheckBox_waitvideo.Checked) then
         chunk_modified:=True;
     end else begin
       if itemgrp<>nil then begin
         item:=itemgrp.InsertNameValue(0,itemgrp.Level+1,'wait_video',boolToOnOff(CheckBox_waitvideo.Checked)+';');
         chunk_modified:=True;
       end;
     end;
   end;
   if item<>nil then
     loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   // wait_key
   item:=itemgrp.FindItemName('wait_key');
   if CheckBoxModConf.Checked then begin
     if item<>nil then begin
       if CheckOnOff(item,CheckBox_waitkey.Checked) then
         chunk_modified:=True;
     end else begin
       if itemgrp<>nil then begin
         item:=itemgrp.InsertNameValue(0,itemgrp.Level+1,'wait_key',boolToOnOff(CheckBox_waitkey.Checked)+';');
         chunk_modified:=True;
       end;
     end;
   end;
   if item<>nil then
     loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   // insert meta copy
   item:=itemgrp.FindItemName('meta');
   if CheckBoxModConf.Checked then begin
     if item<>nil then begin
       if item.Value<>ComboBox_meta.Text+';' then begin
         item.Value:=ComboBox_meta.Text+';';
         chunk_modified:=True;
       end;
     end else begin
       if itemgrp<>nil then begin
         item:=itemgrp.InsertNameValue(0,itemgrp.Level+1,'meta',ComboBox_meta.Text+';');
         chunk_modified:=True;
       end;
     end;
   end;
   if item<>nil then
     loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   // ----- application -----
   itemgrpPrev:=itemgrp;
   itemgrp:=itemgrp.FindItemGroup('application');
   if itemgrp=nil then begin
     itemgrpPrev.AddNameGroup(itemgrpPrev.Level+1,'application','live {');
     itemgrp:=itemgrpPrev.FindItemGroup('application');
     itemgrp.AddNameValue(itemgrp.Level+1,'live','on;');
     itemgrp.AddNameValue(itemgrp.Level+1,'interleave','off;');
     itemgrp.AddNameValue(itemgrp.Level+1,'allow','publish all;');
     itemgrp.AddNameValue(itemgrp.Level+1,'allow','play all;');
     itemgrp.MarkClose;
     chunk_modified:=True;
   end;
   // idle_streams
   item:=itemgrp.FindItemName('idle_streams');
   if item=nil then begin
     itemgrp.AddNameValue(itemgrp.Level+1,'idle_streams','off;');
     chunk_modified:=True;
   end else if CheckBoxModConf.Checked then begin
     if CheckOnOff(item,CheckBox_idlestm.Checked) then
       chunk_modified:=True;
   end;
   // session_relay
   item:=itemgrp.FindItemName('session_relay');
   if item=nil then begin
     itemgrp.AddNameValue(itemgrp.Level+1,'session_relay','off;');
     chunk_modified:=True;
   end else if CheckBoxModConf.Checked then begin
     if CheckOnOff(item,CheckBox_sessionrelay.Checked) then
       chunk_modified:=True;
   end;

   // push check and update
   if itemgrp<>nil then begin
     item:=itemgrp.FindItemName('push');
     i:=1;
     // insert nil value item
     if item=nil then begin
       itemgrp.AddNameValue(itemgrp.Level+1,'push','');
       item:=itemgrp.FindItemName('push');
     end;

     while item<>nil do begin
       if i<=3 then begin
         // restore push
         if not doUpdatePush then begin
           (FindComponent('EdRtmp'+IntToStr(i)) as TEdit).Text:=NginxRemoveTrailValue(item.Value);
           item:=itemgrp.FindItemNameNext(item,'push');
         end else begin
         // update push
           if CheckBoxModConf.Checked then begin
             chunk_modified:=True;
             if (FindComponent('EdRtmp'+IntToStr(i)) as TEdit).Text<>'' then
               item.value:=(FindComponent('EdRtmp'+IntToStr(i)) as TEdit).Text+';'
               else
                 item.value:='';
             itemprev:=item;
             item:=itemgrp.FindItemNameNext(item,'push');
             // insert nil value item
             if item=nil then begin
               itemgrp.InsertNameValue(itemgrp.IndexOfItem(itemprev),itemgrp.Level+1,'push','');
               item:=itemgrp.FindItemNameNext(itemprev,'push');
             end;
           end;
         end;
       end else
         break;
       Inc(i);
     end;
     // remove nil value item
     item:=itemgrp.FindItemName('push');
     while item<>nil do begin
       itemprev:=item;
       item:=itemgrp.FindItemNameNext(item,'push');
       if itemprev.Value='' then
         itemgrp.DeleteItem(itemprev);
     end;
     doUpdatePush:=False;
   end;

   // record [off|all|audio|video|keyframes|manual]
   item:=itemgrp.FindItemName('record');
   if CheckBoxModConf.Checked then begin
     if item<>nil then begin
       if item.Value<>ComboBox_Record.Text+';' then begin
         item.Value:=ComboBox_Record.Text+';';
         chunk_modified:=True;
       end;
     end else begin
       if itemgrp<>nil then begin
         item:=itemgrp.InsertNameValue(0,itemgrp.Level+1,'record',ComboBox_Record.Text+';');
         chunk_modified:=True;
       end;
     end;
   end;
   if item<>nil then
     loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   // record path
   item:=itemgrp.FindItemName('record_path');
   if CheckBoxModConf.Checked then begin
     if item=nil then begin
       if itemgrp<>nil then begin
         item:=itemgrp.InsertNameValue(0,itemgrp.Level+1,'record_path',pchar(StringReplace( ExtractFileDir(GetUserDir),PathDelim,'/',[rfReplaceAll]))+';');
         chunk_modified:=True;
       end;
     end;
   end;
   if item<>nil then
     loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   // record_max_size 128;
   item:=itemgrp.FindItemName('record_max_size');
   if CheckBoxModConf.Checked then begin
     if item=nil then begin
       if itemgrp<>nil then begin
         item:=itemgrp.InsertNameValue(0,itemgrp.Level+1,'record_max_size','600M;');
         chunk_modified:=True;
       end;
     end;
   end;
   if item<>nil then
     loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   // record_suffix .flv;
   item:=itemgrp.FindItemName('record_suffix');
   if CheckBoxModConf.Checked then begin
     if item=nil then begin
       if itemgrp<>nil then begin
         item:=itemgrp.InsertNameValue(0,itemgrp.Level+1,'record_suffix','-%y-%m-%d-%H-%M-%S.flv;');
         chunk_modified:=True;
       end;
     end;
   end;
   if item<>nil then
     loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   if chunk_modified then begin
     try
       configpar.Save('conf/nginx.conf');
     except
       on e:exception do begin
         loglist.AddLog('Fail to write file "conf/nginx.conf"');
       end;
     end;
   end;

   // show push values
   item:=itemgrp.FindItemName('push');
   repeat
     if item<>nil then begin
       loglist.AddLog(Format('%s %s',[item.NameItem,item.Value{Copy(item.Value,1,40)+'...'}]));
       item:=itemgrp.FindItemNameNext(item,'push');
     end;
   until item=nil;

 finally
   configpar.Free;
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
   Result:=boolToInt(CheckBox_waitvideo.Checked)<>JSONPropStorage1.ReadInteger('wait_video',boolToInt(CheckBox_waitvideo.Checked));
 if not Result then
   Result:=boolToInt(CheckBox_waitkey.Checked)<>JSONPropStorage1.ReadInteger('wait_key',boolToInt(CheckBox_waitkey.Checked));
 if not Result then
   Result:=ComboBox_Record.Text<>JSONPropStorage1.ReadString('record',ComboBox_Record.Text);
 if not Result then
   Result:=CheckBox_idlestm.Checked<>JSONPropStorage1.ReadBoolean('idlestreams',CheckBox_idlestm.Checked);
 if not Result then
   Result:=CheckBox_sessionrelay.Checked<>JSONPropStorage1.ReadBoolean('sessionrelay',CheckBox_sessionrelay.Checked);
 if not Result then
   Result:=CheckBox_publishnotify.Checked<>JSONPropStorage1.ReadBoolean('publishnotify',CheckBox_publishnotify.Checked);
 if not Result then
   Result:=SpinEdit_sync.Value<>JSONPropStorage1.ReadInteger('sync',SpinEdit_sync.Value);
end;

procedure TFormNginxtool.NginxLogEndLine;
const
 ngxLogFile = './logs/error.log';
var
 fs : TFileStream;
 l : int64;
 i, j : Integer;
 buf : array[0..1024] of char;
begin
 if not FileExists(ngxLogFile) then
   exit;
 try
   fs := TFileStream.Create(ngxLogFile,fmOpenRead or fmShareDenyNone);
   try
     l:=fs.Size;
     if l>1024 then
       Dec(l,1024)
       else
         l:=0;
     fs.Position:=l;
     i:=fs.Read(buf[0],1024);
   finally
     fs.Free;
   end;
   if i>0 then begin
     Dec(i);
     j:=i;
     while i>=0 do begin
       if buf[i]>#32 then
         break;
       Dec(i);
     end;
     while i>=0 do begin
       if buf[i] in [#10,#13] then begin
         Inc(i);
         break;
       end;
       Dec(i);
     end;
     loglist.AddLog('log: '+Copy(buf,i,1024));
   end;
 except
   on e:exception do
     loglist.AddLog('log: '+e.Message);
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
  Application.SingleInstanceEnabled:=True;
  doUpdatePush:=False;
end;

procedure TFormNginxtool.FormDestroy(Sender: TObject);
begin
  if CheckSettingChange then begin
    SaveOptions;
    try
      JSONPropStorage1.Save;
    except
    end;
  end;
end;

procedure TFormNginxtool.Button1Click(Sender: TObject);
var
  myprocess:TProcess;
  i: integer;
begin
  {$ifdef WINDOWS}
  if checkEnumProcess('nginx.exe') then begin
     loglist.AddLog('> Already running! Try reloading.');
     Button3Click(Sender);
     exit;
  end;
  {$endif}
  myprocess:=TProcess.Create(nil);
  try
    myprocess.InheritHandles:=false;
    myprocess.Options:=[];
    myprocess.ShowWindow:=swoHIDE;
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
  i: integer;
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
    myprocess.ShowWindow:=swoHIDE;
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

procedure TFormNginxtool.Button3Click(Sender: TObject);
var
  myprocess: TProcess;
  i: integer;
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
    myprocess.ShowWindow:=swoHIDE;
    for i:=1 to GetEnvironmentVariableCount do
      myprocess.Environment.Add(GetEnvironmentString(i));
    myprocess.Executable:='nginx';
    myprocess.Parameters.Add('-s');
    myprocess.Parameters.Add('reload');
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
      loglist.AddLog('> nginx reloaded');
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
  EdRtmp1.ReadOnly:=not CheckBoxModConf.Checked;
  EdRtmp2.ReadOnly:=not CheckBoxModConf.Checked;
  EdRtmp3.ReadOnly:=not CheckBoxModConf.Checked;
  SpinEdit_sync.ReadOnly:=not CheckBoxModConf.Checked;
  if CheckBoxModConf.Checked then
    VerboseNginxConfig;
end;

procedure TFormNginxtool.CheckBox_ValueChange(Sender: TObject);
begin
  if (Sender = CheckBox_waitkey) and
    (CheckBox_waitkey.Checked) and
    (not CheckBox_waitvideo.Checked)
    then
    CheckBox_waitvideo.Checked:=True;

  CheckBoxModConfClick(nil);
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

procedure TFormNginxtool.ComboBox_ValueChange(Sender: TObject);
begin
 CheckBoxModConfClick(nil);
end;

procedure TFormNginxtool.EdRtmpExit(Sender: TObject);
begin
  if TEdit(Sender).Modified then begin
    doUpdatePush:=True;
    VerboseNginxConfig;
  end;
end;

procedure TFormNginxtool.EdRtmpKeyPress(Sender: TObject; var Key: char);
begin
  if key=#13 then begin
    key:=#0;
    if TEdit(Sender).Modified then begin
      doUpdatePush:=True;
      VerboseNginxConfig;
    end;
  end;
end;

procedure TFormNginxtool.FormShow(Sender: TObject);
begin
  try
    JSONPropStorage1.Restore;
    CheckBox_priority.Checked:=JSONPropStorage1.ReadBoolean('priority',False);
    CheckBoxModConf.Checked:=JSONPropStorage1.ReadBoolean('modify',True);
    ComboBoxChunk.Text:=JSONPropStorage1.ReadString('chunk_size',ComboBoxChunk.Text);
    ComboBox_meta.ItemIndex:=JSONPropStorage1.ReadInteger('meta',ComboBox_meta.ItemIndex);
    CheckBox_waitvideo.Checked:=JSONPropStorage1.ReadInteger('wait_video',boolToInt(CheckBox_waitvideo.Checked))<>0;
    CheckBox_waitkey.Checked:=JSONPropStorage1.ReadInteger('wait_key',boolToInt(CheckBox_waitkey.Checked))<>0;
    ComboBox_Record.Text:=JSONPropStorage1.ReadString('record',ComboBox_Record.Text);
    CheckBox_idlestm.Checked:=JSONPropStorage1.ReadBoolean('idlestreams',CheckBox_idlestm.Checked);
    CheckBox_sessionrelay.Checked:=JSONPropStorage1.ReadBoolean('sessionrelay',CheckBox_sessionrelay.Checked);
    CheckBox_publishnotify.Checked:=JSONPropStorage1.ReadBoolean('publishnotify',CheckBox_publishnotify.Checked);
    SpinEdit_sync.Value:=JSONPropStorage1.ReadInteger('sync',SpinEdit_sync.Value);
  except
  end;
  SaveOptions;
  CheckBoxModConf.OnClick:=@CheckBoxModConfClick;
  CheckBox_waitvideo.OnChange:=@CheckBox_ValueChange;
  CheckBox_waitkey.OnChange:=@CheckBox_ValueChange;
  CheckBox_idlestm.OnChange:=@CheckBox_ValueChange;
  CheckBox_sessionrelay.OnChange:=@CheckBox_ValueChange;
  CheckBox_publishnotify.OnChange:=@CheckBox_ValueChange;
  SpinEdit_sync.OnChange:=@SpinEdit_syncChange;
  SpinEdit_sync.OnEditingDone:=@SpinEdit_syncChange;
  VerboseNginxConfig;

end;

procedure TFormNginxtool.SpinEdit_syncChange(Sender: TObject);
begin
  CheckBoxModConfClick(nil);
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

