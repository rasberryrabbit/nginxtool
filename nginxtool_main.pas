unit nginxtool_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, JSONPropStorage, UniqueInstance, uConfigParser;

type

  { TFormNginxtool }

  TFormNginxtool = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox_priority: TCheckBox;
    CheckBoxModConf: TCheckBox;
    ComboBox_Record: TComboBox;
    ComboBox_meta: TComboBox;
    ComboBoxChunk: TComboBox;
    ComboBox_waitvideo: TComboBox;
    ComboBox_waitkey: TComboBox;
    EdRtmp1: TEdit;
    EdRtmp2: TEdit;
    EdRtmp3: TEdit;
    GroupBox1: TGroupBox;
    JSONPropStorage1: TJSONPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Timer1: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBoxModConfClick(Sender: TObject);
    procedure ComboBoxChunkCloseUp(Sender: TObject);
    procedure ComboBoxChunkKeyPress(Sender: TObject; var Key: char);
    procedure ComboBox_metaCloseUp(Sender: TObject);
    procedure ComboBox_waitvideoCloseUp(Sender: TObject);
    procedure EdRtmpExit(Sender: TObject);
    procedure EdRtmpKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  public
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

procedure TFormNginxtool.VerboseNginxConfig;
var
 schunksize : string;
 workercount : Integer;
 chunk_modified : Boolean;
 IPBuf:array[0..254] of char;

 configpar:TNginxConfigParser;
 item, itemprev:TNginxItem;
 itemgrp:TNginxItemGroup;
 itemidx:Integer;
 i,k:Integer;
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
       item:=configpar.ItemList.InsertNameValue(0,1,'worker_processes','1;');
       chunk_modified:=True;
     end;
   end;
   if item<>nil then
     loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));

   // check 'rtmp_auto_push on;'
   item:=configpar.ItemList.FindItemName('rtmp_auto_push');
   if item<>nil then begin
     if CheckBoxModConf.Checked then begin
       configpar.ItemList.DeleteItem(item);
       chunk_modified:=True;
       loglist.AddLog('rtmp_auto_push removed');
     end else
       loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));
   end;

   // check 'chunk_size 8192;'
   itemgrp:=configpar.ItemList.FindItemGroup('rtmp');
   if itemgrp<>nil then
     itemgrp:=itemgrp.FindItemGroup('server');
   if itemgrp<>nil then
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

   // insert meta copy
   itemgrp:=configpar.ItemList.FindItemGroup('rtmp');
   if itemgrp<>nil then
     itemgrp:=itemgrp.FindItemGroup('server');
   if itemgrp<>nil then
     itemgrp:=itemgrp.FindItemGroup('application');
   if itemgrp<>nil then
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

   // wait_video
   if itemgrp<>nil then begin
     item:=itemgrp.FindItemName('wait_video');
     if CheckBoxModConf.Checked then begin
       if item<>nil then begin
         if item.Value<>ComboBox_waitvideo.Text+';' then begin
           item.Value:=ComboBox_waitvideo.Text+';';
           chunk_modified:=True;
         end;
       end else begin
         if itemgrp<>nil then begin
           item:=itemgrp.InsertNameValue(0,itemgrp.Level+1,'wait_video',ComboBox_waitvideo.Text+';');
           chunk_modified:=True;
         end;
       end;
     end;
     if item<>nil then
       loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));
   end;

   // wait_key
   if itemgrp<>nil then begin
     item:=itemgrp.FindItemName('wait_key');
     if CheckBoxModConf.Checked then begin
       if item<>nil then begin
         if item.Value<>ComboBox_waitkey.Text+';' then begin
           item.Value:=ComboBox_waitkey.Text+';';
           chunk_modified:=True;
         end;
       end else begin
         if itemgrp<>nil then begin
           item:=itemgrp.InsertNameValue(0,itemgrp.Level+1,'wait_key',ComboBox_waitkey.Text+';');
           chunk_modified:=True;
         end;
       end;
     end;
     if item<>nil then
       loglist.AddLog(Format('%s %s',[item.NameItem,item.Value]));
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
     if CheckBoxModConf.Checked and doUpdatePush then begin
       item:=itemgrp.FindItemName('push');
       while item<>nil do begin
         itemprev:=item;
         item:=itemgrp.FindItemNameNext(item,'push');
         if itemprev.Value='' then
           itemgrp.DeleteItem(itemprev);
       end;
     end;
     doUpdatePush:=False;
   end;

   // record [off|all|audio|video|keyframes|manual]
   if itemgrp<>nil then begin
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
   end;

   // record path
   if itemgrp<>nil then begin
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
   end;

   // record_max_size 128;
   if itemgrp<>nil then begin
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
   end;

   // record_suffix .flv;
   if itemgrp<>nil then begin
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
   end;


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
   if itemgrp<>nil then begin
     item:=itemgrp.FindItemName('push');
     repeat
       if item<>nil then begin
         loglist.AddLog(Format('%s %s',[item.NameItem,item.Value{Copy(item.Value,1,40)+'...'}]));
         item:=itemgrp.FindItemNameNext(item,'push');
       end;
     until item=nil;
   end;

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
   Result:=ComboBox_waitvideo.ItemIndex<>JSONPropStorage1.ReadInteger('wait_video',ComboBox_waitvideo.ItemIndex);
 if not Result then
   Result:=ComboBox_waitkey.ItemIndex<>JSONPropStorage1.ReadInteger('wait_key',ComboBox_waitkey.ItemIndex);
 if not Result then
   Result:=ComboBox_Record.Text<>JSONPropStorage1.ReadString('record',ComboBox_Record.Text);
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
    JSONPropStorage1.WriteBoolean('priority',CheckBox_priority.Checked);
    JSONPropStorage1.WriteBoolean('modify',CheckBoxModConf.Checked);
    JSONPropStorage1.WriteString('chunk_size',ComboBoxChunk.Text);
    JSONPropStorage1.WriteInteger('meta',ComboBox_meta.ItemIndex);
    JSONPropStorage1.WriteInteger('wait_video',ComboBox_waitvideo.ItemIndex);
    JSONPropStorage1.WriteInteger('wait_key',ComboBox_waitkey.ItemIndex);
    JSONPropStorage1.WriteString('record',ComboBox_Record.Text);
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
    CheckBoxModConf.Checked:=JSONPropStorage1.ReadBoolean('modify',False);
    ComboBoxChunk.Text:=JSONPropStorage1.ReadString('chunk_size',ComboBoxChunk.Text);
    ComboBox_meta.ItemIndex:=JSONPropStorage1.ReadInteger('meta',ComboBox_meta.ItemIndex);
    ComboBox_waitvideo.ItemIndex:=JSONPropStorage1.ReadInteger('wait_video',ComboBox_waitvideo.ItemIndex);
    ComboBox_waitkey.ItemIndex:=JSONPropStorage1.ReadInteger('wait_key',ComboBox_waitkey.ItemIndex);
    ComboBox_Record.Text:=JSONPropStorage1.ReadString('record',ComboBox_Record.Text);
  except
  end;
  // store values
  JSONPropStorage1.WriteBoolean('priority',CheckBox_priority.Checked);
  JSONPropStorage1.WriteBoolean('modify',CheckBoxModConf.Checked);
  JSONPropStorage1.WriteString('chunk_size',ComboBoxChunk.Text);
  JSONPropStorage1.WriteInteger('meta',ComboBox_meta.ItemIndex);
  JSONPropStorage1.WriteInteger('wait_video',ComboBox_waitvideo.ItemIndex);
  JSONPropStorage1.WriteInteger('wait_key',ComboBox_waitkey.ItemIndex);
  JSONPropStorage1.WriteString('record',ComboBox_Record.Text);

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

