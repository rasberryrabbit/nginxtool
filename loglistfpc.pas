unit loglistfpc;

{ Simple Log view control

  Copyright (c) Do-wan Kim

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.

  2018. using more reliable lock method
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLType, Forms, syncobjs, ExtCtrls,
  LMessages;

type

  { TLogStringData }

  TLogStringData = class
    public
    F, B : TColor;
    Obj : TObject;

    constructor Create;
    destructor Destroy; override;
  end;

  { TLogStringList }

  TLogStringList = class(TStringList)
    private
      DefaultTCol, DefaultBCol : TColor;
      fEvent:TEvent;
      procedure Enter;
      procedure Leave;
    protected
      function GetCount: Integer; override;
      function Get(Index: Integer): string; override;
      function GetObject(Index: Integer): TObject; override;
      procedure Put(Index: Integer; const S: string); override;
      procedure PutObject(Index: Integer; AObject: TObject); override;
      procedure InsertItem(Index: Integer; const S: string; O: TObject);
        override;
      procedure Clear; override;
    public
      constructor Create;
      destructor Destroy; override;

      function GetStrObj(Index, MaxLen:Integer; out obj:TLogStringData):string;

      property TCol : TColor read DefaultTCol write DefaultTCol;
      property BCol : TColor read DefaultBCol write DefaultBCol;
  end;

  { TLogListFPC }
  TEventLogDeleteLimit = procedure(const s:string) of object;

  TLogListFPC = class(TScrollingWinControl)
    private
      PrevCount : Integer;
      LogData : TLogStringList;
      tHeight, tWidth, MaxTextWidth, tItems, FLineSpace : Integer;
      FLineLimit, FLastPosY, FLastPosX : Integer;
      FEvent : TEvent;
      FOnDeleteLimit : TEventLogDeleteLimit;
      FTimer : TTimer;
      FAddFlag, FSkipLast, FIsViewHorz, FUpdated : Boolean;

      function GetCount:Integer;
      procedure SetItemIndex(Value : Integer);
      function GetItemIndex:Integer;
      procedure SetHPos(Pos : Integer);
      procedure OnTimer(Sender : TObject);
      procedure CheckTextLen(const s: string);
      procedure SetLineSpace(AValue: Integer);
    protected
      procedure Paint; override;
      procedure FontChanged(Sender: TObject); override;
      procedure DoOnResize; override;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure DoEnter; override;
      procedure DoExit; override;
      procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
      procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure EraseBackground(DC: HDC); override;

      procedure SetItemPos(Pos : Integer);
      procedure SetLastPos;
      procedure Enter;
      procedure Leave;

      function AddLog(const s:string; txtColor, BackColor : TColor):Integer; overload;
      function AddLog(const s:string; txtColor : TColor):Integer; overload;
      function AddLog(const s:string):Integer; overload;
      function AddLogLine(const s:string):Integer;

      procedure InsertLog(Idx:Integer; const s:string; txtColor, BackColor : TColor); overload;
      procedure InsertLog(Idx:Integer; const s:string; txtColor : TColor); overload;
      procedure InsertLog(Idx:Integer; const s:string); overload;

      procedure UpdateLog(Idx:Integer; const s:string; txtColor, BackColor : TColor); overload;
      procedure UpdateLog(Idx:Integer; const s:string; txtColor: TColor); overload;
      procedure UpdateLog(Idx:Integer; const s:string); overload;

      procedure DeleteLog(Idx:Integer);

      procedure Clear;

      property Count: Integer read GetCount;
      property SkipMoveLast : Boolean read FSkipLast write FSkipLast;
      property LineSpace: Integer read FLineSpace write SetLineSpace;

    published
      property List : TLogStringList read LogData;
      property LineLimit : Integer read FLineLimit write FLineLimit;
      property OnDeleteLimit : TEventLogDeleteLimit read FOnDeleteLimit write FOnDeleteLimit;
      property ItemIndex : Integer read GetItemIndex write SetItemIndex;
      property IsViewHorz : Boolean read FIsViewHorz;
  end;

implementation

const
  _LogMaxCharLen = 1024;

{ TLogListFPC }

function TLogListFPC.GetCount: Integer;
begin
  Result:=LogData.Count;
end;

procedure TLogListFPC.SetItemIndex(Value: Integer);
begin
  SetItemPos(Value);
end;

function TLogListFPC.GetItemIndex: Integer;
begin
  if Assigned(VertScrollBar) then begin
    FLastPosY:=VertScrollBar.Position;
    Result:=FLastPosY div tHeight;
  end else
    Result:=0;
end;

procedure TLogListFPC.SetHPos(Pos: Integer);
begin
  if Assigned(HorzScrollBar) then begin
    if Pos<0 then Pos:=0;
    HorzScrollBar.Position:=Pos;
    FUpdated:=True;
  end;
end;

procedure TLogListFPC.OnTimer(Sender: TObject);
var
  CurCount, NewRange : Integer;
begin
  CurCount:=Count;
  if PrevCount<>CurCount then
     if Assigned(VertScrollBar) then begin
        PrevCount:=CurCount;
        NewRange:=CurCount * tHeight;
        VertScrollBar.Range:=NewRange;
        FUpdated:=True;
     end;
  if FAddFlag then begin
    FAddFlag:=False;
    FUpdated:=True;
    if not FSkipLast then
       SetLastPos;
  end;
  if FUpdated and (FEvent.WaitFor(0)=wrSignaled) then begin
    FUpdated:=False;
    Invalidate;
  end;
end;

procedure TLogListFPC.CheckTextLen(const s: string);
var
  txtWidth:Integer;
begin
  txtWidth:=Canvas.TextWidth(s);
  if MaxTextWidth<txtWidth then begin
     MaxTextWidth:=txtWidth;

     HorzScrollBar.Range:=(MaxTextWidth+BorderWidth+tWidth) div 2;
     FIsViewHorz:=True;
     FUpdated:=True;
  end;
end;

procedure TLogListFPC.SetLineSpace(AValue: Integer);
begin
  if FLineSpace=AValue then Exit;
  FLineSpace:=AValue;
  tHeight:=Canvas.TextHeight('hj');
  Inc(tHeight,FLineSpace);
  if tHeight=0 then tHeight:=1;
  tItems:=ClientHeight div tHeight;
  FUpdated:=True;
end;


procedure TLogListFPC.Paint;
var
  cPos, cPointY, cPointX, ViewHeight : Integer;
  cstr : string;
  temp : TLogStringData;
  SRect : TRect;
begin
  FEvent.ResetEvent;
  try
  Canvas.Brush.Color:=Color;
  SRect:=GetScrolledClientRect;
  Canvas.FillRect(SRect);
  FLastPosX:=HorzScrollBar.Position;
  cPos := GetItemIndex; // FLastPosY updated
  cPointY:=cPos*tHeight+BorderWidth;
  cPointX:=BorderWidth-FLastPosX;
  ViewHeight:=HorzScrollBar.ClientSizeWithoutBar;
  while cPos<Count do begin
    cstr:=LogData.GetStrObj(cPos,_LogMaxCharLen,temp);
    if temp<>nil then begin
      Canvas.Brush.Color:=temp.B;
      Canvas.Font.Color:=temp.F;
    end;
    Canvas.TextRect(SRect,cPointX,cPointY,cstr);
    (*
    if Focused and (FSelectedIndex=cPos) then
      Canvas.DrawFocusRect(Rect(cPointX-1,cPointY-1,cPointX+MaxTextWidth+1,cPointY+tHeight-1));
    *)
    Inc(cPointY,tHeight);
    if (cPointY-FLastPosY)>ViewHeight then
       Break;
    Inc(cPos);
  end;
  Canvas.Brush.Color:=Color;
  Canvas.FillRect(SRect.Left,SRect.Top,SRect.Left+BorderWidth-1,SRect.Bottom);
  if Focused then
     Canvas.Brush.Color:=clHighlight
     else Canvas.Brush.Color:=clInactiveBorder;
  Canvas.FillRect(SRect.Left,SRect.Top,SRect.Left+BorderWidth-2,SRect.Bottom);
  finally
    FEvent.SetEvent;
  end;
  inherited Paint;
end;

procedure TLogListFPC.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  tHeight:=Canvas.TextHeight('hj');
  Inc(tHeight,FLineSpace);
  if tHeight=0 then tHeight:=1;
  tWidth:=Canvas.TextWidth('F');
  tItems:=ClientHeight div tHeight;
  FUpdated:=True;
  OnTimer(Self);
end;

procedure TLogListFPC.DoOnResize;
begin
  inherited DoOnResize;
  if Assigned(VertScrollBar) then
     VertScrollBar.Page:=ClientHeight div tHeight * tHeight;
  if Assigned(HorzScrollBar) then
     HorzScrollBar.Page:=ClientWidth div 2;
  FUpdated:=True;
end;

procedure TLogListFPC.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ssCtrl in Shift then begin
    case Key of
    VK_HOME : begin SetItemPos(0); Key:=0; end;
    VK_END  : begin SetLastPos; Key:=0; end;
    end;
  end else begin
    case Key of
    VK_HOME : begin SetHPos(0); Key:=0; end;
    VK_END  : begin SetHPos(MaxTextWidth); Key:=0; end;
    VK_UP   : begin ItemIndex:=ItemIndex-1; Key:=0; end;
    VK_DOWN : begin ItemIndex:=ItemIndex+1; Key:=0 end;
    VK_PRIOR: begin ItemIndex:=ItemIndex-tItems; Key:=0; end;
    VK_NEXT : begin ItemIndex:=ItemIndex+tItems; Key:=0; end;
    VK_LEFT : begin HorzScrollBar.Position:=HorzScrollBar.Position-ClientWidth div 8; Key:=0; end;
    VK_RIGHT: begin HorzScrollBar.Position:=HorzScrollBar.Position+ClientWidth div 8; Key:=0; end;
    end;
  end;
  FUpdated:=True;
  inherited KeyDown(Key, Shift);
end;

procedure TLogListFPC.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TLogListFPC.DoExit;
begin
  Invalidate;
  inherited DoExit;
end;

procedure TLogListFPC.WMHScroll(var Message: TLMHScroll);
begin
  inherited;
  FUpdated:=True;
end;

procedure TLogListFPC.WMVScroll(var Message: TLMVScroll);
begin
  inherited;
  FUpdated:=True;
end;

constructor TLogListFPC.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Parent=nil then Parent:=TWinControl(AOwner);
  FLineSpace:=2;
  BorderWidth:=5;
  FAddFlag:=False;
  FSkipLast:=False;
  FIsViewHorz:=False;
  FLineLimit:=1000;
  FOnDeleteLimit:=nil;
  PrevCount:=-1;
  FLastPosY:=0;
  FLastPosX:=0;
  LogData := TLogStringList.Create;
  FEvent := TEvent.Create(nil,True,True,'LOGLIST'+IntToStr(GetTickCount));
  FontChanged(Self);
  LogData.DefaultBCol:=Color;
  LogData.DefaultTCol:=Font.Color;
  FTimer:=TTimer.Create(self);
  FTimer.OnTimer:=@OnTimer;
  FTimer.Interval:=100;
  FLineLimit:=1000;
  FUpdated:=True;
end;

destructor TLogListFPC.Destroy;
begin
  FTimer.OnTimer:=nil;
  LogData.Free;
  FEvent.Free;
  inherited Destroy;
end;

procedure TLogListFPC.EraseBackground(DC: HDC);
begin
  ;
end;

procedure TLogListFPC.SetItemPos(Pos: Integer);
var
  NewPos : Integer;
begin
  if Assigned(VertScrollBar) then begin
   if Pos<0 then Pos:=0;
   NewPos:=Pos*tHeight;
   Enter;
   try
     VertScrollBar.Position:=NewPos;
     FUpdated:=True;
   finally
     Leave;
   end;
  end;
end;

procedure TLogListFPC.SetLastPos;
begin
  if Assigned(VertScrollBar) then begin
    VertScrollBar.Position:=VertScrollBar.Range;
    FUpdated:=True;
  end;
end;

procedure TLogListFPC.Enter;
begin
  while FEvent.WaitFor(0)=wrTimeout do
    Sleep(0);
  FEvent.ResetEvent;
end;

procedure TLogListFPC.Leave;
begin
  FEvent.SetEvent;
end;

function TLogListFPC.AddLog(const s: string; txtColor, BackColor: TColor
  ): Integer;
var
  temp:TLogStringData;
begin
  if (LogData.Count>0) and (FLineLimit<=LogData.Count) then begin
    if Assigned(FOnDeleteLimit) then
       FOnDeleteLimit(LogData.Strings[0]);
    LogData.Delete(0);
  end;
  temp:=TLogStringData.Create;
  try
    temp.F:=txtColor;
    temp.B:=BackColor;
    CheckTextLen(s);
    Result:=LogData.AddObject(s,temp);
    FAddFlag:=True;
  except
    temp.Free;
    raise;
  end;
end;

function TLogListFPC.AddLog(const s: string; txtColor: TColor): Integer;
begin
  Result:=AddLog(s,txtColor,LogData.DefaultBCol);
end;

function TLogListFPC.AddLog(const s: string): Integer;
begin
  Result:=AddLog(s,LogData.DefaultTCol,LogData.DefaultBCol);
end;

function ReadLine(const s:string; var next:Integer; len:Integer):string;
var
  i:Integer;
  ch: char;
begin
  i:=next;
  Result:='';
  while i<=len do begin
    ch:=s[i];
    if ch in [#10,#13] then begin
      Inc(i);
      if (i<=len) and (s[i] in [#10,#13]) then
         if s[i]<>ch then
            Inc(i);
      break;
    end else
      Result:=Result+ch;
    Inc(i);
  end;
  next:=i;
end;

function TLogListFPC.AddLogLine(const s: string): Integer;
var
  temp:TLogStringData;
  stemp:string;
  i,l:Integer;
begin
  if (LogData.Count>0) and (FLineLimit<=LogData.Count) then begin
    if Assigned(FOnDeleteLimit) then
       FOnDeleteLimit(LogData.Strings[0]);
    LogData.Delete(0);
  end;
  i:=1;
  l:=Length(s);
  repeat
    stemp:=ReadLine(s,i,l);
    temp:=TLogStringData.Create;
    try
      temp.F:=LogData.DefaultTCol;
      temp.B:=LogData.DefaultBCol;
      CheckTextLen(s);
      Result:=LogData.AddObject(stemp,temp);
    except
      temp.Free;
      raise;
    end;
  until i>l;
  FAddFlag:=True;
end;


procedure TLogListFPC.InsertLog(Idx: Integer; const s: string; txtColor,
  BackColor: TColor);
var
  temp:TLogStringData;
begin
  if (LogData.Count>0) and (FLineLimit<=LogData.Count) then begin
    if Assigned(FOnDeleteLimit) then
       FOnDeleteLimit(LogData.Strings[0]);
    LogData.Delete(0);
  end;
  temp:=TLogStringData.Create;
  try
    temp.F:=txtColor;
    temp.B:=BackColor;
    CheckTextLen(s);
    if Idx>Count then
       Idx:=Count;
    LogData.Insert(Idx,s);
    LogData.Objects[Idx]:=temp;
    FAddFlag:=True;
  except
    temp.Free;
    raise;
  end;
end;

procedure TLogListFPC.InsertLog(Idx: Integer; const s: string;
  txtColor: TColor);
begin
  InsertLog(Idx,s,txtColor,LogData.DefaultBCol);
end;

procedure TLogListFPC.InsertLog(Idx: Integer; const s: string);
begin
  InsertLog(Idx,s,LogData.DefaultTCol,LogData.DefaultBCol);
end;

procedure TLogListFPC.UpdateLog(Idx: Integer; const s: string; txtColor,
  BackColor: TColor);
var
  temp:TLogStringData;
begin
  LogData.Strings[Idx]:=s;
  temp:=TLogStringData.Create;
  try
    temp.F:=txtColor;
    temp.B:=BackColor;
    CheckTextLen(s);
    LogData.Objects[Idx]:=temp;
    FUpdated:=True;
  except
    temp.Free;
    raise;
  end;
end;

procedure TLogListFPC.UpdateLog(Idx: Integer; const s: string;
  txtColor: TColor);
begin
  UpdateLog(Idx,s,txtColor,LogData.DefaultBCol);
end;

procedure TLogListFPC.UpdateLog(Idx: Integer; const s: string);
begin
  UpdateLog(Idx,s,LogData.DefaultTCol,LogData.DefaultBCol);
end;

procedure TLogListFPC.DeleteLog(Idx: Integer);
begin
  LogData.Delete(Idx);
  FUpdated:=True;
end;

procedure TLogListFPC.Clear;
begin
  LogData.Clear;
  FLastPosY:=0;
  FLastPosX:=0;
  MaxTextWidth:=0;
  FAddFlag:=False;
  FIsViewHorz:=False;
  if Assigned(HorzScrollBar) then begin
    HorzScrollBar.Range:=0;
    HorzScrollBar.Position:=0;
  end;
  if Assigned(VertScrollBar) then
     VertScrollBar.Range:=0;
  FUpdated:=True;
end;

{ TLogStringList }

procedure TLogStringList.Enter;
begin
  while fEvent.WaitFor(0)=wrTimeout do
    sleep(0);
  fEvent.ResetEvent;
end;

procedure TLogStringList.Leave;
begin
  fEvent.SetEvent;
end;

function TLogStringList.GetCount: Integer;
begin
  Enter;
  try
    Result:=inherited GetCount;
  finally
    Leave;
  end;
end;

function TLogStringList.Get(Index: Integer): string;
begin
  Enter;
  try
    Result:=inherited Get(Index);
  finally
    Leave;
  end;
end;

function TLogStringList.GetObject(Index: Integer): TObject;
begin
  Enter;
  try
    Result:=inherited GetObject(Index);
  finally
    Leave;
  end;
end;

procedure TLogStringList.Put(Index: Integer; const S: string);
begin
  Enter;
  try
    inherited Put(Index, S);
  finally
    Leave;
  end;
end;

procedure TLogStringList.PutObject(Index: Integer; AObject: TObject);
begin
  Enter;
  try
    inherited PutObject(Index, AObject);
  finally
    Leave;
  end;
end;

procedure TLogStringList.InsertItem(Index: Integer; const S: string; O: TObject
  );
begin
  Enter;
  try
    inherited InsertItem(Index, S, O);
  finally
    Leave;
  end;
end;

procedure TLogStringList.Clear;
begin
  Enter;
  try
    inherited Clear;
  finally
    Leave;
  end;
end;

constructor TLogStringList.Create;
begin
  inherited;
  DefaultBCol:=clBtnFace;
  DefaultTCol:=clWindowText;
  OwnsObjects:=True;
  fEvent:=TEvent.Create(nil,True,True,'LOGSTRLST'+IntToStr(GetTickCount));
end;

destructor TLogStringList.Destroy;
begin
  fEvent.Free;
  inherited Destroy;
end;

function TLogStringList.GetStrObj(Index, MaxLen: Integer; out
  obj: TLogStringData): string;
begin
  Enter;
  try
    Result:=Copy(inherited Get(Index),1,MaxLen);
    try
      obj:=TLogStringData(inherited GetObject(Index));
    except
      obj:=nil;
    end;
  finally
    Leave;
  end;
end;


{ TLogStringData }

constructor TLogStringData.Create;
begin
  Obj:=nil;
end;

destructor TLogStringData.Destroy;
begin
  inherited Destroy;
end;

end.

