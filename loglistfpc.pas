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
    protected
    public
      constructor Create;

      property TCol : TColor read DefaultTCol write DefaultTCol;
      property BCol : TColor read DefaultBCol write DefaultBCol;
  end;

  { TLogListFPC }
  TEventLogDeleteLimit = procedure(const s:string) of object;

  TLogListFPC = class(TScrollingWinControl)
    private
      PrevCount : Integer;
      LogData : TLogStringList;
      tHeight, tWidth, MaxTextWidth, tItems : Integer;
      FLineLimit, FLastPosY, FLastPosX : Integer;
      FCriSec : TCriticalSection;
      FOnDeleteLimit : TEventLogDeleteLimit;
      FTimer : TTimer;
      FAddFlag, FSkipLast, FIsViewHorz, FUpdated : Boolean;

      function GetCount:Integer;
      procedure SetItemIndex(Value : Integer);
      function GetItemIndex:Integer;
      procedure SetHPos(Pos : Integer);
      procedure OnTimer(Sender : TObject);
      procedure CheckTextLen(const s:string);
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
  FCriSec.Acquire;
  try
     Result:=LogData.Count;
  finally
    FCriSec.Release;
  end;
end;

procedure TLogListFPC.SetItemIndex(Value: Integer);
begin
  SetItemPos(Value);
end;

function TLogListFPC.GetItemIndex: Integer;
begin
  FCriSec.Acquire;
  try
     if Assigned(VertScrollBar) then begin
       FLastPosY:=VertScrollBar.Position;
       Result:=FLastPosY div tHeight;
     end else Result:=0;
  finally
    FCriSec.Release;
  end;
end;

procedure TLogListFPC.SetHPos(Pos: Integer);
begin
  if Assigned(HorzScrollBar) then begin
    if Pos<0 then Pos:=0;
    HorzScrollBar.Position:=Pos;
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
    if not FSkipLast then
       SetLastPos;
  end;
  if FUpdated then begin
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
     FCriSec.Acquire;
     try
       HorzScrollBar.Range:=(MaxTextWidth+BorderWidth+tWidth) div 2;
     finally
       FCriSec.Release;
     end;
     FIsViewHorz:=True;
  end;
end;


procedure TLogListFPC.Paint;
var
  cPos, cPointY, cPointX : Integer;
  cstr : string;
  temp : TLogStringData;
  SRect : TRect;
begin
  Canvas.Brush.Color:=Color;
  SRect:=GetScrolledClientRect;
  Canvas.FillRect(SRect);
  FLastPosX:=HorzScrollBar.Position;
  cPos := GetItemIndex; // FLastPosY updated
  cPointY:=cPos*tHeight+BorderWidth;
  cPointX:=BorderWidth-FLastPosX;
  while cPos<Count do begin
    FCriSec.Acquire;
    try
       cstr:=Copy(LogData.Strings[cPos],1,_LogMaxCharLen);
       temp:=TLogStringData(LogData.Objects[cPos]);
    finally
      FCriSec.Release;
    end;
    Canvas.Brush.Color:=temp.B;
    Canvas.Font.Color:=temp.F;
    Canvas.TextRect(SRect,cPointX,cPointY,cstr);
    (*
    if Focused and (FSelectedIndex=cPos) then
      Canvas.DrawFocusRect(Rect(cPointX-1,cPointY-1,cPointX+MaxTextWidth+1,cPointY+tHeight-1));
    *)
    Inc(cPointY,tHeight);
    if (cPointY-FLastPosY)>ClientHeight then Break;
    Inc(cPos);
  end;
  Canvas.Brush.Color:=Color;
  Canvas.FillRect(SRect.Left,SRect.Top,SRect.Left+BorderWidth-1,SRect.Bottom);
  if Focused then
     Canvas.Brush.Color:=clHighlight
     else Canvas.Brush.Color:=clInactiveBorder;
  Canvas.FillRect(SRect.Left,SRect.Top,SRect.Left+BorderWidth-2,SRect.Bottom);
  inherited Paint;
end;

procedure TLogListFPC.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  tHeight:=Canvas.TextHeight('hj');
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
  FCriSec := TCriticalSection.Create;
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
  FCriSec.Free;
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
  FCriSec.Acquire;
  try
     if Assigned(VertScrollBar) then begin
       if Pos<0 then Pos:=0;
       NewPos:=Pos*tHeight;
       VertScrollBar.Position:=NewPos;
     end;
  finally
    FCriSec.Release;
  end;
end;

procedure TLogListFPC.SetLastPos;
begin
  FCriSec.Acquire;
  try
     if Assigned(VertScrollBar) then
       VertScrollBar.Position:=VertScrollBar.Range;
  finally
    FCriSec.Release;
  end;
end;

procedure TLogListFPC.Enter;
begin
  FCriSec.Acquire;
end;

procedure TLogListFPC.Leave;
begin
  FCriSec.Release;
end;

function TLogListFPC.AddLog(const s: string; txtColor, BackColor: TColor
  ): Integer;
var
  temp:TLogStringData;
begin
  FCriSec.Acquire;
  try
     if (LogData.Count>0) and (FLineLimit<=LogData.Count) then begin
        if Assigned(FOnDeleteLimit) then
           FOnDeleteLimit(LogData.Strings[0]);
        LogData.Delete(0);
     end;
  finally
    FCriSec.Release;
  end;
  temp:=TLogStringData.Create;
  try
    temp.F:=txtColor;
    temp.B:=BackColor;
    CheckTextLen(s);
    FCriSec.Acquire;
    try
       Result:=LogData.AddObject(s,temp);
    finally
      FCriSec.Release;
    end;
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

procedure TLogListFPC.InsertLog(Idx: Integer; const s: string; txtColor,
  BackColor: TColor);
var
  temp:TLogStringData;
begin
  FCriSec.Acquire;
  try
     if (LogData.Count>0) and (FLineLimit<=LogData.Count) then begin
        if Assigned(FOnDeleteLimit) then
           FOnDeleteLimit(LogData.Strings[0]);
        LogData.Delete(0);
     end;
  finally
    FCriSec.Release;
  end;
  temp:=TLogStringData.Create;
  try
    temp.F:=txtColor;
    temp.B:=BackColor;
    CheckTextLen(s);
    FCriSec.Acquire;
    try
       LogData.Insert(Idx,s);
       LogData.Objects[Idx]:=temp;
    finally
      FCriSec.Release;
    end;
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
  FCriSec.Acquire;
  try
     LogData.Strings[Idx]:=s;
  finally
    FCriSec.Release;
  end;
  temp:=TLogStringData.Create;
  try
    temp.F:=txtColor;
    temp.B:=BackColor;
    CheckTextLen(s);
    FCriSec.Acquire;
    try
       LogData.Objects[Idx]:=temp;
       FUpdated:=True;
    finally
      FCriSec.Release;
    end;
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
  FCriSec.Acquire;
  try
    LogData.Delete(Idx);
  finally
    FCriSec.Release;
  end;
end;

procedure TLogListFPC.Clear;
begin
  FCriSec.Acquire;
  try
    LogData.Clear;
  finally
    FCriSec.Release;
  end;
  FLastPosY:=0;
  FLastPosX:=0;
  MaxTextWidth:=0;
  FAddFlag:=False;
  FIsViewHorz:=False;
  if Assigned(HorzScrollBar) then begin
     HorzScrollBar.Range:=0;
     HorzScrollBar.Position:=0;
  end;
  if Assigned(VertScrollBar) then VertScrollBar.Range:=0;
  FUpdated:=True;
end;

{ TLogStringList }

constructor TLogStringList.Create;
begin
  inherited;
  DefaultBCol:=clBtnFace;
  DefaultTCol:=clWindowText;
  OwnsObjects:=True;
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

