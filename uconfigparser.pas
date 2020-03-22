unit uConfigParser;


{ Nginx Config Parser

  Copyright (C) 2018 rasberryrabbit github.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  { TNginxItem }

  TNginxItem = class
    private
      FLevel:Integer;
      FName:string;
      FValue:string;

    public
      property NameItem:string read FName write FName;
      property Value:string read FValue write FValue;
      property Level:Integer read FLevel write FLevel;
  end;

  { TNginxItemGroup }

  TNginxItemGroup = class(TNginxItem)
    private
      FParents:TNginxItemGroup;
      FItemList: TObjectList;
    public
      constructor Create;
      destructor Destroy; override;

      function AddNameValue(lvl: Integer; const sName, sValue: string): Integer;
      function InsertNameValue(Idx, lvl: Integer; const sName, sValue: string): TNginxItem;
      function AddNameGroup(lvl: Integer; const sName, sValue: string): Integer;
      procedure MarkClose;
      function InsertNameGroup(Idx, lvl: Integer; const sName, sValue: string): TNginxItemGroup;

      function FindItemName(const str: string): TNginxItem;
      function FindItemNameNext(BaseItem:TNginxItem; const str: string): TNginxItem;
      function FindItemGroup(const str: string): TNginxItemGroup;
      function FindItemGroupNext(BaseItem:TNginxItemGroup; const str: string): TNginxItemGroup;
      procedure DeleteItem(item:TObject);
      function IndexOfItem(item:TNginxItem):Integer;

      property Parents:TNginxItemGroup read FParents write FParents;
      property ItemList:TObjectList read FItemList;
  end;

  { TNginxConfigParser }

  TNginxConfigOnToken = procedure(const sName,sValue:string) of object;

  TNginxConfigParser = class
    private
      FStream:TStream;
      FBuffer:array[0..4095] of char;
      FBufLen:Integer;
      FBufIdx:Integer;

      FCurLevel:Integer;
      FItem:TNginxItemGroup;

      FOnToken:TNginxConfigOnToken;

      function ReadBufferChk:Boolean;
      function ReadToken:string;
      function Eof:Boolean;
      procedure ReadSkipSpace;

    protected
      procedure IterGroupSave(Obj:TNginxItemGroup);
    public
      constructor Create;
      destructor Destroy; override;

      procedure LoadFromStream(Stream:TStream);
      procedure Load(FileName:string);

      procedure SaveToStream(Stream:TStream);
      procedure Save(FileName:string);

      property OnToken:TNginxConfigOnToken read FOnToken write FOnToken;
      property ItemList:TNginxItemGroup read FItem;
  end;


function NginxRemoveTrailValue(const s:string; delim:char=';'):string;

implementation

uses
  strutils;


function NginxRemoveTrailValue(const s: string; delim: char): string;
var
  i, l, fspace:Integer;
  ch : char;
begin
  Result:='';
  i:=Length(s);
  l:=i;
  fspace:=0;
  while i>0 do begin
    ch := s[i];
    if fspace>0 then begin
        if ch=delim then begin
          Dec(i);
          break;
        end;
    end else begin
      if ch>#32 then begin
        fspace:=i;
        Inc(i);
      end;
    end;
    Dec(i);
  end;
  // remove space
  while i>0 do begin
    ch:=s[i];
    if ch>#32 then
      break;
    Dec(i);
  end;
  if i<=0 then
    i:=l;
  Result:=Copy(s,1,i);
end;

{ TNginxConfigParser }

function TNginxConfigParser.ReadBufferChk: Boolean;
begin
  if Assigned(FStream) then begin
    if FBufIdx>=FBufLen then begin
      FBufLen:=FStream.Read(FBuffer[0],sizeof(FBuffer));
      Result:=FBufLen>0;
      FBufIdx:=0;
    end else
      Result:=True;
  end else begin
    Result:=False;
    FBufLen:=0;
  end;
end;

function TNginxConfigParser.ReadToken: string;
var
  ch:char;
  bReadEol:Boolean;
begin
  Result:='';
  bReadEol:=False;
  ReadSkipSpace;
  while FBufIdx<FBufLen do begin
    ch:=FBuffer[FBufIdx];
    if not bReadEol then begin
      { break on delimiter or space }
      if (ch<=#32) or
         ((ch = ';') and (Result<>'')) or
         ((ch = '{') and (Result<>''))
      then
        break;
      { check comment }
      if (ch='#') and (Result='') then
        bReadEol:=True;
    end else
      { read end of line }
      if ch in [#10,#13] then
        break;
    Result:=Result+ch;

    Inc(FBufIdx);
    if not ReadBufferChk then
      break;
  end;
end;

function TNginxConfigParser.Eof: Boolean;
begin
  ReadBufferChk;
  Result:=FBufLen<=0;
end;

procedure TNginxConfigParser.ReadSkipSpace;
var
  ch:char;
begin
  while ReadBufferChk and (FBufIdx<FBufLen) do begin
    ch:=FBuffer[FBufIdx];
    if ch>#32 then
      break;
    Inc(FBufIdx);
  end;
end;

procedure TNginxConfigParser.IterGroupSave(Obj: TNginxItemGroup);
var
  stemp:string;
  temp:TObject;
  dummy:Pointer;
begin
  for dummy in Obj.ItemList do begin
    temp:=TObject(dummy);
    stemp:=DupeString(#9,(temp as TNginxItem).Level)+
           (temp as TNginxItem).NameItem+
           ' '+
           (temp as TNginxItem).Value+
           LineEnding;
    if Assigned(FStream) then
      FStream.Write(stemp[1],Length(stemp));
    if temp is TNginxItemGroup then
      IterGroupSave(TNginxItemGroup(temp));
  end;
end;

constructor TNginxConfigParser.Create;
begin
  inherited Create;
  FBufIdx:=0;
  FBufLen:=0;
  FCurLevel:=0;
  FillChar(FBuffer,sizeof(FBuffer),0);
  { root item }
  FItem:=TNginxItemGroup.Create;
  FItem.NameItem:='';
end;

destructor TNginxConfigParser.Destroy;
begin
  FItem.Free;
  inherited Destroy;
end;

procedure TNginxConfigParser.LoadFromStream(Stream: TStream);
var
  sName, sValue, sTemp: string;
  temp: TNginxItemGroup;
  iLVL, iCurr: Integer;
begin
  FStream:=Stream;
  FItem.ItemList.Clear;
  FItem.NameItem:='';
  temp:=FItem;
  iLVL:=0;
  while not Eof do begin
    sName:=ReadToken;
    sValue:='';
    if (sName<>'}') and ((sName<>'') and (sName[1]<>'#')) then begin
      { read all values }
      repeat
        sTemp:=ReadToken;
        if (sValue<>'') and (sTemp<>';') then
          sValue:=sValue+' ';
        sValue:=sValue+sTemp;
      until (sTemp=';') or (sTemp='{') or (FBufLen=0);
      { add group or item }
      if stemp='{' then begin
        iCurr:=temp.AddNameGroup(iLVL,sName,sValue);
        temp:=TNginxItemGroup(temp.ItemList[iCurr]);
        Inc(iLVL);
      end else
        iCurr:=temp.AddNameValue(iLVL,sName,sValue);
    end else begin
      iCurr:=temp.AddNameValue(iLVL,sName,sValue);
      { group end bracket }
      if sName='}' then begin
        if temp.Parents<>nil then
          temp:=temp.Parents;
        Dec(iLVL);
      end
    end;
    if Assigned(FOnToken) then
      FOnToken(sName,sValue);
  end;
end;

// utf8
procedure TNginxConfigParser.Load(FileName: string);
var
  fsFile:TFileStream;
begin
  fsFile:=TFileStream.Create(UTF8Decode(FileName),fmOpenRead);
  try
    LoadFromStream(fsFile);
  finally
    fsFile.Free;
    FStream:=nil;
  end;
end;

procedure TNginxConfigParser.SaveToStream(Stream: TStream);
var
  temp:TNginxItemGroup;
begin
  FStream:=Stream;
  temp:=FItem;
  IterGroupSave(temp);
end;

// utf8
procedure TNginxConfigParser.Save(FileName: string);
var
  fsFile:TFileStream;
begin
  fsFile:=TFileStream.Create(UTF8Decode(FileName),fmCreate or fmOpenWrite);
  try
    SaveToStream(fsFile);
  finally
    fsFile.Free;
    fsFile:=nil;
  end;
end;


{ TNginxItemGroup }

constructor TNginxItemGroup.Create;
begin
  inherited Create;
  FItemList:=TObjectList.Create(True);
  FParents:=nil;
end;

destructor TNginxItemGroup.Destroy;
begin
  FItemList.Free;
  inherited Destroy;
end;

function TNginxItemGroup.AddNameValue(lvl:Integer; const sName, sValue: string): Integer;
var
  temp, tail:TNginxItem;
begin
  temp:=TNginxItem.Create;
  try
    temp.Level:=lvl;
    temp.NameItem:=sName;
    temp.Value:=sValue;
    if (FItemList.Count=0) or
       (TNginxItem(FItemList[FItemList.Count-1]).NameItem<>'}')
    then
      Result:=FItemList.Add(temp)
    else begin
      FItemList.Insert(FItemList.Count-1,temp);
      Result:=FItemList.Count-2;
    end;
  except
    temp.Free;
  end;
end;

function TNginxItemGroup.InsertNameValue(Idx, lvl: Integer; const sName,
  sValue: string): TNginxItem;
var
  temp:TNginxItem;
begin
  Result:=nil;
  temp:=TNginxItem.Create;
  try
    temp.Level:=lvl;
    temp.NameItem:=sName;
    temp.Value:=sValue;
    FItemList.Insert(Idx,temp);
    Result:=temp;
  except
    temp.Free;
  end;
end;

function TNginxItemGroup.AddNameGroup(lvl: Integer; const sName, sValue: string
  ): Integer;
var
  temp:TNginxItemGroup;
begin
  temp:=TNginxItemGroup.Create;
  try
    temp.Level:=lvl;
    temp.NameItem:=sName;
    temp.Value:=sValue;
    temp.Parents:=self;
    if (FItemList.Count=0) or
       ((FItemList[FItemList.Count-1] is TNginxItem) and
        (TNginxItem(FItemList[FItemList.Count-1]).NameItem<>'}'))
    then
      Result:=FItemList.Add(temp)
    else begin
      FItemList.Insert(FItemList.Count-1,temp);
      Result:=FItemList.Count-2;
    end;
  except
    temp.Free;
  end;
end;

procedure TNginxItemGroup.MarkClose;
var
  temp:TNginxItem;
begin
  temp:=TNginxItem.Create;
  try
    temp.Level:=Level;
    temp.NameItem:='}';
    temp.Value:='';
    FItemList.Insert(FItemList.Count,temp);
  except
    temp.Free;
  end;
end;

function TNginxItemGroup.InsertNameGroup(Idx, lvl: Integer; const sName,
  sValue: string): TNginxItemGroup;
var
  temp:TNginxItemGroup;
begin
  Result:=nil;
  temp:=TNginxItemGroup.Create;
  try
    temp.Level:=lvl;
    temp.NameItem:=sName;
    temp.Value:=sValue;
    FItemList.Insert(Idx,temp);
    Result:=temp;
  except
    temp.Free;
  end;
end;

function TNginxItemGroup.FindItemName(const str: string): TNginxItem;
var
  iptr:Pointer;
  temp:TObject;
  stemp:string;
begin
  Result:=nil;
  for iptr in FItemList do begin
    temp:=TObject(iptr);
    if temp is TNginxItem then begin
      stemp:=(temp as TNginxItem).NameItem;
      if CompareText(str,stemp)=0 then
        Result:=(temp as TNginxItem);
    end;
  end;
end;

function TNginxItemGroup.FindItemNameNext(BaseItem: TNginxItem;
  const str: string): TNginxItem;
var
  i:Integer;
  iptr:Pointer;
  temp:TObject;
  stemp:string;
begin
  Result:=nil;
  i:=FItemList.IndexOf(BaseItem);
  for iptr in FItemList do begin
    temp:=TObject(iptr);
    if temp is TNginxItem then begin
      stemp:=(temp as TNginxItem).NameItem;
      if (FItemList.IndexOf(temp)<i) and (CompareText(str,stemp)=0) then
        Result:=(temp as TNginxItem);
    end;
  end;
end;

function TNginxItemGroup.FindItemGroup(const str: string): TNginxItemGroup;
var
  iptr:Pointer;
  temp:TObject;
  stemp:string;
begin
  Result:=nil;
  for iptr in FItemList do begin
    temp:=TObject(iptr);
    if temp is TNginxItemGroup then begin
      stemp:=(temp as TNginxItemGroup).NameItem;
      if CompareText(str,stemp)=0 then
        Result:=(temp as TNginxItemGroup);
    end;
  end;
end;

// Item search reverse order, return prev item
function TNginxItemGroup.FindItemGroupNext(BaseItem: TNginxItemGroup;
  const str: string): TNginxItemGroup;
var
  i:Integer;
  iptr:Pointer;
  temp:TObject;
  stemp:string;
begin
  Result:=nil;
  i:=FItemList.IndexOf(BaseItem);
  for iptr in FItemList do begin
    temp:=TObject(iptr);
    if temp is TNginxItemGroup then begin
      stemp:=(temp as TNginxItemGroup).NameItem;
      if (FItemList.IndexOf(temp)<i) and (CompareText(str,stemp)=0) then
        Result:=(temp as TNginxItemGroup);
    end;
  end;
end;

procedure TNginxItemGroup.DeleteItem(item: TObject);
var
  i: Integer;
begin
  i:=FItemList.IndexOf(item);
  FItemList.Delete(i);
end;

function TNginxItemGroup.IndexOfItem(item: TNginxItem): Integer;
begin
  Result:=FItemList.IndexOf(item);
end;


end.

