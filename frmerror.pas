unit frmError;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TFormError }

  TFormError = class(TForm)
    MemoErr: TMemo;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  FormError: TFormError;

implementation

{$R *.lfm}

{ TFormError }

procedure TFormError.FormShow(Sender: TObject);
begin
  Timer1.Enabled:=True;
end;

procedure TFormError.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=False;
  MemoErr.SelStart:=MemoErr.GetTextLen;
  MemoErr.Lines.Add('');
end;

end.

