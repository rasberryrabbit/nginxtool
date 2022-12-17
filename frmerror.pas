unit frmError;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormError }

  TFormError = class(TForm)
    MemoErr: TMemo;
  private

  public

  end;

var
  FormError: TFormError;

implementation

{$R *.lfm}

end.

