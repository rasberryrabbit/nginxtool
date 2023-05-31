program nginxtool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, nginxtool_main, uniqueinstance_package, ExceptionLogger, uConfigParser,
  frmError
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormNginxtool, FormNginxtool);
  Application.Run;
end.

