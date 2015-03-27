program sdb;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  connection_transaction,
  DBMain,
  FormConnect,
  metadata,
  aboutsdb,
  listview { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TConTran, ConTran);
  Application.CreateForm(TConnectForm, ConnectForm);
  Application.CreateForm(TAboutProg, AboutProg);
  Application.Run;
end.
