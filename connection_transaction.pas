unit connection_transaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs;

type

  { TConTran }

  TConTran = class(TDataModule)
    DBConnection: TIBConnection;
    DBTransaction: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
  end;

var
  ConTran: TConTran;

implementation

{$R *.lfm}

{ TConTran }

procedure TConTran.FormCreate(Sender: TObject);
begin
  DBConnection.Connected := True;
end;

end.

