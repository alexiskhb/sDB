unit FormConnect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, connection_transaction;

type

  { TConnectForm }

  TConnectForm = class(TForm)
    BrowseDBPath: TButton;
    OkButton: TButton;
    CanselButton: TButton;
    DBUserName: TLabeledEdit;
    DBPassword: TLabeledEdit;
    DBPath: TLabeledEdit;
    BrowseDBPathDialog: TOpenDialog;
    procedure BrowseDBPathClick(Sender: TObject);
    procedure CanselButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  end;

var
  ConnectForm: TConnectForm;

implementation

{$R *.lfm}

{ TConnectForm }

procedure TConnectForm.FormShow(Sender: TObject);
begin
  DBPath.Text := ConTran.DBConnection.DatabaseName;
  DBUserName.Text := ConTran.DBConnection.UserName;
  DBPassword.Text := '';
  ActiveControl := DBPassword;
end;

procedure TConnectForm.BrowseDBPathClick(Sender: TObject);
begin
  if BrowseDBPathDialog.Execute then
    DBPath.Text := BrowseDBPathDialog.FileName;
end;

procedure TConnectForm.CanselButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  ConnectForm.Hide;
end;

procedure TConnectForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    13: OkButtonClick(OkButton);
    27: CanselButtonClick(CanselButton);
  end;
end;

procedure TConnectForm.OkButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
  ConnectForm.Hide;
end;

end.


