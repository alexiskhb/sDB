unit cell_contents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, types;

type

  { TCellContentsForm }

  TCellContentsForm = class(TForm)
    Content: TMemo;
    procedure ContentMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ContentMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
  end;

implementation

{$R *.lfm}

{ TCellContentsForm }

procedure TCellContentsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TCellContentsForm.ContentMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Content.Font.Size := Content.Font.Size - 1;
end;

procedure TCellContentsForm.ContentMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Content.Font.Size := Content.Font.Size + 1;
end;

procedure TCellContentsForm.FormDeactivate(Sender: TObject);
begin
  Close;
end;

end.

