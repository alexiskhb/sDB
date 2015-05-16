unit cell_contents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, types;

type

  { TCellContents }

  TCellContents = class(TForm)
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

{ TCellContents }

procedure TCellContents.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TCellContents.ContentMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Content.Font.Size := Content.Font.Size - 1;
end;

procedure TCellContents.ContentMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Content.Font.Size := Content.Font.Size + 1;
end;

procedure TCellContents.FormDeactivate(Sender: TObject);
begin
  Close;
end;

end.

