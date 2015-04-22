unit record_cards;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, metadata, StdCtrls, db, ExtCtrls;

type

  TCustomEditClass = class of TCustomEdit;

  TCellEdit = class
  private
    cbbValues: TComboBox;
    pnlCellEdit: TPanel;
    CellEditor: TCustomEdit;
    lbTitle: TLabel;
    FValue: Variant;
  public
    property Value: Variant read FValue;
    constructor Create(AField: TDBField; APos: integer; ACard: TForm); overload;
    constructor Create(ATableRef: TDBTable; AField: TDBField; APos: integer; ACard: TForm); overload;
	end;

  TRecordCard = class(TForm)
  private
    FCellEdits: array of TCellEdit;
  public
    Values: TVariantDynArray;
    NewValues: TVariantDynArray;
    constructor Create(ATable: TDBTable; ACaption: string; AFields: TStringList);
  end;

var
  TypeOfEditor: array [Low(TFieldType)..High(TFieldType)] of TCustomEditClass;

implementation

{$R *.lfm}

constructor TRecordCard.Create(ATable: TDBTable; ACaption: string; AFields: TStringList);
var
  i: integer;
begin
  inherited Create(Application);
  Caption := ACaption;

  for i := 0 to AFields.Count - 1 do begin
    SetLength(FCellEdits, Length(FCellEdits) + 1);
    FCellEdits[High(FCellEdits)] := TCellEdit.Create(ATable, AFields.Objects[i] as TDBField, i, Self);
	end;

end;

constructor TCellEdit.Create(AField: TDBField; APos: integer; ACard: TForm);
begin
  pnlCellEdit := TPanel.Create(ACard);
  with pnlCellEdit do begin
    Parent := ACard;
    Height := 50;
    Align := alTop;
    Top := APos * Height;
  end;

end;

constructor TCellEdit.Create(ATableRef: TDBTable; AField: TDBField; APos: integer; ACard: TForm);
begin
  pnlCellEdit := TPanel.Create(ACard);
  with pnlCellEdit do begin
    Parent := ACard;
    Height := 50;
    Align := alTop;
    Top := APos * Height;
  end;

  lbTitle := TLabel.Create(pnlCellEdit);
  with lbTitle do begin
    Parent := pnlCellEdit;
    AutoSize := false;
    Top := 10;
    Left := 8;
    Width := 100;
    Caption := AField.Caption;
	end;

  cbbValues := TComboBox.Create(pnlCellEdit);
  with cbbValues do begin
    Parent := pnlCellEdit;
    AutoSize := false;
    Top := 10;
    Left := lbTitle.Left + lbTitle.Width;
    Height := 30;
    Width := pnlCellEdit.Width;
    Style := csDropDownList;
    AField.RowsTo(Items);
	end;

end;

end.
















