unit record_cards;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, metadata,
  StdCtrls, db, ExtCtrls, DBGrids;

type

  TCustomEditClass = class of TCustomEdit;

  TCellEdit = class
  private
    cbbValues: TComboBox;
    pnlCellEdit: TPanel;
    CellEditor: TCustomEdit;
    lbTitle: TLabel;
    FValue: Variant;
    FReferringField: TDBField;
    FDisplayedField: TDBField;
    procedure cbbValuesSelect(Sender: TObject);
    procedure CellEditorChange(Sender: TObject);
    procedure SetValue(Value: Variant);
  public
    property Value: Variant read FValue write SetValue;
    constructor Create(ADisplayedField: TDBField; APos: integer; ACard: TForm); overload;
    constructor Create(AReferringField, ADisplayedField: TDBField; APos: integer; ACard: TForm); overload;
	end;

  TRecordCard = class(TForm)
  private
    FCellEdits: array of TCellEdit;
  public
    Values: TVariantDynArray;
    NewValues: TVariantDynArray;
    constructor Create(ATable: TDBTable; AFields: TStringList);
  end;

  TEditRecordCard = class(TRecordCard)
  public
    OldValues: TVariantDynArray;
    constructor Create(ATable: TDBTable; AFields: TStringList; AGrid: TDBGrid);
	end;

var
  TypeOfEditor: array [Low(TFieldType)..High(TFieldType)] of TCustomEditClass;

implementation

{$R *.lfm}

constructor TRecordCard.Create(ATable: TDBTable; AFields: TStringList);
var
  i: integer;
begin
  inherited Create(Application);
  Caption := 'Добавить запись';

  for i := 0 to AFields.Count - 1 do begin
    SetLength(FCellEdits, Length(FCellEdits) + 1);
    if ATable.Fields[i].FieldRef = nil then
      FCellEdits[High(FCellEdits)] := TCellEdit.Create(AFields.Objects[i] as TDBField, i, Self)
    else
      FCellEdits[High(FCellEdits)] := TCellEdit.Create(ATable.Fields[i].FieldRef, AFields.Objects[i] as TDBField, i, Self);
	end;

end;

constructor TEditRecordCard.Create(ATable: TDBTable; AFields: TStringList; AGrid: TDBGrid);
var
  i: integer;
begin
  inherited Create(ATable, AFields);
  Caption := 'Изменить запись';
  for i := 0 to AFields.Count - 1 do
    FCellEdits[i].Value :=
    AGrid.DataSource.DataSet.FieldByName
    ((AFields.Objects[i] as TDBField).TableOwner.Name + (AFields.Objects[i] as TDBField).Name).Value;
end;

constructor TCellEdit.Create(ADisplayedField: TDBField; APos: integer; ACard: TForm);
begin
  FDisplayedField := ADisplayedField;
  FReferringField := nil;
  cbbValues := nil;

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
    Width := 120;
    Caption := ADisplayedField.Caption;
	end;

  CellEditor := TEdit.Create(pnlCellEdit);
  with CellEditor do begin
    Parent := pnlCellEdit;
    OnChange := @CellEditorChange;
    AutoSize := false;
    Top := 10;
    Left := lbTitle.Left + lbTitle.Width;
    Height := 30;
    Width := pnlCellEdit.Width;
    MaxLength := ADisplayedField.VarCharLimit;
	end;
end;

constructor TCellEdit.Create(AReferringField, ADisplayedField: TDBField; APos: integer; ACard: TForm);
begin
  FDisplayedField := ADisplayedField;
  FReferringField := AReferringField;
  CellEditor := nil;

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
    Width := 120;
    Caption := ADisplayedField.Caption;
	end;

  cbbValues := TComboBox.Create(pnlCellEdit);
  with cbbValues do begin
    Parent := pnlCellEdit;
    OnSelect := @cbbValuesSelect;
    AutoSize := false;
    Top := 10;
    Left := lbTitle.Left + lbTitle.Width;
    Height := 30;
    Width := pnlCellEdit.Width;
    Style := csDropDownList;
    ADisplayedField.RowsTo(Items);
    ItemIndex := 0;
    FValue := Items[0];
	end;

end;

procedure TCellEdit.cbbValuesSelect(Sender: TObject);
begin
  FValue :=
    AppropriateValue(FDisplayedField,
                     (Sender as TCombobox).Items[(Sender as TCombobox).ItemIndex],
                     FReferringField)
end;

procedure TCellEdit.CellEditorChange(Sender: TObject);
begin
  FValue := (Sender as TEdit).Text;
end;

procedure TCellEdit.SetValue(Value: Variant);
begin
  if CellEditor <> nil then
    CellEditor.Text := Value
  else if cbbValues <> nil then
    cbbValues.ItemIndex := cbbValues.Items.IndexOf(Value);
end;

end.
















