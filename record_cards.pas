unit record_cards;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, metadata,
  StdCtrls, db, ExtCtrls, Buttons, edit_database, types, connection_transaction;

type

  TRecordCard = class;

  TActionType = (atInsert, atUpdate, atDelete);

  TCardsManager = class
  private
    FCardsList: TStringList;
    FRefreshRequest: TNotifyEvent;
    procedure CMInsertRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
    procedure CMUpdateRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
    procedure CMDeleteRecord(ATable: TDBTable; APrimaryKey: integer);
    procedure CardOkClicked(Sender: TObject);
    procedure CardClosed(Sender: TObject);
  public
    property OnRequestRefresh: TNotifyEvent read FRefreshRequest write FRefreshRequest;
    procedure EditTable(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
    constructor Create;
	end;

  TCustomEditClass = class of TCustomEdit;

  TCellEdit = class
  private
    pnlCellEdit: TPanel;
    lbTitle: TLabel;
    lbOldValue: TLabel;
    FValue: Variant;
    FTag: integer;
    FDisplayedField: TDBField;
    FReferringField: TDBField;
    FValueChanged: TNotifyEvent;
    procedure SetValue(Value: Variant); virtual; abstract;
  public
    property ReferringField: TDBField read FReferringField;
    property DisplayedField: TDBField read FDisplayedField;
    property OnValueChanged: TNotifyEvent read FValueChanged write FValueChanged;
    property Value: Variant read FValue write SetValue;
    property Tag: integer read FTag write FTag;
	end;

  TEditCellEdit = class (TCellEdit)
  private
    CellEditor: TCustomEdit;
    procedure SetValue(AValue: Variant); override;
  public
    procedure CellEditorChange(Sender: TObject);
    constructor Create(ADisplayedField: TDBField; APos: integer; ACard: TForm);
	end;

  TComboCellEdit = class (TCellEdit)
  private
    cbbValues: TComboBox;
    FIDsAsObjects: TIntegerDynArray;
    procedure SetValue(AValue: Variant); override;
  public
    procedure cbbValuesChange(Sender: TObject);
    constructor Create(ADisplayedField, AReferringField: TDBField; APos: integer; ACard: TForm);
	end;

	{ TRecordCard }

  TRecordCard = class(TForm)
  published
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    FCellEdits: array of TCellEdit;
    FActionType: TActionType;
    FTable: TDBTable;
    FPrimaryKey: integer;
    FOkClick: TNotifyEvent;
    FQueryFields: TStringList;
    FValuesList: TStringList;
    FCardClose: TNotifyEvent;
  public
    NewValues: TVariantDynArray;
    property OnOkClick: TNotifyEvent read FOkClick write FOkClick;
    property OnCardClose: TNotifyEvent read FCardClose write FCardClose;
    property PrimaryKey: integer read FPrimaryKey;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CellEditValueChange(Sender: TObject);
    constructor Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
  end;

  TEditRecordCard = class(TRecordCard)
  public
    constructor Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
  end;

  TRecordCardDynArray = array of TRecordCard;

var
  TypeOfEditor: array [Low(TFieldType)..High(TFieldType)] of TCustomEditClass;
  CardsManager: TCardsManager;

const
  CellEditPanelHeight = 70;

implementation

{$R *.lfm}

procedure TRecordCard.btnOkClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(FTable.Fields) do
    if (FTable.Fields[i].FieldRef = nil) then
      NewValues[i] := FValuesList.Strings[FValuesList.IndexOfObject(FTable.Fields[i])]
    else
      NewValues[i] := FValuesList.Strings[FValuesList.IndexOfObject(FTable.Fields[i].FieldRef)];

  case FActionType of
    atUpdate: UpdateRecord(FTable, FPrimaryKey, NewValues);
    atInsert: InsertRecord(FTable, NextID, NewValues);
  end;

  FOkClick(Sender);
  Close;
end;

procedure TRecordCard.btnCancelClick(Sender: TObject);
begin
  Close;
end;

constructor TRecordCard.Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
var
  i, k: integer;
  Field: TDBField;
begin
  inherited Create(Application);
  Caption := ATable.Caption + ': Добавить запись';
  Position := poDefault;
  FActionType := AActionType;
  FTable := ATable;
  OnClose := @FormClose;
  FPrimaryKey := APrimaryKey;
  FQueryFields := TStringList.Create;
  AllQueryColumnsToList(ATable, FQueryFields);
  FValuesList := TStringList.Create;
  SetLength(NewValues, Length(ATable.Fields));

  for i := 0 to FQueryFields.Count - 1 do begin
    Field := FQueryFields.Objects[i] as TDBField;
    k := 0;

    if Field.Visible then begin
      repeat
        inc(k)
      until
        (k = FQueryFields.Count) or
        (Assigned((FQueryFields.Objects[k] as TDBField).FieldRef) and
        ((FQueryFields.Objects[k] as TDBField).FieldRef.TableOwner = Field.TableOwner));

      SetLength(FCellEdits, Length(FCellEdits) + 1);
      if (k = FQueryFields.Count) then begin
        FCellEdits[High(FCellEdits)] := TEditCellEdit.Create
                                        (Field, High(FCellEdits), Self);
        if (FValuesList.IndexOfObject(Field) < 0) then
          FValuesList.AddObject('', Field);
			end
			else begin
        FCellEdits[High(FCellEdits)] := TComboCellEdit.Create
                                        (Field,
                                         (FQueryFields.Objects[k] as TDBField).FieldRef,
                                         High(FCellEdits),
                                         Self);
        if (FValuesList.IndexOfObject((FQueryFields.Objects[k] as TDBField).FieldRef) < 0) then
          FValuesList.AddObject('', (FQueryFields.Objects[k] as TDBField).FieldRef);
      end;
      FCellEdits[High(FCellEdits)].OnValueChanged := @CellEditValueChange;
    end;

    if (not Field.Visible) and (Field.TableOwner = ATable) and (Field.FieldRef = nil) then
      if (FValuesList.IndexOfObject(Field) < 0) then
        FValuesList.AddObject('', Field);
	end;

  Height := CellEditPanelHeight * (1 + Length(FCellEdits)) + 30;
  BorderStyle := bsSingle;
end;

constructor TEditRecordCard.Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
var
  i: integer;
begin
  inherited Create(ATable, APrimaryKey, AActionType);
  Caption := ATable.Caption + ': Изменить запись';

  ConTran.CommonSQLQuery.Close;
  SetSQLQuery(ATable, ConTran.CommonSQLQuery);
  with ConTran.CommonSQLQuery, SQL do begin
    Append('where ' + ATable.Name + '.id = :primary_key');
    ParamByName('primary_key').Value := APrimaryKey;
    Open;

    for i := 0 to High(FCellEdits) do begin
      FCellEdits[i].Value := FieldByName(FCellEdits[i].DisplayedField.TableOwner.Name + FCellEdits[i].DisplayedField.Name).Value;

		end;
	end;
end;

procedure TRecordCard.CellEditValueChange(Sender: TObject);
begin
  FValuesList.Strings[FValuesList.IndexOfObject((Sender as TCellEdit).ReferringField)] := (Sender as TCellEdit).Value;
end;

procedure TRecordCard.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FCardClose(Self);
  CloseAction := caFree;
end;

constructor TEditCellEdit.Create(ADisplayedField: TDBField; APos: integer; ACard: TForm);
begin
  FDisplayedField := ADisplayedField;
  FReferringField := ADisplayedField;

  pnlCellEdit := TPanel.Create(ACard);
  with pnlCellEdit do begin
    Parent := ACard;
    Height := CellEditPanelHeight;
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
    Anchors := [akLeft, akRight];
	end;

  lbOldValue := TLabel.Create(pnlCellEdit);
  with lbOldValue do begin
    Parent := pnlCellEdit;
    AutoSize := false;
    Top := CellEditor.Top + CellEditor.Height;
    Left := CellEditor.Left;
    Width := 500;
    Caption := CellEditor.Text;
	end;

  ACard.ActiveControl := CellEditor;
end;

constructor TComboCellEdit.Create(ADisplayedField, AReferringField: TDBField; APos: integer; ACard: TForm);
begin
  FDisplayedField := ADisplayedField;
  FReferringField := AReferringField;

  pnlCellEdit := TPanel.Create(ACard);
  with pnlCellEdit do begin
    Parent := ACard;
    Height := CellEditPanelHeight;
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
    AutoSize := false;
    Top := 10;
    Left := lbTitle.Left + lbTitle.Width;
    Height := 30;
    Width := 320;
    Style := csDropDownList;
    ADisplayedField.RowsTo(cbbValues, FIDsAsObjects);
    OnChange := @cbbValuesChange;
  end;
  cbbValuesChange(cbbValues);

  lbOldValue := TLabel.Create(pnlCellEdit);
  with lbOldValue do begin
    Parent := pnlCellEdit;
    AutoSize := false;
    Top := cbbValues.Top + cbbValues.Height;
    Left := cbbValues.Left;
    Width := 500;
	end;
end;

procedure TComboCellEdit.cbbValuesChange(Sender: TObject);
begin
  FValue := FIDsAsObjects[(Sender as TComboBox).ItemIndex];
  if Assigned(FValueChanged) then
    FValueChanged(Self);
end;

procedure TEditCellEdit.CellEditorChange(Sender: TObject);
begin
  FValue := (Sender as TEdit).Text;
  if Assigned(FValueChanged) then
    FValueChanged(Self);
end;

procedure TEditCellEdit.SetValue(AValue: Variant);
begin
  CellEditor.Text := AValue;
  FValue := AValue;
end;

procedure TComboCellEdit.SetValue(AValue: Variant);
begin
  cbbValues.ItemIndex := cbbValues.Items.IndexOf(AValue);
  cbbValuesChange(cbbValues);
end;

constructor TCardsManager.Create;
begin
  FCardsList := TStringList.Create;
end;

procedure TCardsManager.EditTable(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
begin
  case AActionType of
    atInsert: CMInsertRecord(ATable, APrimaryKey, AActionType);
    atUpdate: CMUpdateRecord(ATable, APrimaryKey, AActionType);
    atDelete: CMDeleteRecord(ATable, APrimaryKey);
	end;
end;

procedure TCardsManager.CMInsertRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
begin
  FCardsList.AddObject(IntToStr(APrimaryKey), TRecordCard.Create(ATable, APrimaryKey, AActionType));
  with (FCardsList.Objects[FCardsList.Count - 1] as TRecordCard) do begin
    OnOkClick := @CardOkClicked;
    OnCardClose := @CardClosed;
    Show;
	end;
end;

procedure TCardsManager.CMUpdateRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
begin
  with FCardsList do begin
    if IndexOf(IntToStr(APrimaryKey)) = -1 then begin
      FCardsList.AddObject(IntToStr(APrimaryKey), TEditRecordCard.Create(ATable, APrimaryKey, AActionType));
      with (Objects[FCardsList.Count - 1] as TEditRecordCard) do begin
        OnOkClick := @CardOkClicked;
        OnCardClose := @CardClosed;
        Show;
			end;
		end
		else
      (Objects[IndexOf(IntToStr(APrimaryKey))] as TEditRecordCard).SetFocus;
	end;
end;

procedure TCardsManager.CMDeleteRecord(ATable: TDBTable; APrimaryKey: integer);
begin
  DeleteRecord(ATable, APrimaryKey);
  with FCardsList do begin
    if IndexOf(IntToStr(APrimaryKey)) <> -1 then begin
      (Objects[IndexOf(IntToStr(APrimaryKey))] as TEditRecordCard).Close;
      Delete(IndexOf(IntToStr(APrimaryKey)));
  	end;
	end;
end;

procedure TCardsManager.CardOkClicked(Sender: TObject);
begin
  FRefreshRequest(Sender);
end;

procedure TCardsManager.CardClosed(Sender: TObject);
begin
  FCardsList.Delete(FCardsList.IndexOf(IntToStr((Sender as TRecordCard).PrimaryKey)));
end;

initialization

  CardsManager := TCardsManager.Create;


end.
















