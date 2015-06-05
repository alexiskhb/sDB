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
    FRefreshTablesRequest: TNotifyEvent;
    FShouldShow: boolean;
    procedure CMInsertRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType); overload;
    procedure CMInsertRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType;
      Field1, Field2: TDBField; ID1, ID2: integer); overload;
    procedure CMUpdateRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType); overload;
    procedure CMUpdateRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType;
      Field1, Field2: TDBField; ID1, ID2: integer); overload;
    procedure CMDeleteRecord(ATable: TDBTable; APrimaryKey: integer);
    procedure CardOkClicked(Sender: TObject);
    procedure CardClosed(Sender: TObject);
    procedure RefreshValuesInCards(Sender: TObject);
  public
    property ShouldShow: boolean read FShouldShow write FShouldShow;
    property OnRequestRefreshTables: TNotifyEvent read FRefreshTablesRequest write FRefreshTablesRequest;
    procedure EditTable(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType); overload;
    procedure EditTable(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType;
      Field1, Field2: TDBField; ID1, ID2: integer); overload;
    procedure OkCloseLast;
    constructor Create;
  end;

  TCellEdit = class
  private
    pnlCellEdit: TPanel;
    lbTitle: TLabel;
    lbOldValue: TLabel;
    FValue: Variant;
    FValueID: integer;
    FTag: integer;
    FDisplayedField: TDBField;
    FReferringField: TDBField;
    FValueChanged: TNotifyEvent;
    procedure SetValue(Value: Variant); virtual; abstract;
    procedure SetValueID(Value: Integer); virtual; abstract;
    function GetCaption: string; virtual; abstract;
  public
    property ReferringField: TDBField read FReferringField;
    property DisplayedField: TDBField read FDisplayedField;
    property OnValueChanged: TNotifyEvent read FValueChanged write FValueChanged;
    property Value: Variant read FValue write SetValue;
    property ValueID: integer read FValueID write SetValueID;
    property Caption: string read GetCaption;
    property Tag: integer read FTag write FTag;
    constructor Create(ADisplayedField: TDBField; APos: integer; ACard: TForm);
  end;

  TEditCellEdit = class (TCellEdit)
  private
    CellEditor: TCustomEdit;
    procedure SetValue(AValue: Variant); override;
    function GetCaption: string; override;
    procedure CellEditorChange(Sender: TObject);
  public
    constructor Create(ADisplayedField: TDBField; APos: integer; ACard: TForm);
  end;

  TComboCellEdit = class (TCellEdit)
  private
    cbbValues: TComboBox;
    procedure SetValue(AValue: Variant); override;
    procedure SetValueID(AValue: Integer); override;
    function GetCaption: string; override;
    procedure cbbValuesChange(Sender: TObject);
  public
    procedure RefreshValues;
    constructor Create(ADisplayedField, AReferringField: TDBField; APos: integer; ACard: TForm);
  end;

	{ TRecordCard }

  TRecordCard = class(TForm)
  private
    FCellEdits: array of TCellEdit;
    FActionType: TActionType;
    FTable: TDBTable;
    FPrimaryKey: integer;
    FOkClick: TNotifyEvent;
    FQueryFields: TStringList;
    FValuesList: TStringList;
    FCardClose: TNotifyEvent;
    FRequestRefreshCards: TNotifyEvent;
    FRequestRememberRecNO: TNotifyEvent;
  public
    NewValues: TVariantDynArray;
    property OnOkClick: TNotifyEvent read FOkClick write FOkClick;
    property OnCardClose: TNotifyEvent read FCardClose write FCardClose;
    property PrimaryKey: integer read FPrimaryKey;
    property Table: TDBTable read FTable;
    property OnRequestRefreshCards: TNotifyEvent read FRequestRefreshCards write FRequestRefreshCards;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CellEditValueChange(Sender: TObject);
    constructor Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
  published
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    cbNoClose: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  end;

  TFixedRecordCard = class(TRecordCard)
    constructor Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType;
      Field1, Field2: TDBField; ID1, ID2: integer);
  end;

  TEditRecordCard = class(TRecordCard)
    btnDeleteThisRecord: TButton;
    procedure btnDeleteThisRecordMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
  end;

  TFixedEditRecordCard = class(TEditRecordCard)
    constructor Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType;
      Field1, Field2: TDBField; ID1, ID2: integer);
  end;

  TRecordCardDynArray = array of TRecordCard;

var
  CardsManager: TCardsManager;

const
  CellEditPanelHeight = 70;

implementation

{$R *.lfm}

procedure TRecordCard.btnOkClick(Sender: TObject);
var
  i: integer;
  EditResult: integer;
begin
  for i := 0 to High(FTable.Fields) do
    if (FTable.Fields[i].FieldRef = nil) then
      NewValues[i] := FValuesList.Strings[FValuesList.IndexOfObject(FTable.Fields[i])]
    else
      NewValues[i] := FValuesList.Strings[FValuesList.IndexOfObject(FTable.Fields[i].FieldRef)];

  case FActionType of
    atUpdate: EditResult := UpdateRecord(FTable, FPrimaryKey, NewValues);
    atInsert: EditResult := InsertRecord(FTable, NextID, NewValues);
  end;

  FRequestRefreshCards(FTable);
  FOkClick(Sender);
  if (EditResult = 0) and (not cbNoClose.Checked) then Close
  else Visible := true;
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
  Caption := ATable.Caption + ': добавить запись';
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
        FCellEdits[High(FCellEdits)] :=
          TEditCellEdit.Create(Field, High(FCellEdits), Self);
        if (FValuesList.IndexOfObject(Field) < 0) then
          FValuesList.AddObject('', Field);
      end
      else begin
        FCellEdits[High(FCellEdits)] := TComboCellEdit.Create(
          Field, (FQueryFields.Objects[k] as TDBField).FieldRef,
          High(FCellEdits), Self);
        if (FValuesList.IndexOfObject((FQueryFields.Objects[k] as TDBField).FieldRef) < 0) then
          FValuesList.AddObject('', (FQueryFields.Objects[k] as TDBField).FieldRef);
      end;
      FCellEdits[High(FCellEdits)].OnValueChanged := @CellEditValueChange;
    end;

    if (not Field.Visible) and (Field.TableOwner = ATable) and (Field.FieldRef = nil) then
      if (FValuesList.IndexOfObject(Field) < 0) then
        FValuesList.AddObject('', Field);
  end;

  Height := CellEditPanelHeight * (1 + Length(FCellEdits));
  btnCancel.Top := Height - CellEditPanelHeight div 2 - btnCancel.Height div 2;
  btnOk.Top := btnCancel.Top;
  cbNoClose.Top := btnOk.Top - cbNoClose.Height;
  BorderStyle := bsSingle;
end;

constructor TFixedRecordCard.Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType;
  Field1, Field2: TDBField; ID1, ID2: integer);
var
  i: integer;
begin
  inherited Create(ATable, APrimaryKey, AActionType);
  for i := 0 to High(FCellEdits) do begin
    if FCellEdits[i].DisplayedField = Field1 then begin
      FCellEdits[i].ValueID := ID1;
      FCellEdits[i].pnlCellEdit.Enabled := false;
    end;
    if FCellEdits[i].DisplayedField = Field2 then begin
      FCellEdits[i].ValueID := ID2;
      FCellEdits[i].pnlCellEdit.Enabled := false;
    end;
  end;
end;

constructor TEditRecordCard.Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
var
  i: integer;
begin
  inherited Create(ATable, APrimaryKey, AActionType);
  Caption := ATable.Caption + ': изменить запись';
  cbNoClose.Visible := false;

  ConTran.CommonSQLQuery.Close;
  SetSQLQuery(ATable, ConTran.CommonSQLQuery);
  with ConTran.CommonSQLQuery, SQL do begin
    Append('where ' + ATable.Name + '.id = :primary_key');
    ParamByName('primary_key').Value := APrimaryKey;
    Open;

    for i := 0 to High(FCellEdits) do begin
      FCellEdits[i].Value := FieldByName(FCellEdits[i].DisplayedField.TableOwner.Name + FCellEdits[i].DisplayedField.Name).Value;
      FCellEdits[i].lbOldValue.Caption := 'Было: ' + FCellEdits[i].Caption;
    end;
  end;
  btnDeleteThisRecord := TButton.Create(Self);
  with btnDeleteThisRecord do begin
    Parent := Self;
    Height := 25;
    Width := 52;
    Font.Size := 9;
    Caption := 'Удалить';
    OnMouseUp := @btnDeleteThisRecordMouseUp;
    Top := Self.Height - Height - 2;
    Left := 2;
  end;
end;

constructor TFixedEditRecordCard.Create(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType;
  Field1, Field2: TDBField; ID1, ID2: integer);
var
  i: integer;
begin
  inherited Create(ATable, APrimaryKey, AActionType);
  for i := 0 to High(FCellEdits) do begin
    if FCellEdits[i].DisplayedField = Field1 then begin
      FCellEdits[i].ValueID := ID1;
      FCellEdits[i].pnlCellEdit.Enabled := false;
    end;
    if FCellEdits[i].DisplayedField = Field2 then begin
      FCellEdits[i].ValueID := ID2;
      FCellEdits[i].pnlCellEdit.Enabled := false;
    end;
  end;
end;

procedure TRecordCard.CellEditValueChange(Sender: TObject);
begin
  FValuesList.Strings[FValuesList.IndexOfObject((Sender as TCellEdit).ReferringField)] := (Sender as TCellEdit).Value;
end;

procedure TEditRecordCard.btnDeleteThisRecordMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if MessageDlg('Удалить запись?', mtConfirmation, mbOKCancel, 0) = 1 then
    CardsManager.EditTable(FTable, FPrimaryKey, atDelete);
end;

procedure TRecordCard.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FCardClose(Self);
  CloseAction := caFree;
end;

constructor TCellEdit.Create(ADisplayedField: TDBField; APos: integer; ACard: TForm);
begin
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
    Top := pnlCellEdit.Height div 2 - Height;
    Left := ACard.Width div 32;
    Width := 4 * ACard.Width div 16;
    Caption := ADisplayedField.Caption;
  end;
end;

constructor TEditCellEdit.Create(ADisplayedField: TDBField; APos: integer; ACard: TForm);
begin
  inherited Create(ADisplayedField, APos, ACard);

  FDisplayedField := ADisplayedField;
  FReferringField := ADisplayedField;

  CellEditor := TEdit.Create(pnlCellEdit);
  with CellEditor do begin
    Parent := pnlCellEdit;
    OnChange := @CellEditorChange;
    AutoSize := false;
    Top := pnlCellEdit.Height div 2 - Height;
    Left := lbTitle.Left + lbTitle.Width;
    Height := 30;
    Width := 11 * ACard.Width div 16;
    MaxLength := ADisplayedField.VarCharLimit;
  end;

  lbOldValue := TLabel.Create(pnlCellEdit);
  with lbOldValue do begin
    Parent := pnlCellEdit;
    AutoSize := false;
    Top := CellEditor.Top + CellEditor.Height;
    Left := CellEditor.Left;
    Width := CellEditor.Width;
    Caption := CellEditor.Text;
    Font.Size := 9;
  end;

  ACard.ActiveControl := CellEditor;
end;

constructor TComboCellEdit.Create(ADisplayedField, AReferringField: TDBField; APos: integer; ACard: TForm);
begin
  inherited Create(ADisplayedField, APos, ACard);

  FDisplayedField := ADisplayedField;
  FReferringField := AReferringField;

  cbbValues := TComboBox.Create(pnlCellEdit);
  with cbbValues do begin
    Parent := pnlCellEdit;
    AutoSize := false;
    Top := pnlCellEdit.Height div 2 - Height;
    Left := lbTitle.Left + lbTitle.Width;
    Height := 30;
    Width := 11 * ACard.Width div 16;
    Style := csDropDownList;
    ADisplayedField.RowsTo(cbbValues);
    OnChange := @cbbValuesChange;
    Sorted := true;
  end;
  cbbValuesChange(cbbValues);

  lbOldValue := TLabel.Create(pnlCellEdit);
  with lbOldValue do begin
    Parent := pnlCellEdit;
    AutoSize := false;
    Top := cbbValues.Top + cbbValues.Height;
    Left := cbbValues.Left;
    Width := cbbValues.Width;
    Font.Size := 9;
  end;
end;

procedure TComboCellEdit.cbbValuesChange(Sender: TObject);
begin
  if (Sender as TComboBox).ItemIndex >= 0 then
    FValue := Integer(Pointer((Sender as TComboBox).Items.Objects[(Sender as TComboBox).ItemIndex]));
  if Assigned(FValueChanged) then
    FValueChanged(Self);
end;

procedure TComboCellEdit.RefreshValues;
var
  MemID: integer;
begin
  MemID := -1;
  if cbbValues.ItemIndex <> -1 then
    MemID := Integer(Pointer(cbbValues.Items.Objects[cbbValues.ItemIndex]));
  FDisplayedField.RowsTo(cbbValues);
  if MemID <> -1 then
    cbbValues.ItemIndex := cbbValues.Items.IndexOfObject(TObject(Pointer(MemID)));
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

procedure TComboCellEdit.SetValueID(AValue: integer);
begin
  cbbValues.ItemIndex := cbbValues.Items.IndexOfObject(TObject(Pointer((AValue))));
  cbbValuesChange(cbbValues);
end;

function TEditCellEdit.GetCaption: string;
begin
  Result := CellEditor.Text;
end;

function TComboCellEdit.GetCaption: string;
begin
  Result := cbbValues.Items[cbbValues.ItemIndex];
end;

constructor TCardsManager.Create;
begin
  FCardsList := TStringList.Create;
  FShouldShow := true;
end;

procedure TCardsManager.EditTable(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
begin
  case AActionType of
    atInsert: CMInsertRecord(ATable, APrimaryKey, AActionType);
    atUpdate: CMUpdateRecord(ATable, APrimaryKey, AActionType);
    atDelete: CMDeleteRecord(ATable, APrimaryKey);
  end;
end;

procedure TCardsManager.EditTable(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType;
  Field1, Field2: TDBField; ID1, ID2: integer);
begin
  case AActionType of
    atInsert: CMInsertRecord(ATable, APrimaryKey, AActionType, Field1, Field2, ID1, ID2);
    atUpdate: CMUpdateRecord(ATable, APrimaryKey, AActionType, Field1, Field2, ID1, ID2);
    atDelete: CMDeleteRecord(ATable, APrimaryKey);
  end;
end;

procedure TCardsManager.OkCloseLast;
var
  Card: TRecordCard;
begin
  with FCardsList do begin
    Card := Objects[Count - 1] as TRecordCard;
    Card.btnOkClick(Card.btnOk);
  end;
end;

procedure TCardsManager.CMInsertRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType);
begin
  FCardsList.AddObject(IntToStr(APrimaryKey), TRecordCard.Create(ATable, APrimaryKey, AActionType));
  with (FCardsList.Objects[FCardsList.Count - 1] as TRecordCard) do begin
    OnOkClick := @CardOkClicked;
    OnCardClose := @CardClosed;
    OnRequestRefreshCards := @RefreshValuesInCards;
    if FShouldShow then Show;
  end;
end;

procedure TCardsManager.CMInsertRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType;
  Field1, Field2: TDBField; ID1, ID2: integer);
begin
  FCardsList.AddObject(
    IntToStr(APrimaryKey),
    TFixedRecordCard.Create(ATable, APrimaryKey, AActionType, Field1, Field2, ID1, ID2));
  with (FCardsList.Objects[FCardsList.Count - 1] as TFixedRecordCard) do begin
    OnOkClick := @CardOkClicked;
    OnCardClose := @CardClosed;
    OnRequestRefreshCards := @RefreshValuesInCards;
    if FShouldShow then Show;
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
        OnRequestRefreshCards := @RefreshValuesInCards;
        if FShouldShow then Show;
      end;
    end
    else
      (Objects[IndexOf(IntToStr(APrimaryKey))] as TEditRecordCard).SetFocus;
  end;
end;

procedure TCardsManager.CMUpdateRecord(ATable: TDBTable; APrimaryKey: integer; AActionType: TActionType;
  Field1, Field2: TDBField; ID1, ID2: integer);
begin
  with FCardsList do begin
    if IndexOf(IntToStr(APrimaryKey)) = -1 then begin
      FCardsList.AddObject(IntToStr(APrimaryKey), TFixedEditRecordCard.Create(ATable, APrimaryKey, AActionType, Field1, Field2, ID1, ID2));
      with (Objects[FCardsList.Count - 1] as TEditRecordCard) do begin
        OnOkClick := @CardOkClicked;
        OnCardClose := @CardClosed;
        OnRequestRefreshCards := @RefreshValuesInCards;
        if FShouldShow then Show;
      end;
    end
    else
      (Objects[IndexOf(IntToStr(APrimaryKey))] as TEditRecordCard).SetFocus;
  end;
end;

procedure TCardsManager.CMDeleteRecord(ATable: TDBTable; APrimaryKey: integer);
var
  DeleteResult: integer;
begin
  DeleteResult := DeleteRecord(ATable, APrimaryKey);
  with FCardsList do
    if DeleteResult = 0 then begin
      if IndexOf(IntToStr(APrimaryKey)) <> -1 then
        (Objects[IndexOf(IntToStr(APrimaryKey))] as TEditRecordCard).Close;
      RefreshValuesInCards(ATable);
    end;
  if Assigned(FRefreshTablesRequest) then FRefreshTablesRequest(Self);
end;

procedure TCardsManager.CardOkClicked(Sender: TObject);
begin
  if Assigned(FRefreshTablesRequest) then FRefreshTablesRequest(Sender);
end;

procedure TCardsManager.CardClosed(Sender: TObject);
begin
  FCardsList.Delete(FCardsList.IndexOf(IntToStr((Sender as TRecordCard).PrimaryKey)));
end;

procedure TCardsManager.RefreshValuesInCards(Sender: TObject);
var
  i, j: integer;
begin
  with FCardsList do
    for i := 0 to Count - 1 do
      for j := 0 to High((Objects[i] as TRecordCard).FCellEdits) do
        if (Objects[i] as TRecordCard).FCellEdits[j].FDisplayedField.TableOwner =
          (Sender as TDBTable) then
            if (Objects[i] as TRecordCard).FCellEdits[j].ClassType = TComboCellEdit then
              ((Objects[i] as TRecordCard).FCellEdits[j] as TComboCellEdit).RefreshValues;
end;

initialization

  CardsManager := TCardsManager.Create;

end.
















