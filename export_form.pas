unit export_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, sf_export, CheckLst, metadata, query_filter, Buttons, Spin,
  fpspreadsheet;

type

  { TExportForm }

  TExportForm = class(TForm)
  private
    FColsCount: integer;
    FRowsCount: integer;
    FCheckList: TCheckListBox;
    FFilters: TQueryFilterDynArray;
    FHorzField: TDBField;
    FVertField: TDBField;
    btnApply: TBitbtn;
    FCheckedCount: ^integer;
    FIsColEmpty: TBoolDynArray;
    FIsRowEmpty: TBoolDynArray;
    FStrings: TDblStrinListDynArray;
  public
    procedure HTMLFillStringList(StringList: TStringList);
    procedure HTMLAppendDescription(StringList: TStringList);
    procedure HTMLAppendCells(StringList: TStringList);
    procedure XLFillWorkbook(Workbook: TsWorkbook);
    procedure XLAddDescription(DescrSheet: TsWorksheet);
    procedure RefreshData(AStrings: TDblStrinListDynArray; AColsCount, ARowsCount: integer;
      AHorzField, AVertField: TDBField; AFilters: TQueryFilterDynArray;
      IsColEmpty, IsRowEmpty: TBoolDynArray);
    constructor Create(AParent: TForm; ACheckList: TCheckListBox; AbtnApply: TBitbtn;
      var ACheckedCount: integer);
  published
    lbFontSize: TLabel;
    seFontSize: TSpinEdit;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnBrowse: TButton;
    cgParams: TCheckGroup;
    lbePath: TLabeledEdit;
    rgFormat: TRadioGroup;
    SaveDialog: TSaveDialog;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  end;

var
  ExportForm: TExportForm;

implementation

{$R *.lfm}

procedure TExportForm.btnBrowseClick(Sender: TObject);
begin
  SaveDialog.FilterIndex := rgFormat.ItemIndex + 1;
  if SaveDialog.Execute then begin
    lbePath.Text := SaveDialog.FileName;
    rgFormat.ItemIndex := SaveDialog.FilterIndex - 1;
  end;
end;

procedure TExportForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TExportForm.btnOkClick(Sender: TObject);
var
  StringList: TStringList;
  Stream: TFileStream;
  Workbook: TsWorkbook;
begin
  Stream := TFileStream.Create(Utf8ToAnsi(lbePath.Text), fmCreate);
  case rgFormat.ItemIndex of
    0:
      begin
        StringList := TStringList.Create;
        HTMLFillStringList(StringList);
        StringList.SaveToStream(Stream);
        StringList.Free;
      end;
    1:
      begin
        Workbook := TsWorkbook.Create;
        XLFillWorkbook(Workbook);
        Workbook.WriteToStream(Stream, sfExcel8);
        Workbook.Free;
      end;
  end;
  Stream.Free;
  Close;
end;

procedure TExportForm.XLFillWorkbook(Workbook: TsWorkbook);
var
  Worksheet: TsWorksheet;
  Description: TsWorksheet;
  i, j: integer;
begin
  Worksheet := Workbook.AddWorksheet(FVertField.Caption + ' - ' + FHorzField.Caption);
  Description := Workbook.AddWorksheet('Описание');
  with Worksheet do begin
    DefaultColWidth := 10;
    DefaultRowHeight := 10;
    Options := Worksheet.Options + [soHasFrozenPanes];
    LeftPaneWidth := 1;
    TopPaneHeight := 1;

    for i := 0 to FRowsCount - 1 do begin
      for j := 0 to FColsCount - 1 do begin
        if Assigned(FStrings[i, j]) then
          WriteUTF8Text(i, j, FStrings[i, j].Text);
        WriteVertAlignment(i, j, vaTop);
        WriteColWidth(j, 25);
        WriteWordwrap(i, j, true);
        Worksheet.WriteFontSize(i, j, seFontSize.Value);
      end;
      WriteRowHeight(i, 25);
    end;

    for i := 0 to FRowsCount do begin
      WriteBorders(i, 0, [cbEast]);
      WriteColWidth(0, 15);
      WriteVertAlignment(i, 0, vaCenter);
      WriteHorAlignment(i, 0, haCenter);
      if cgParams.Checked[4] and FIsRowEmpty[i] then
        WriteRowHeight(i, 0);
    end;

    for j := 0 to FColsCount do begin
      WriteBorders(0, j, [cbSouth]);
      WriteRowHeight(0, 2);
      WriteVertAlignment(0, j, vaCenter);
      WriteHorAlignment(0, j, haCenter);
      if cgParams.Checked[3] and FIsColEmpty[j] then
        WriteColWidth(j, 0);
    end;
  end;

  XLAddDescription(Description);
end;

procedure TExportForm.XLAddDescription(DescrSheet: TsWorksheet);
  procedure StartDescrSection(DescrSheet: TsWorksheet; Shift: integer; Caption: string);
  begin
    with DescrSheet do begin
      MergeCells(Shift, 0, Shift, 5);
      WriteUTF8Text(Shift, 0, Caption);
      WriteBorders(Shift, 0, [cbSouth, cbWest, cbEast, cbNorth]);
      WriteBorders(Shift, 5, [cbEast, cbNorth]);
      WriteRowHeight(Shift, 2);
      WriteVertAlignment(Shift, 0, vaCenter);
    end;
  end;
  procedure AddDescr(DescrSheet: TsWorksheet; Shift, Count: integer; StringList: TStringList);
  begin
    with DescrSheet do begin
      MergeCells(Shift + 1, 0, Shift + 1, 3);
      WriteUTF8Text(Shift + 1, 0, StringList.Text);
      WriteRowHeight(Shift + 1, 1 + Count);
      WriteVertAlignment(Shift + 1, 0, vaTop);
    end;
  end;
var
  Shift: integer = 0;
  i: integer;
  StringList: TStringList;
  FieldCaption: string;
begin
  StringList := TStringList.Create;
  with DescrSheet do begin
    if btnApply.Enabled then begin
      StartDescrSection(DescrSheet, Shift, 'Описание может не соответствовать содержимому таблицы');
      inc(Shift, 2);
    end;

    if (FCheckedCount^ > 0) and (cgParams.Checked[0]) then begin
      StringList.Clear;
      StartDescrSection(DescrSheet, Shift, 'Отображаемые поля:');
      for i := 0 to FCheckList.Count - 1 do
        if FCheckList.Checked[i] then
          StringList.Append(FCheckList.Items.Strings[i]);
      AddDescr(DescrSheet, Shift, FCheckedCount^, StringList);
      inc(Shift, 3);
    end;

    if (Length(FFilters) > 0) and (cgParams.Checked[1]) then begin
      StringList.Clear;
      StartDescrSection(DescrSheet, Shift, 'Фильтры:');
      for i := 0 to High(FFilters) do begin
        FieldCaption := '';
        if Assigned(FFilters[i].ChosenField) then
          FieldCaption := FFilters[i].ChosenField.Caption
        else if Assigned(FFilters[i].ConstantEditor) then
          FieldCaption := 'ИЛИ'
        else continue;
        StringList.Append(
                          FieldCaption + ' ' +
                          FFilters[i].Operation.Caption + ' ' +
                          string(FFilters[i].Value));
      end;
      AddDescr(DescrSheet, Shift, Length(FFilters), StringList);
      inc(Shift, 3);
    end;

    if cgParams.Checked[2] then
      StartDescrSection(DescrSheet, Shift, FVertField.Caption + ' \ ' + FHorzField.Caption);
  end;
end;

procedure TExportForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

constructor TExportForm.Create(AParent: TForm; ACheckList: TCheckListBox; AbtnApply: TBitbtn;
      var ACheckedCount: integer);
var
  i: integer;
begin
  inherited Create(AParent);
  FCheckList := ACheckList;
  btnApply := AbtnApply;
  FCheckedCount := @ACheckedCount;
  with cgParams do
    for i := 0 to Items.Count - 1 do
      Checked[i] := true;
end;

procedure TExportForm.RefreshData(AStrings: TDblStrinListDynArray; AColsCount, ARowsCount: integer;
      AHorzField, AVertField: TDBField; AFilters: TQueryFilterDynArray;
      IsColEmpty, IsRowEmpty: TBoolDynArray);
begin
  FColsCount := AColsCount;
  FRowsCount := ARowsCount;
  FFilters := AFilters;
  FHorzField := AHorzField;
  FVertField := AVertField;
  FIsColEmpty := IsColEmpty;
  FIsRowEmpty := IsRowEmpty;
  FStrings := AStrings;
end;

procedure TExportForm.HTMLAppendDescription(StringList: TStringList);
var
  i: integer;
  FieldCaption: string;
begin
  with StringList do begin
    if btnApply.Enabled then
      Append('Описание может не соответствовать содержимому таблицы' + '<br><br>');

    if (FCheckedCount^ > 0) and (cgParams.Checked[0]) then begin
      Append('<li>' + 'Отображаемые поля:' + '</li>');
      for i := 0 to FCheckList.Count - 1 do
        if FCheckList.Checked[i] then
          Append(FCheckList.Items.Strings[i] + ' <br>');
    end;

    if (Length(FFilters) > 0) and (cgParams.Checked[1]) then begin
      Append('<br><li>' + 'Фильтры:' + ' </li>');
      for i := 0 to High(FFilters) do begin
        FieldCaption := '';
        if Assigned(FFilters[i].ChosenField) then
          FieldCaption := FFilters[i].ChosenField.Caption
        else if Assigned(FFilters[i].ConstantEditor) then
          FieldCaption := 'ИЛИ'
        else continue;
        Append(
               FieldCaption + ' ' +
               FFilters[i].Operation.Caption + ' ' +
               string(FFilters[i].Value) + '<br>');
      end;
    end;

    if cgParams.Checked[2] then
      Append(
             '<br><br>' +
             FVertField.Caption + ' \ ' +
             FHorzField.Caption);
  end;
end;

procedure TExportForm.HTMLAppendCells(StringList: TStringList);
var
  i, j, k: integer;
begin
  with StringList do begin
    Append('<table>');
    for i := 0 to FRowsCount - 1 do begin
      if cgParams.Checked[4] and FIsRowEmpty[i] then continue;
      Append('<tr>');
      for j := 0 to FColsCount - 1 do begin
        if cgParams.Checked[3] and FIsColEmpty[j] then continue;
        Append('<td>');
        if (i < Length(FStrings)) and (j < Length(FStrings[i])) and Assigned(FStrings[i, j]) then
          for k := 0 to FStrings[i, j].Count - 1 do
            Append(FStrings[i, j].Strings[k] + ' <br>');
        Append('</td>');
      end;
      Append('</tr>');
    end;
    Append('</table>');
  end;
end;

procedure TExportForm.HTMLFillStringList(StringList: TStringList);
begin
  with StringList do begin
    Append('<html> <head> <meta charset="utf-8">');
    Append('<style>');
    Append('table { border: 1px solid #000; border-collapse:collapse;}');
    Append('td { border: 1px solid #000; padding: 5px; vertical-align: top; font-size: ' +
           seFontSize.Text + 'px}');
    Append('</style>');
    Append('</head>');
    Append('<body>');
    HTMLAppendDescription(StringList);
    HTMLAppendCells(StringList);
    Append('</body> </html>');
  end;
end;

end.

