unit UScheduleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Buttons, ExtCtrls, metadata, sqldb, DB, querycreate,
  DBComponentCreate, UFilterForm, ufilter, ueditform, UCheckGroupForm;

type
  TCurrColRow = record
    aCol: integer;
    aRow: integer;
  end;

  TColumnRowName = record
    ID: integer;
    Caption: string;
  end;

  TItems = record
    ID: integer;
    Ids: array of integer;
    Item: array of string;
  end;

  TColumnRowNames = array of TColumnRowName;
  TScheduleData = array of array of array of TItems;

  { TScheduleForm }

  TScheduleForm = class(TForm)
    ExportBitBtn: TBitBtn;
    CheckColBitBtn: TBitBtn;
    ResetBtn: TBitBtn;
    InterfaceImage: TImageList;
    LabelY: TLabel;
    LabelX: TLabel;
    AddFilters: TBitBtn;
    ResetFilters: TBitBtn;
    SQLQuery: TSQLQuery;
    RefreshBtn: TBitBtn;
    DrawGrid: TDrawGrid;
    XComboBox: TComboBox;
    YComboBox: TComboBox;
    DataSource: TDataSource;
    procedure AddFiltersClick(Sender: TObject);
    procedure CheckGroupItemClick(Sender: TObject; Index: integer);
    procedure DrawGridDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure DrawGridDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure DrawGridEndDrag(Sender, Target: TObject; X, Y: integer);
    procedure DrawGridStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ExportBitBtnClick(Sender: TObject);
    procedure OnCheckGroupFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CheckColBitBtnClick(Sender: TObject);
    procedure DrawGridClick(Sender: TObject);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormCreate(Sender: TObject);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ResetBtnClick(Sender: TObject);
    procedure ResetFiltersClick(Sender: TObject);
    procedure EditFormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    CurrentHeight: integer;
    XTitles: array of TColumnRowName;
    YTitles: array of TColumnRowName;
    FCols: array of TColumnInfo;
    GridEditBtn: TRect;
    GridInsertBtn: TRect;
    Items: TScheduleData;
    CurrentPoint: TPoint;
    CurCol: integer;
    CurRow: integer;
    CurRecord: integer;
    CurrientPos: integer;
    TempCol: integer;
    TempRow: integer;
    procedure RefreshInfo;
    procedure FillRowTitles;
    procedure FillColumnTitles;
    procedure FillComboBoxes;
    procedure SetColsRows;
    procedure FillItems;
    procedure ButtonClick(aRect: TRect; IsInsert: boolean);
    procedure OKBtnClick(Sender: TObject);
    procedure OnFilterFormClose(Sender: TObject; var CloseAction: TCloseAction);
    function CreateScheduleQuery: string;
    function InsertQuery(cCol, cRow: integer): string;
    function UpdateQuery(cCol, cRow: integer): string;
    function DeleteQuery: string;
    function GetLookUpResult(aTable: TTableInfo): TColumnRowNames;
  public
    VisibleColumn: array of boolean;
  end;

  { TDragDimension }

  TDragDimension = class(TDragControlObject)
  private
    FDragCanvas: TCanvas;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  end;

var
  ScheduleForm: TScheduleForm;
  ScheduleTable: TTableInfo;
  ListOfEditForm: TListOfEditForm;
  FDragObject: TDragDimension;

implementation

uses
  UExport, main;

{$R *.lfm}

{ TTDragDimension }

constructor TDragDimension.Create(AControl: TControl);
begin
  inherited Create(AControl);
  FDragCanvas := TCanvas.Create;
end;

destructor TDragDimension.Destroy;
begin
  FDragCanvas.Free;
  inherited Destroy;
end;

{ TScheduleForm }

procedure TScheduleForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(ScheduleTable.ColumnInfos) do
    if (ScheduleTable.ColumnInfos[i].VisibleColumn) then
    begin
      SetLength(FCols, length(FCols) + 1);
      FCols[high(FCols)] := ScheduleTable.ColumnInfos[i];
    end;

  FillComboBoxes;
  for i := 0 to high(FCols) do
  begin
    SetLength(VisibleColumn, length(VisibleColumn) + 1);
    VisibleColumn[high(VisibleColumn)] := True;
  end;

  XComboBox.ItemIndex := 4;
  YComboBox.ItemIndex := 5;
  RefreshInfo;
end;

procedure TScheduleForm.AddFiltersClick(Sender: TObject);
var
  NewForm: TFilterForm;
begin
  TBitBtn(Sender).Enabled := False;
  NewForm := TFilterForm.Create(Self);
  with NewForm do
  begin
    FilterTable := ScheduleTable;
    OKBitBtn.OnClick := @OKBtnClick;
    OnClose := @OnFilterFormClose;
    Show;
  end;
end;

procedure TScheduleForm.CheckGroupItemClick(Sender: TObject; Index: integer);
var
  i: integer;
  Count: integer;
begin
  Count := 0;

  VisibleColumn[Index] := TCheckGroup(Sender).Checked[Index];

  for i := 0 to high(VisibleColumn) do
  begin
    if (VisibleColumn[i]) then
      Inc(Count);
  end;

  CurrentHeight := Count * 20;

  DrawGrid.Invalidate;

  with DrawGrid do
    for i := 1 to RowCount - 1 do
      RowHeights[i] := CurrentHeight;
end;

procedure TScheduleForm.DrawGridDragDrop(Sender, Source: TObject; X, Y: integer);
begin

end;

procedure TScheduleForm.DrawGridDragOver(Sender, Source: TObject;
  X, Y: integer; State: TDragState; var Accept: boolean);
begin

end;

procedure TScheduleForm.DrawGridEndDrag(Sender, Target: TObject; X, Y: integer);
var
  aCol, aRow: integer;
  Query: string;
begin
  DrawGrid.MouseToCell(X, Y, aCol, aRow);

  if Length(Items[aRow - 1][aCol - 1]) = 0 then
  begin
    Query := InsertQuery(aCol, aRow);
  end
  else
  begin
    Query := UpdateQuery(aCol, aRow);
  end;

  SetScheduleQuery(SQLQuery, Query);
  if Length(Items[aRow - 1][aCol - 1]) = 0 then
    SetScheduleQuery(SQLQuery, DeleteQuery);
  MainForm.SQLTransaction.Commit;
  RefreshInfo;

  //ShowMessage(IntToStr(xID));
end;

procedure TScheduleForm.DrawGridStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  DragObject := FDragObject;

  TempCol := CurCol - 1;
  TempRow := CurRow - 1;
end;

procedure TScheduleForm.ExportBitBtnClick(Sender: TObject);
begin
  SchExport.ExportToFile(Items, XTitles, YTitles, FCols[XComboBox.ItemIndex],
    FCols[YComboBox.ItemIndex], ListOfFilters, FCols);
end;

procedure TScheduleForm.CheckColBitBtnClick(Sender: TObject);
var
  NewForm: TCheckGroupForm;
begin
  TBitBtn(Sender).Enabled := False;
  NewForm := TCheckGroupForm.Create(Self, VisibleColumn, FCols);
  with NewForm do
  begin
    CheckGroup.OnItemClick := @CheckGroupItemClick;
    OnClose := @OnCheckGroupFormClose;
    Show;
  end;
end;

procedure TScheduleForm.DrawGridClick(Sender: TObject);
begin
  ButtonClick(GridEditBtn, False);
  ButtonClick(GridInsertBtn, True);
  DrawGrid.Invalidate;
end;

procedure TScheduleForm.DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  TCol, TRow: integer;
begin
  CurrentPoint.X := X;
  CurrentPoint.Y := Y;
  CurrientPos := (Y - DrawGrid.CellRect(CurCol, CurRow).Top) div CurrentHeight;

  with DrawGrid do
  begin
    if ssCtrl in Shift then
    begin
      MouseToCell(X, Y, TCol, TRow);
      Tag := 1;
      if (TCol > 0) or (TRow > 0) then
        BeginDrag(True)
      else
        Tag := 0;
    end;
  end;
end;

procedure TScheduleForm.DrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  with DrawGrid do
  begin
    MouseToCell(X, Y, CurCol, CurRow);
    CurRecord := (Y - CellRect(CurCol, CurRow).Top) div CurrentHeight;
    Invalidate;
  end;
end;

procedure TScheduleForm.DrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
const
  ElemHeight = 20;
  LeftSpace = 5;
var
  i, j: integer;
  CurrHeight: integer;
begin
  CurrHeight := 0;
  if (aRow = 0) and (aCol = 0) then
    Exit;

  with DrawGrid.Canvas do
  begin
    Pen.Color := clBlack;
    if aCol = 0 then
      TextOut(aRect.Left, aRect.Top, YTitles[aRow - 1].Caption)
    else
    if aRow = 0 then
      TextOut(aRect.Left, aRect.Top, XTitles[aCol - 1].Caption)
    else
    if (((aRow - 1) <= high(Items)) and ((aCol - 1) <= high(Items[aRow - 1]))) then
      for i := 0 to high(Items[aRow - 1][aCol - 1]) do
      begin

        DrawGrid.Canvas.Brush.Color := clWhite;
        DrawGrid.Canvas.Brush.Style := bsClear;

        for j := 0 to high(Items[aRow - 1][aCol - 1][i].Item) do
          if (VisibleColumn[j]) then
          begin
            TextOut(aRect.Left + LeftSpace, aRect.Top + CurrHeight,
              FCols[j].Caption + ': ' + Items[aRow - 1][aCol - 1][i].Item[j]);
            CurrHeight += ElemHeight;
          end;

        DrawGrid.Canvas.Brush.Color := clBlack;
        DrawGrid.Canvas.Brush.Style := bsSolid;

        if (aRow = CurRow) and (aCol = CurCol) and (i = CurRecord) then
          with aRect do
          begin
            GridEditBtn := Rect(Right - 10, Top + CurrHeight - 160,
              Right, Top + CurrHeight - 120);
            InterfaceImage.Draw(DrawGrid.Canvas, Right - 16, Top +
              CurrHeight - 160, 2, True);
          end;

        Line(aRect.Left, aRect.Top + CurrHeight, aRect.Right,
          aRect.Top + CurrHeight);
        CurrHeight += 5;
      end;

    if (CurRow = aRow) and (CurCol = aCol) and (aCol > 0) and (aRow > 0) then
      with aRect do
      begin
        GridInsertBtn := Rect(Left, Bottom - 16, Left + 16, Bottom);
        InterfaceImage.Draw(DrawGrid.Canvas, Left, Bottom - 16, 1, True);
      end;

    Brush.Color := clBlack;
    Brush.Style := bsSolid;

    with aRect do
      if ((aRow > 0) and (aCol > 0)) and (DrawGrid.RowHeights[aRow] <
        ((CurrentHeight + 5) * length(Items[aRow - 1][aCol - 1]) - 5)) then
        InterfaceImage.Draw(DrawGrid.Canvas, Right - 16, Bottom - 16, 0, True);
  end;
end;

procedure TScheduleForm.ResetBtnClick(Sender: TObject);
var
  buttonSelected: integer;
begin
  buttonSelected := 0;

  if XComboBox.ItemIndex = YComboBox.ItemIndex then
    buttonSelected := MessageDlg('Выбраны недопустимые значения X и Y', mtError, [mbOK], 0);
  if buttonSelected = 0 then
    RefreshInfo;
end;

procedure TScheduleForm.ResetFiltersClick(Sender: TObject);
begin
  ListOfFilters.Clear();
  RefreshInfo;
end;

procedure TScheduleForm.EditFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(ListOfEditForm);
  RefreshInfo;
end;

procedure TScheduleForm.DrawGridDblClick(Sender: TObject);
var
  CurrentSize: integer;
begin
  CurrentSize := (CurrentHeight + 5) * length(Items[DrawGrid.Row - 1][DrawGrid.Col - 1]);
  if CurrentSize = 0 then
    exit;

  with DrawGrid do
  begin
    if (RowHeights[Row] = CurrentSize) then
      RowHeights[Row] := CurrentHeight
    else
      RowHeights[Row] := CurrentSize;
  end;
end;

procedure TScheduleForm.RefreshInfo;
begin
  FillRowTitles;
  FillColumnTitles;
  FillItems;
  SetColsRows;
  DrawGrid.Invalidate;
end;

procedure TScheduleForm.FillRowTitles;
var
  aTable: TTableInfo;
begin
  aTable := GetTableByName(FCols[YComboBox.ItemIndex].ReferenceTable);
  YTitles := GetLookUpResult(aTable);
end;

procedure TScheduleForm.FillColumnTitles;
var
  aTable: TTableInfo;
begin
  aTable := GetTableByName(FCols[XComboBox.ItemIndex].ReferenceTable);
  XTitles := GetLookUpResult(aTable);
end;

procedure TScheduleForm.FillComboBoxes;
var
  i: integer;
begin
  XComboBox.Items.Clear;
  YComboBox.Items.Clear;
  for i := 0 to high(FCols) do
  begin
    XComboBox.Items.Add(FCols[i].Caption);
    YComboBox.Items.Add(FCols[i].Caption);
  end;
end;

procedure TScheduleForm.SetColsRows;
var
  i: integer;
begin
  with DrawGrid do
  begin
    DefaultColWidth := 300;
    DefaultRowHeight := 160;
    RowCount := length(YTitles) + 1;
    ColCount := length(XTitles) + 1;
    for i := 0 to RowCount - 1 do
      RowHeights[i] := DefaultRowHeight;
    ColWidths[0] := 80;
    RowHeights[0] := 30;
  end;
  CurrentHeight := 160;
end;

procedure TScheduleForm.FillItems;
var
  i, j, k: integer;
  YFieldID, XFieldID, ColumnCount: integer;
  XIDCol, YIDCol: TColumnInfo;
  aQuery: string;
  Param: array of string;
begin
  Items := nil;

  ColumnCount := length(FCols);

  XIDCol := FCols[XComboBox.ItemIndex];
  YIDCol := FCols[YComboBox.ItemIndex];

  aQuery += CreateScheduleQuery;

  with ListOfFilters do
  begin
    if (length(Filters) <> 0) and (CheckPopulateFields) {and (not IsReset)} then
    begin
      for i := 0 to high(Filters) do
      begin
        with Filters[i] do
        begin
          SetLength(Param, length(Param) + 1);
          Param[i] := Format(FCondition.Conditions[
            FComboBoxCondition.ItemIndex].ParamFormat, [FEdit.Caption]);
        end;
      end;
      aQuery += ' ' + CreateFQuery;
    end
    else
      SetLength(Param, 0);
  end;

  aQuery += ' order by ' + FCols[YComboBox.ItemIndex].ReferenceTable +
    '.ID' + ', ' + FCols[XComboBox.ItemIndex].ReferenceTable + '.ID' + ', Times.ID';

  SetParamQuery(SQLQuery, aQuery, Param);

  XFieldID := SQLQuery.FieldByName(XIDCol.AliasName).Index;
  YFieldID := SQLQuery.FieldByName(YIDCol.AliasName).Index;

  SetLength(Items, length(YTitles));
  for i := 0 to length(YTitles) - 1 do
    SetLength(Items[i], length(XTitles));

  i := 0;
  j := 0;

  with SQLQuery do
    while (not EOF) do
    begin
      while (Fields[YFieldID].AsInteger > YTitles[i].ID) do
      begin
        Inc(i);
        j := 0;
      end;

      while (Fields[XFieldID].AsInteger > XTitles[j].ID) do
        Inc(j);

      SetLength(Items[i][j], length(Items[i][j]) + 1);
      SetLength(Items[i][j][high(Items[i][j])].Item, ColumnCount);
      SetLength(Items[i][j][high(ITems[i][j])].Ids, ColumnCount);


      for k := 0 to ColumnCount - 1 do
      begin
        Items[i][j][high(Items[i][j])].Item[k] :=
          FieldByName(GetTableByName(FCols[k].ReferenceTable).ColumnInfos
          [1].AliasName).AsString;
        Items[i][j][high(Items[i][j])].Ids[k] :=
          FieldByName(ScheduleTable.Name + FCols[k].Name).AsInteger;
        //ShowMessage (IntToStr(Items[i][j][high(Items[i][j])].Ids[k]));
        //ShowMessage(Items[i][j][high(Items[i][j])].Item[k]);
      end;

      Items[i][j][high(Items[i][j])].ID := FieldByName('Schedule_itemsID').AsInteger;
      Next;
    end;
end;


procedure TScheduleForm.ButtonClick(aRect: TRect; IsInsert: boolean);
var
  NewForm: TEditForm;
  CurrCB, CurrCol: TPoint;
begin
  CurrCB.X := XComboBox.ItemIndex;
  CurrCB.Y := YComboBox.ItemIndex;
  CurrCol.X := DrawGrid.Col - 1;
  CurrCol.Y := DrawGrid.Row - 1;

  with aRect do
  begin
    with CurrentPoint do
    begin
      if (Left < X) and (Right > X) and (Top < Y) and (Bottom > Y) then
      begin
        if (IsInsert) then
          NewForm := TEditForm.Create(ScheduleForm, ScheduleTable, 0, CurrCB, CurrCol)
        else
          NewForm := TEditForm.Create(ScheduleForm, ScheduleTable,
            Items[CurRow - 1][CurCol - 1][CurRecord].ID, CurrCB, CurrCol);
        NewForm.OnClose := @EditFormClose;
        NewForm.Show;
      end;
    end;
  end;
end;

procedure TScheduleForm.OKBtnClick(Sender: TObject);
begin
  RefreshInfo;
end;

procedure TScheduleForm.OnCheckGroupFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CheckColBitBtn.Enabled := True;
end;

procedure TScheduleForm.OnFilterFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  AddFilters.Enabled := True;
end;

function TScheduleForm.CreateScheduleQuery: string;
var
  Table: TTableInfo;
  i, j: integer;
begin
  Result := '';

  Result += 'Select';

  with ScheduleTable do
  begin
    for i := 0 to high(ColumnInfos) do
      Result += Format(' %s.%s as %s,', [Name, ColumnInfos[i].Name,
        ColumnInfos[i].AliasName]);
    for i := 0 to high(ColumnInfos) do
    begin
      if (ColumnInfos[i].Reference) then
      begin
        Table := GetTableByName(ColumnInfos[i].ReferenceTable);
        with Table do
        begin
          for j := 0 to high(ColumnInfos) do
          begin
            if (ColumnInfos[j].VisibleColumn) then
            begin
              if (i <> high(ScheduleTable.ColumnInfos)) then
                Result += Format(' %s.%s as %s,',
                  [Name, ColumnInfos[j].Name, ColumnInfos[j].AliasName])
              else
                Result += Format(' %s.%s as %s',
                  [Name, ColumnInfos[j].Name, ColumnInfos[j].AliasName]);
            end;
          end;
        end;
      end;
    end;

    Result += Format(' from %s ', [ScheduleTable.Name]);

    for i := 0 to high(ColumnInfos) do
    begin
      with ColumnInfos[i] do
        if VisibleColumn then
          Result += Format('inner join %s on %s.%s = %s.ID ',
            [ReferenceTable, ScheduleTable.Name, Name, ReferenceTable]);
    end;
  end;
end;

function TScheduleForm.InsertQuery(cCol, cRow: integer): string;
var
  i: integer;
  xID, yID: integer;
begin
  xID := XComboBox.ItemIndex;
  yID := YComboBox.ItemIndex;

  with ScheduleTable do
  begin
    Result := Format('Insert into %s values (next value for %s, ',
      [Name, GenerateName]);

    with Items[TempRow][TempCol][CurrientPos] do
    begin
      for i := 0 to high(Ids) do
      begin
        if (i <> xID) and (i <> yID) then
          Result += IntToStr(Ids[i])
        else
        if i = xID then
          Result += IntToStr(XTitles[cCol - 1].ID)
        else
        if i = yID then
          Result += IntToStr(YTitles[cRow - 1].ID);

        if (i <> high(Ids)) then
          Result += ', '
        else
          Result += ')';
      end;
    end;
  end;
end;

function TScheduleForm.UpdateQuery(cCol, cRow: integer): string;
var
  i: integer;
  xID, yID: integer;
begin
  xID := XComboBox.ItemIndex;
  yID := YComboBox.ItemIndex;

  with ScheduleTable do
  begin
    with Items[TempRow][TempCol][CurrientPos] do
    begin
      Result := Format('Update %s set ', [Name]);
      for i := 0 to high(Ids) do
      begin
        if (i <> xID) and (i <> yID) then
          Result += Format(' %s = %d', [ColumnInfos[i + 1].Name, Ids[i]])
        else
        if i = xID then
          Result += Format(' %s = %d', [ColumnInfos[i + 1].Name,
            XTitles[cCol - 1].ID])
        else
        if i = yID then
          Result += Format(' %s = %d', [ColumnInfos[i + 1].Name,
            YTitles[cRow - 1].ID]);

        if (i <> high(Ids)) then
          Result += ', '
        else
          Result += ' ';
      end;
      Result += Format(' where %s.ID = %d', [Name, ID]);



    end;
  end;
end;

function TScheduleForm.DeleteQuery: string;
begin
  Result := Format('Delete from %s where %s.ID = %d',
    [ScheduleTable.Name, ScheduleTable.Name, Items[TempRow][TempCol][CurrientPos].ID]);
end;

function TScheduleForm.GetLookUpResult(aTable: TTableInfo): TColumnRowNames;
var
  LookUpQuery: TSQLQuery;
  DataSrc: TDataSource;
begin
  LookUpQuery := SQLQueryCreate;
  DataSrc := TDataSource.Create(nil);

  SetQuery(LookUpQuery, CreateQuery(aTable));

  while (not LookUpQuery.EOF) do
  begin
    SetLength(Result, length(Result) + 1);
    Result[high(Result)].ID := LookUpQuery.Fields[0].AsInteger;
    Result[high(Result)].Caption := LookUpQuery.Fields[1].AsString;
    LookUpQuery.Next;
  end;

  FreeAndNil(LookUpQuery);
  FreeAndNil(DataSrc);
end;

initialization

  ScheduleTable := GetTableByName('Schedule_items');
  ListOfEditForm := TListOfEditForm.Create;

end.
