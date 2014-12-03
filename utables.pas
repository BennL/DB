unit UTables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, Buttons, ExtCtrls, StdCtrls, MetaUnit, FilterUnit, Query;

type

  { TGridForm }

  TGridForm = class(TForm)
    UpdateFilter: tspeedbutton;
    FilterScroll: tscrollbox;
    Datasource: TDatasource;
    AddFilter: tspeedbutton;
    OrderByBox: TComboBox;
    OrderBy: TSpeedButton;
    SQLQuery: TSQLQuery;
    FilterPanel: TPanel;
    SortPanel: TPanel;
    DBGrid: TDBGrid;
    procedure orderbyboxgetitems(Sender: TObject);
    procedure updatefilterclick(Sender: TObject);
    procedure CreateSortPanel(aowner: TPanel);
    procedure addfilterclick(Sender: TObject);
    procedure orderbyclick(Sender: TObject);
    function createorderquery: string;
  Public
    Table: TTable;
    constructor Create(aowner: tcontrol; uTable: TTable);
  end;

var
  Filter: TFilter;
  uTable: TTable;
  ArrayOfFilters: TArrayOfFilters;
  ArrayOfConditions: TArrayOfCondition;

implementation

{$R *.lfm}

{ TGridForm }

procedure tgridform.createsortpanel(aowner: tpanel);
var
  i: integer;
begin
  OrderByBox := TComboBox.Create(aowner);
  with OrderByBox do
  begin
    Parent := SortPanel;
    ReadOnly := True;
    Top := 10;
    Left := 20;
    Height := 20;
    Width := 135;
    for i := 0 to high(Table.ColumnInfos) do
      Items.Add(Table.ColumnInfos[i].Caption);
  end;

  OrderBy := TSpeedButton.Create(aowner);
  with OrderBy do
  begin
    Parent := SortPanel;
    Top := 10;
    Left := 175;
    Height := 25;
    Width := 50;
    Caption := 'Sort ↓';
    Flat := True;
    OnClick := @orderbyclick;
  end;
end;

procedure tgridform.addfilterclick(Sender: TObject);
begin
  ArrayOfFilters.AddFilter(FilterScroll, Table);
end;

procedure tgridform.orderbyboxgetitems(Sender: TObject);
var
  i: integer;
begin
  with OrderByBox do
  begin
    ReadOnly := True;
    for i := 0 to high(Table.ColumnInfos) do
      Items.Add(Table.ColumnInfos[i].Caption);
  end;
end;

procedure tgridform.orderbyclick(Sender: TObject);
begin
  Table.Sort := not Table.Sort;
  if Table.Sort then
    OrderBy.Caption := 'Sort ↓'
  else
    OrderBy.Caption := 'Sort ↑';
  if OrderByBox.ItemIndex >= 0 then
    if Length(ArrayOfFilters.Filters) <> 0 then
      ShowOrderByResults(SQLQuery, DBGrid, Table, ArrayOfFilters.createfilterquery +
        createorderquery)
    else
      ShowOrderByResults(SQLQuery, DBGrid, Table, createorderquery);
end;

function tgridform.createorderquery: string;
begin
  Result += ' Order by ';
  if Table.ColumnInfos[OrderByBox.ItemIndex].IsReference then
    Result += Table.ColumnInfos[OrderByBox.ItemIndex].ReferenceTable +
      '.' + Table.ColumnInfos[OrderByBox.ItemIndex].ReferenceName
  else
    Result += Table.TName + '.' + Table.ColumnInfos[OrderByBox.ItemIndex].Name;
  if Table.Sort then
    Result += ' desc ';
end;

procedure tgridform.updatefilterclick(Sender: TObject);
var
  i: integer;
  flag: boolean;
  Parameters: array of string;
begin
  with ArrayOfFilters do
  begin
    if (length(Filters) <> 0) then
    begin
      for i := 0 to high(Filters) do
      begin
        flag := True;
        if ((Filters[i].FilterColumn.Caption = '') or
          (Filters[i].FilterCondition.Caption = '') or
          (Filters[i].FilterEdit.Caption = '')) then
        begin
          ShowMessage('Заполните все поля');
          flag := False;
          break;
        end;
      end;

      for i := 0 to high(ArrayOfFilters.Filters) do
      begin
        SetLength(Parameters, Length(Parameters) + 1);
        Parameters[i] := Format(
          ArrayOfConditions.Conditions[Filters[i].FilterCondition.ItemIndex].ParamF,
          [Filters[i].FilterEdit.Caption]);
      end;

      if (flag) then
        ShowFilterResults(SQLQuery, DBGrid, Table, createfilterquery, Parameters);
    end
    else
    begin
      ShowItems(SQLQuery, DBGrid, Table);
      ShowMessage('Добавьте фильтры');
    end;
  end;
end;

constructor tgridform.Create(aowner: tcontrol; utable: ttable);
begin
  inherited Create(aOwner);
  ArrayOfFilters := TArrayOfFilters.Create;
  ArrayOfConditions := TArrayOfCondition.Create;
  Table := uTable;
  createsortpanel(SortPanel);
end;


end.
