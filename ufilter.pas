unit ufilter;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, sqldb, DB, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, Buttons, StdCtrls, metadata;

type

  { TCondition }

  TCondition = record
    Caption: string;
    QueryFormat: string;
    ParamFormat: string;
  end;

  { TListOfCondition }

  TListOfCondition = class
    Conditions: array of TCondition;
    constructor Create();
    procedure AddCondition(aCaption, aQueryFormat, aParamFormat: string);
  end;

  { TFilter }

  TFilter = class
    FPanel: TPanel;
    FComboBoxColumn: TComboBox;
    FComboBoxCondition: TComboBox;
    FEdit: TEdit;
    FDestroyCheckBox: TCheckBox;
    FCondition: TListOfCondition;
    Column: string;
    Text: string;
    Query: string;
    Tag: integer;
    FTable: TTableInfo;
    FieldType: TFieldType;
    constructor Create(aOwner: TWinControl; aTable: TTableInfo);
    destructor Destroy; override;
    procedure FilterInterfae(aOwner: TWinControl);
    function GetQuery(aIndex: integer): string;
    function GetCondition: string;
  end;

  { TListOfFilters }

  TListOfFilters = class
    Filters: array of TFilter;
    destructor Destroy; override;
    procedure AddFilter(aOwner: TWinControl; aTable: TTableInfo);
    procedure DeleteFilter(aIndex: integer);
    procedure Clear();
    function CreateFQuery: string;
    function Count(): integer;
    function CheckPopulateFields: boolean;
  end;

var
  ListOfFilters: TListOfFilters;

implementation

var
  ListOfCondition: TListOfCondition;

{ TConditions }

constructor TListOfCondition.Create;
begin
  AddCondition('>', ' %s.%s > :p%s', '%s');
  AddCondition('<', ' %s.%s < :p%s', '%s');
  AddCondition('>=', ' %s.%s >= :p%s', '%s');
  AddCondition('<=', ' %s.%s <= :p%s', '%s');
  AddCondition('=', ' %s.%s = :p%s', '%s');
  AddCondition('начинается на', ' %s.%s like :p%s', '%s%%');
  AddCondition('заканчивается на', ' %s.%s like :p%s', '%%%s');
  AddCondition('содержит', ' %s.%s like :p%s', '%%%s%%');
end;

procedure TListOfCondition.AddCondition(aCaption, aQueryFormat, aParamFormat: string);
begin
  SetLength(Conditions, length(Conditions) + 1);
  Conditions[high(Conditions)].Caption := aCaption;
  Conditions[high(Conditions)].QueryFormat := aQueryFormat;
  Conditions[high(Conditions)].ParamFormat := aParamFormat;
end;

{ TListOfFilters }

destructor TListOfFilters.Destroy;
var
  i: integer;
begin
  Clear();
  inherited Destroy;
end;

procedure TListOfFilters.AddFilter(aOwner: TWinControl; aTable: TTableInfo);
begin
  SetLength(Filters, length(Filters) + 1);
  Filters[high(Filters)] := TFilter.Create(aOwner, aTable);
  Filters[high(Filters)].Tag := high(Filters);
end;

procedure TListOfFilters.DeleteFilter(aIndex: integer);
var
  i: integer;
begin
  FreeAndNil(Filters[aIndex]);

  for i := aIndex to high(Filters) - 1 do
  begin
    Filters[i] := Filters[i + 1];
    Filters[i].Tag := i;
  end;

  SetLength(Filters, length(Filters) - 1);
end;

procedure TListOfFilters.Clear;
var
  i: integer;
begin
  if Count() <> 0 then
  begin
    for i := high(Filters) downto 0 do
    begin
      FreeAndNil(Filters[i]);
      SetLength(Filters, length(Filters) - 1);
    end;
  end;
end;

function TListOfFilters.CreateFQuery: string;
var
  i: integer;
begin
  Result += ' Where ';
  for i := 0 to high(Filters) do
  begin
    Result += Filters[i].GetQuery(i);
    if (i <> high(Filters)) then
      Result += ' and ';
  end;
end;

function TListOfFilters.Count: integer;
begin
  Result := Length(Filters);
end;

function TListOfFilters.CheckPopulateFields: boolean;
var
  i: integer;
begin
  for i := 0 to high(Filters) do
  begin
    Result := True;
    if ((Filters[i].FComboBoxColumn.Caption = '') or
      (Filters[i].FComboBoxCondition.Caption = '') or
      (Filters[i].FEdit.Caption = '')) then
    begin
      ShowMessage('Заполните все поля');
      Result := False;
      break;
    end;
  end;
end;

{ TFilter }

constructor TFilter.Create(aOwner: TWinControl; aTable: TTableInfo);
begin
  FCondition := TListOfCondition.Create();
  FTable := aTable;
  FilterInterfae(aOwner);
end;

destructor TFilter.Destroy;
begin
  FreeAndNil(FEdit);
  FreeAndNil(FComboBoxColumn);
  FreeAndNil(FComboBoxCondition);
  FreeAndNil(FDestroyCheckBox);
  FreeAndNil(FPanel);
  inherited Destroy;
end;

procedure TFilter.FilterInterfae(aOwner: TWinControl);
const
  FilterComponentTop = 11;
  FilterComponentHeight = 23;
  FilterComponentWidth = 83;
var
  i: integer;
begin
  FPanel := TPanel.Create(aOwner);
  with FPanel do
  begin
    Parent := aOwner;
    Align := alTop;
    BevelOuter := bvSpace;
  end;

  FComboBoxColumn := TComboBox.Create(FPanel);
  with FComboBoxColumn do
  begin
    Parent := FPanel;
    ReadOnly := True;
    Top := FilterComponentTop;
    Left := 20;
    Height := FilterComponentHeight;
    Width := FilterComponentWidth;
    for i := 0 to high(FTable.ColumnInfos) do
      if (FTable.ColumnInfos[i].VisibleColumn = True) then
        Items.Add(FTable.ColumnInfos[i].Caption);
  end;

  FComboBoxCondition := TComboBox.Create(FPanel);
  with FComboBoxCondition do
  begin
    Parent := FPanel;
    ReadOnly := True;
    Top := FilterComponentTop;
    Left := 125;
    Height := FilterComponentHeight;
    Width := FilterComponentWidth;
    for i := 0 to high(FCondition.Conditions) do
      Items.Add(FCondition.Conditions[i].Caption);
  end;

  FEdit := TEdit.Create(FPanel);
  with FEdit do
  begin
    Parent := FPanel;
    Top := FilterComponentTop;
    Left := 230;
    Height := FilterComponentHeight;
    Width := FilterComponentWidth;
  end;

  FDestroyCheckBox := TCheckBox.Create(FPanel);
  with FDestroyCheckBox do
  begin
    Parent := FPanel;
    Top := FilterComponentTop + 2;
    Left := 330;
    Height := FilterComponentHeight;
  end;
end;

function TFilter.GetQuery(aIndex: integer): string;
begin
  if (not FTable.ColumnInfos[FComboBoxColumn.ItemIndex + 1].Reference) then
    Result += Format(FCondition.Conditions[FComboBoxCondition.ItemIndex].QueryFormat,
      [FTable.Name, FTable.ColumnInfos[FComboBoxColumn.ItemIndex + 1].Name,
      IntToStr(aIndex)])
  else
    Result += Format(FCondition.Conditions[FComboBoxCondition.ItemIndex].QueryFormat,
      [FTable.ColumnInfos[FComboBoxColumn.ItemIndex + 1].ReferenceTable,
      FTable.ColumnInfos[FComboBoxColumn.ItemIndex + 1].ReferenceColumn,
      IntToStr(aIndex)]);
end;

function TFilter.GetCondition: string;
begin
  Result := FCondition.Conditions[FComboBoxCondition.ItemIndex].Caption;
end;

end.
