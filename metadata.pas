unit metadata;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, DB, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs;

const
  NOT_STRING_VALUE = '';

type

  Strings = array of string;

  TColumnInfo = record
    Name: string;
    Caption: string;
    Size: integer;
    FieldType: TFieldType;
    Reference: boolean;
    VisableColumn: boolean;
    ReferenceColumn: string;
    ReferenceTable: string;
    AliasName: string;
    ParentTable: string;
  end;

  { TTableInfo }

  TTableInfo = class
    ColumnInfos: array of TColumnInfo;
    Name: string;
    Caption: string;
    GenerateName: string;
    constructor Create(aCaption, aName: string);
    function AddColumn(aCaption, aName: string; aFieldType: TFieldType;
      aSize: integer; aReference: boolean; aVisible: boolean;
      aReferenceColumn, aReferenceTable: string; aParentTable: string): TTableInfo;
  end;

  { TListOfTable }

  TListOfTable = class
    TableInfos: array of TTableInfo;
    constructor Create();
    function AddTable(aCaption, aName: string): TTableInfo;
    function GetTableCaption(): Strings;
  end;

function GetTableByName(aName: string): TTableInfo;
function GetTableByColumn(aColumn: TColumnInfo): TTableInfo;

var
  ListOfTable: TListOfTable;

implementation

function GetTableByName(aName: string): TTableInfo;
var
  i: integer;
begin
  with ListOfTable do
    for i := 0 to High(TableInfos) do
      if LowerCase(aName) = LowerCase(TableInfos[i].Name) then
        Exit(TableInfos[i]);

  Result := nil;
end;

function GetTableByColumn(aColumn: TColumnInfo): TTableInfo;
begin
  Result := GetTableByName(aColumn.ParentTable);
end;

{ TListOfTable }

constructor TListOfTable.Create();
begin

  AddTable('Преподаватели', 'Professors').
    AddColumn('ID', 'ID', ftInteger, 10,
      False, False, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Professors').
    AddColumn('Преподаватель', 'Name', ftString, 110,
      False, True, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Professors');

  AddTable('Предметы', 'Subjects').
    AddColumn('ID', 'ID', ftInteger, 10,
      False, False, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Subjects').
    AddColumn('Предмет', 'Name', ftString, 385,
      False, True, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Subjects');

  AddTable('Кабинеты', 'Rooms').
    AddColumn('ID', 'ID', ftInteger, 10,
      False, False, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Rooms').
    AddColumn('Кабинет', 'Name', ftString, 100,
      False, True, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Rooms').
    AddColumn('Вместимость', 'Room_size', ftInteger, 80,
      False, True, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Rooms');

  AddTable('Группы', 'Groups').
    AddColumn('ID', 'ID', ftInteger, 10,
      False, False, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Groups').
    AddColumn('Группа', 'Name', ftString, 70,
      False, True, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Groups').
    AddColumn('Количество человек', 'Group_size', ftInteger, 30,
      False, True, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Groups');

  AddTable('Дни недели', 'Days').
    AddColumn('ID', 'ID', ftInteger, 10,
      False, False, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Days').
    AddColumn('Название', 'Name', ftString, 100,
      False, True, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Days');

  AddTable('Типы занятий', 'Subject_Types').
    AddColumn('ID', 'ID', ftInteger, 10,
      False, False, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Subject_Types').
    AddColumn('Название', 'Name', ftString, 30,
      False, True, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Subject_Types');

  AddTable('Неделя', 'Weeks').
    AddColumn('ID', 'ID', ftInteger, 10,
      False, False, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Weeks').
    AddColumn('Тип недели', 'Name', ftString, 100,
      False, True, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Weeks');

  AddTable('Расписание звонков', 'Times').
    AddColumn('ID', 'ID', ftInteger, 10,
      False, False, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Times').
    AddColumn('Время', 'Begin_End_Time', ftString, 100,
      False, True, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Times');

  AddTable('Расписание', 'Schedule_items').
    AddColumn('ID', 'ID', ftInteger, 10,
      False, False, NOT_STRING_VALUE, NOT_STRING_VALUE, 'Schedule_items').
    AddColumn('Предмет', 'Subject_id', ftString, 385,
      True, True, 'Name', 'Subjects', 'Schedule_items').
    AddColumn('Тип', 'Subject_type_id', ftString, 30,
      True, True, 'Name', 'Subject_types', 'Schedule_items').
    AddColumn('Преподаватель', 'Professor_id', ftString, 110,
      True, True, 'Name', 'Professors', 'Schedule_items').
    AddColumn('Время', 'Time_id', ftString, 90,
      True, True, 'Begin_End_Time', 'Times', 'Schedule_items').
    AddColumn('День недели', 'Day_id', ftDate, 90,
      True, True, 'Name', 'Days', 'Schedule_items').
    AddColumn('Группа', 'Group_id', ftString, 70,
      True, True, 'Name', 'Groups', 'Schedule_items').
    AddColumn('Кабинет', 'Room_id', ftString, 55,
      True, True, 'Name', 'Rooms', 'Schedule_items').
    AddColumn('Неделя', 'Week_id', ftString, 90,
      True, True, 'Name', 'Weeks', 'Schedule_items');

end;

function TListOfTable.AddTable(aCaption, aName: string): TTableInfo;
begin
  Result := TTableInfo.Create(aCaption, aName);
  SetLength(TableInfos, length(TableInfos) + 1);
  TableInfos[high(TableInfos)] := Result;
end;

function TListOfTable.GetTableCaption: Strings;
var
  i: integer;
begin
  SetLength(Result, length(TableInfos));
  for i := 0 to high(TableInfos) do
    Result[i] := TableInfos[i].Caption;
end;

{ TTableInfo }

constructor TTableInfo.Create(aCaption, aName: string);
begin
  Caption := aCaption;
  Name := aName;
  GenerateName := Name + '_GEN';
end;

function TTableInfo.AddColumn(aCaption, aName: string; aFieldType: TFieldType;
  aSize: integer; aReference: boolean; aVisible: boolean; aReferenceColumn,
  aReferenceTable: string; aParentTable: string): TTableInfo;
begin
  SetLength(ColumnInfos, length(ColumnInfos) + 1);
  with ColumnInfos[high(ColumnInfos)] do
  begin
    Name := aName;
    Caption := aCaption;
    Reference := aReference;
    VisableColumn := aVisible;
    ParentTable := aParentTable;


    if (Reference) then
    begin
      ReferenceTable := aReferenceTable;
      ReferenceColumn := aReferenceColumn;
    end;

    AliasName := Self.Name + aName;

    Size := aSize;
    FieldType := aFieldType;
  end;

  Result := Self;
end;

initialization

  ListOfTable := TListOfTable.Create();

end.
