unit MetaUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, DBGrids, DBCtrls, sqldb, Menus, Dialogs;

type

  { TColumn }

  TColumn = object
    Name: string;
    Caption: string;
    ReferenceTable: string;
    ReferenceName: string;
    Size: integer;
    function IsReference: boolean; inline;
  end;

  TColumns = array of TColumn;

  { TTable }

  TTable = class
    ColumnInfos: TColumns;
    TCaption: string;
    TName: string;
    TColumn: TColumns;
    function AddColumn(aName, aCaption, aReferenceName: string;
      aSize: integer): TTable;
    constructor Create(aCaption, aName: string);
  end;

  //TTableClass = class of TTable;
  TTables = array of TTable;

  TListOfTables = class
    TableInfos: TTables;
    constructor Create();
    function Add(aCaption, aName: string): TTable;
  end;

implementation

{ TColumn }

function TColumn.IsReference: boolean;
begin
  Result := ReferenceName <> '';
end;

function TTable.AddColumn(aName, aCaption, aReferenceName: string;
  aSize: integer): TTable;
begin
  SetLength(ColumnInfos, length(ColumnInfos) + 1);
  with ColumnInfos[High(ColumnInfos)] do
  begin
    Name := aName;
    Caption := aCaption;
    if aReferenceName <> '' then
    begin
      ReferenceName := aReferenceName;
      ReferenceTable := Copy(aName, 1, Length(aName) - 3) + 's';
    end;
    Size := aSize;
  end;
  Result := self;
end;

constructor TTable.Create(aCaption, aName: string);
begin
  TCaption := aCaption;
  TName := aName;
end;

function TListOfTables.Add(aCaption, aName: string): TTable;
begin
  Result := TTable.Create(aCaption, aName);
  SetLength(TableInfos, Length(TableInfos) + 1);
  TableInfos[High(TableInfos)] := Result;
end;


constructor TListOfTables.Create();
begin
  Add('Предметы', 'Subjects').
    AddColumn('Name', 'Название предмета', '', 400);
  Add('Группы', 'Groups').
    AddColumn('Name', 'Группа', '', 100).
    AddColumn('Group_size', 'Размер группы', '', 100);
  Add('Преподаватели', 'Professors').
    AddColumn('Name', 'Преподаватель', '', 150);
  Add('Кабинеты', 'Rooms').
    AddColumn('Name', 'Номер кабинета', '', 100).
    AddColumn('Room_size', 'Вместимость', '', 80);
  Add('Расписание', 'Schedule_items').
    AddColumn('Subject_id', 'Название предмета', 'Name', 400).
    AddColumn('Subject_type_id', 'Тип', 'Name', 50).
    AddColumn('Professor_id', 'Преподаватель', 'Name', 150).
    AddColumn('Day_id', 'День недели', 'Name', 100).
    AddColumn('Group_id', 'Группа', 'Name', 100).
    AddColumn('Room_id', 'Номер кабинета', 'Name', 100);
end;

end.
