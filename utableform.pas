unit utableform;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, Buttons, StdCtrls, metadata, ufilter, querycreate,
  ueditform;

type

  { TTableForm }

  TTableForm = class(TForm)
    ResetBitBtn: TBitBtn;
    DestroyBitBtn: TBitBtn;
    UpdateBitBtn: TBitBtn;
    FilterBitBtn: TBitBtn;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    FilterScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure DBGridTitleClick(Column: TColumn);
    procedure DestroyBitBtnClick(Sender: TObject);
    procedure FilterBitBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ResetBitBtnClick(Sender: TObject);
    procedure UpdateBitBtnClick(Sender: TObject);
    function CheckPopulateFields: Boolean;
  private
    Order: Boolean;
  public
    MTable: TTableInfo;
    constructor Create (aOwner: TControl; aTable: TTableInfo);
  end;

var
  TableForm: TTableForm;
  ListOfFilters: TListOfFilters;

implementation

{$R *.lfm}

{ TTableForm }

procedure TTableForm.ResetBitBtnClick(Sender: TObject);
begin
  if (length (ListOfFilters.Filters) <> 0) then ListOfFilters.Destroy;
  ShowTable (SQLQuery, DBGrid, MTable);
end;

procedure TTableForm.UpdateBitBtnClick(Sender: TObject);
var
  i: integer;
  Param: array of string;
begin
  with ListOfFilters do begin
    if (length (Filters) <> 0) then
      if (CheckPopulateFields) then begin
        for i := 0 to high (Filters) do begin
          with (Filters[i]) do begin
            SetLength (Param, length (Param) + 1);
            Param[i] := Format (FCondition.Conditions[FComboBoxCondition.ItemIndex].ParamFormat,
            [FEdit.Caption]);
          end;
        end;
        ShowFilterTable (SQLQuery, DBGrid, MTable, CreateFQuery, Param);
      end;
  end;
end;

function TTableForm.CheckPopulateFields: Boolean;
var
  i: integer;
begin
  with ListOfFilters do begin
    for i := 0 to high (Filters) do begin
      Result := true;

      if ((Filters[i].FComboBoxColumn.Caption = '') or
        (Filters[i].FComboBoxCondition.Caption = '') or
        (Filters[i].FEdit.Caption = '')) then begin
          ShowMessage ('Заполните все поля');
          Result := false;
          break;
        end;

    end;
  end;
end;

procedure TTableForm.DBGridTitleClick(Column: TColumn);
var
  Query: string;
begin
  Order := not Order;
  Query := '';
  if (ListOfFilters.Count() <> 0) then
    Query := ListOfFilters.CreateFQuery;

  ShowSortTable (SQLQuery, DBGrid, MTable, Query, Column.Index, Order);
end;

procedure TTableForm.DestroyBitBtnClick(Sender: TObject);
var
  i: integer;
  flag: Boolean;
begin
  if ListOfFilters.Count <> 0 then begin
    flag := false;
    while (not flag) do begin
      flag := true;
      for i := 0 to high (ListOfFilters.Filters) do begin
        if (ListOfFilters.Filters[i].FDestroyCheckBox.Checked) then begin
          ListOfFilters.DeleteFilter (i);
          flag := false;
          break;
        end;
      end;
    end;
  end;
end;

procedure TTableForm.FilterBitBtnClick(Sender: TObject);
begin
  ListOfFilters.AddFilter (FilterScrollBox, MTable);
end;

procedure TTableForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (length (ListOfFilters.Filters) <> 0) then
    ListOfFilters.Destroy;
end;

constructor TTableForm.Create(aOwner: TControl; aTable: TTableInfo);
begin
  inherited Create(aOwner);
  MTable := aTable;
  ListOfFilters := TListOfFilters.Create;
end;

end.

