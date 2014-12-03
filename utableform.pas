unit utableform;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, sqldb, DB, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, Buttons, StdCtrls, metadata, UFilterForm, querycreate,
  ueditform, ufilter;

type

  { TTableForm }

  TTableForm = class(TForm)
    ResetFilterBitBtn: TBitBtn;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    InsertBitBtn: TBitBtn;
    SQLQuery: TSQLQuery;
    AddFilterBitBtn: TBitBtn;
    procedure AddFilterBitBtnClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure InsertBitBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure ResetBitBtnClick(Sender: TObject);
    procedure OnFilterFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OnEditFormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    Order: boolean;
  public
    MTable: TTableInfo;
    constructor Create(aOwner: TControl; aTable: TTableInfo);
  end;

var
  TableForm: TTableForm;
  ListOfEditForm: TListOfEditForm;

implementation

{$R *.lfm}

{ TTableForm }

procedure TTableForm.OKBtnClick(Sender: TObject);
var
  i: integer;
  Param: array of string;
begin
  with ListOfFilters do
  begin
    if (length(Filters) <> 0) and (CheckPopulateFields) then
    begin
      for i := 0 to high(Filters) do
      begin
        with (Filters[i]) do
        begin
          SetLength(Param, length(Param) + 1);
          Param[i] := Format(FCondition.Conditions[
            FComboBoxCondition.ItemIndex].ParamFormat, [FEdit.Caption]);

        end;
      end;
      ShowFilterTable(SQLQuery, DBGrid, MTable, CreateFQuery, Param);
    end;
  end;
end;

procedure TTableForm.ResetBitBtnClick(Sender: TObject);
begin
  ShowTable(SQLQuery, DBGrid, MTable);
end;

procedure TTableForm.OnFilterFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  AddFilterBitBtn.Enabled := True;
end;

procedure TTableForm.OnEditFormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
  CurrID: integer;
  HashResult: integer;
begin
  InsertBitBtn.Enabled := true;
  CurrID := TEditForm(Sender).ID;
  
  if CurrID <> 0 then 
  begin

   with ListOfEditForm do
   begin
     HashResult := HashFunction(CurrID);
     DeleteForm(EditForms[HashResult],
       EditForms[HashResult][high(EditForms[HashResult])]);
   end;
  end;

  CloseAction := caFree;
  ShowTable(SQLQuery, DBGrid, MTable);
end;

procedure TTableForm.DBGridTitleClick(Column: TColumn);
var
  Query: string;
begin
  Order := not Order;
  Query := '';
  if (ListOfFilters.Count() <> 0) then
    Query := ListOfFilters.CreateFQuery;

  ShowSortTable(SQLQuery, DBGrid, MTable, Query, Column.Index, Order);
end;

procedure TTableForm.AddFilterBitBtnClick(Sender: TObject);
var
  NewForm: TFilterForm;
begin
  TButton(Sender).Enabled := False;
  NewForm := TFilterForm.Create(Self);
  with NewForm do
  begin
    FilterTable := MTable;
    OKBitBtn.OnClick := @OKBtnClick;
    OnClose := @OnFilterFormClose;
    Show;
  end;
end;

procedure TTableForm.DBGridDblClick(Sender: TObject);
var
  NewForm: TEditForm;
  HashResult: integer;
  CurrID: integer;
begin
  CurrID := SQLQuery.Fields[0].AsInteger;
  NewForm := TEditForm.Create(Self, MTable, CurrID);
  with ListOfEditForm do
  begin
    HashResult := HashFunction(CurrID);
    InsertForm(EditForms[HashResult], NewForm);

    with EditForms[HashResult][high(EditForms[HashResult])] do
    begin
      OnClose := @OnEditFormClose;
      Show;
    end;
  end;
end;

procedure TTableForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (length(ListOfFilters.Filters) <> 0) then
    ListOfFilters.Destroy;
end;


procedure TTableForm.InsertBitBtnClick(Sender: TObject);
var
  NewForm: TEditForm;
begin
  TBitBtn(Sender).Enabled := false;
  NewForm := TEditForm.Create(Self, MTable, 0);
  with NewForm do
  begin
    OnClose := @OnEditFormClose;
    Show;
  end;
end;

constructor TTableForm.Create(aOwner: TControl; aTable: TTableInfo);
begin
  inherited Create(aOwner);
  MTable := aTable;
  ListOfEditForm := TListOfEditForm.Create;
end;

end.



