Unit MainUnit;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, Menus, MetaUnit, UTables, Query;

Type

  { TMainForm }

  TMainForm = Class(TForm)
    IBConnection: TIBConnection;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    About: TMenuItem;
    mExit: TMenuItem;
    TablesMenu: TMenuItem;
    SQLTransaction: TSQLTransaction;
    Procedure AboutClick(Sender: TObject);
    Procedure mExitClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure ShowForm(Sender: TObject);
  Private
    { private declarations }
  Public
    { public declarations }
  End;

var
  MainForm: TMainForm;
  ListOfTables: TListOfTables;

Implementation

{$R *.lfm}

{ TMainForm }

Procedure TMainForm.mExitClick(Sender: TObject);
begin
  SQLTransaction.Active := False;
  IBConnection.Connected := False;
  MainForm.Close;
end;

Procedure TMainForm.AboutClick(Sender: TObject);
begin
  ShowMessage('Швецова А.');
end;

Procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  MenuItem: TMenuItem;
begin
  MainForm.IBConnection.Connected := True;

  ListOfTables := TListOfTables.Create;
  For i := 0 To high(ListOfTables.TableInfos) Do
  Begin
    MenuItem := TMenuItem.Create(MainMenu);
    MenuItem.Caption := ListOfTables.TableInfos[i].TCaption;
    MenuItem.Tag := i;
    MenuItem.OnClick := @ShowForm;
    TablesMenu.Add(MenuItem);
  End;
end;

Procedure TMainForm.ShowForm(Sender: TObject);
var
  NewForm: TGridForm;
begin
  NewForm := TGridForm.Create(MainForm, ListOfTables.TableInfos[TMenuItem(Sender).Tag]);
  NewForm.Caption := TMenuItem(Sender).Caption;
  NewForm.Table := ListOfTables.TableInfos[TMenuItem(Sender).Tag];
  ShowItems(NewForm.SQLQuery, NewForm.DBGrid, NewForm.Table);
  NewForm.Show;
end;

end.
