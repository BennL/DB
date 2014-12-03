unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, Menus, MetaUnit, UTables, Query;

type

  { TMainForm }

  TMainForm = class(TForm)
    IBConnection: TIBConnection;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    About: TMenuItem;
    mExit: TMenuItem;
    TablesMenu: TMenuItem;
    SQLTransaction: TSQLTransaction;
    procedure AboutClick(Sender: TObject);
    procedure mExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowForm(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  ListOfTables: TListOfTables;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.mExitClick(Sender: TObject);
begin
  SQLTransaction.Active:= False;
  IBConnection.Connected:= False;
  MainForm.Close;
end;

procedure TMainForm.AboutClick(Sender: TObject);
begin
  ShowMessage('Швецова А.');
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
  MenuItem: TMenuItem;
begin
  MainForm.IBConnection.Connected:= True;

  ListOfTables:= TListOfTables.Create;
  for i:= 0 to high(ListOfTables.TableInfos) do
  begin
    MenuItem:= TMenuItem.Create(MainMenu);
    MenuItem.Caption:= ListOfTables.TableInfos[i].TCaption;
    MenuItem.Tag:= i;
    MenuItem.OnClick:= @ShowForm;
    TablesMenu.Add(MenuItem);
  end;
end;

procedure TMainForm.ShowForm(Sender: TObject);
var
  NewForm: TGridForm;
begin
  NewForm:= TGridForm.Create(MainForm);
  NewForm.Caption:= TMenuItem(sender).Caption;
  NewForm.Table := ListOfTables.TableInfos[TMenuItem(Sender).Tag];
  ShowItems(NewForm.SQLQuery, NewForm.DBGrid, NewForm.Table);
  NewForm.Show;
end;

end.
