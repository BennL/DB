unit main;

{$mode objfpc}{$H+} {$R+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, metadata, utableform, Menus, Buttons, StdCtrls, ExtCtrls,
  querycreate, UScheduleForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    OpenSchedule: TBitBtn;
    FileNameEdit: TEdit;
    FileSelectButton: TButton;
    IBConnection: TIBConnection;
    Image: TImage;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    CaptionExit: TMenuItem;
    MenuAbout: TMenuItem;
    MenuTable: TMenuItem;
    OpenDialog: TOpenDialog;
    SQLTransaction: TSQLTransaction;
    procedure FileSelectButtonClick(Sender: TObject);
    procedure CaptionExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure NewFormCreate(Sender: TObject);
    procedure OpenScheduleClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  ListOfTable: TListOfTable;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.CaptionExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.FileSelectButtonClick(Sender: TObject);
begin
  IBConnection.Connected := False;
  with OpenDialog do
    if Execute then
      FileNameEdit.Text := FileName;

  IBConnection.DatabaseName := OpenDialog.FileName;
  IBConnection.Connected := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
  MenuItem: TMenuItem;
  Names: Strings;
begin
  FileNameEdit.Text := OpenDialog.FileName;
  IBConnection.DatabaseName := OpenDialog.FileName;
  IBConnection.Connected := True;

  ListOfTable := TListOfTable.Create();

  Names := ListOfTable.GetTableCaption();
  for i := 0 to high(Names) do
  begin
    MenuItem := TMenuItem.Create(MainMenu);
    MenuItem.Caption := Names[i];
    MainMenu.Items.Items[1].Add(MenuItem);
    MenuItem.Tag := i;
    MenuItem.OnClick := @NewFormCreate;
  end;

end;

procedure TMainForm.MenuAboutClick(Sender: TObject);
begin
  ShowMessage('Швецова А. Б8103а');
end;

procedure TMainForm.NewFormCreate(Sender: TObject);
var
  NewForm: TTableForm;
  index: integer;
begin
  index := TMenuItem(Sender).Tag;
  NewForm := TTableForm.Create(MainForm, ListOfTable.TableInfos[index]);
  NewForm.Caption := ListOfTable.GetTableCaption[index];
  NewForm.MTable := ListOfTable.TableInfos[index];
  NewForm.Show;
  with NewForm do
    ShowTable(SQLQuery, DBGrid, MTable);
end;

procedure TMainForm.OpenScheduleClick(Sender: TObject);
begin
  ScheduleForm.Show;
end;

end.
