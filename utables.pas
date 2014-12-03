unit UTables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, MetaUnit;

type

  { TGridForm }

  TGridForm = class(TForm)
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    SQLQuery: TSQLQuery;
  public
   Table: TTable;
   constructor Create(aOwner: TControl);
  end;

var
  GridForm: TGridForm;

implementation

{$R *.lfm}

{ TGridForm }

constructor tgridform.create(aowner: tcontrol);
begin
  inherited Create(aOwner);

end;


end.

