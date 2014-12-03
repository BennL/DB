unit DBComponentCreate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, DB;

function SQLQueryCreate(): TSQLQuery;


implementation

uses
  main;

function SQLQueryCreate: TSQLQuery;
var
  SQLQuery: TSQLQuery;
  DataSource: TDataSource;
begin
  SQLQuery := TSQLQuery.Create(nil);
  DataSource := TDataSource.Create(nil);

  SQLQuery.DataSource := DataSource;

  SQLQuery.DataBase := MainForm.IBConnection;
  SQLQuery.Transaction := MainForm.SQLTransaction;

  Result := SQLQuery;
end;


end.
