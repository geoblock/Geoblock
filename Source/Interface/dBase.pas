//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{ The Data Module with nonvisual components like TTable, TQueries and TSQLTable}

unit dBase;

interface

uses
  System.Classes, 
  System.SysUtils,
  Vcl.ImgList, 
  Vcl.Controls, 
  Vcl.Dialogs, 
  Vcl.Graphics,

  //DB
  Data.SqlExpr,
  Data.FMTBcd,
  Data.DB,
  Bde.DBTables;

type
  TdmBase = class(TDataModule)
    SQLQuery:   TSQLQuery;
    SQLTable:   TSQLTable;
    TableOutput: TTable;
    TableInput: TTable;
    QueryInput: TQuery;
    Query:      TQuery;
    DataSourceTemp: TDataSource;
    TableTemp:  TTable;
    DataSource: TDataSource;
    Table:      TTable;
    TableExport: TTable;
    TableMaterials: TTable;
    TableLevels: TTable;
    TableImport: TTable;
    TableInputB: TTable;
    TableLookup: TTable;
    DataSourceLookup: TDataSource;
    TableLegend: TTable;
  private
     
  public
     
  end;

var
  dmBase: TdmBase;

implementation

{$R *.dfm}

end.
