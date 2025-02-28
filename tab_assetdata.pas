unit Tab_AssetData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, DBCtrls, Spin,
  StdCtrls, ExtCtrls, RxDBSpinEdit, cyDBMemo, Data_Assets;

type

  { TtabAssetData }

  TtabAssetData = class(TForm)
    dbNote: TDBMemo;
    dbSYMBOL: TDBText;
    dbDrescription: TDBText;
    dsAssets: TDataSource;
    dbTYPE: TDBText;
    grpHB: TGroupBox;
    lblSensibility: TLabel;
    RxDBSpinEdit1: TRxDBSpinEdit;
  private

  public

  end;

var
  tabAssetData: TtabAssetData;

implementation

{$R *.lfm}

{ TtabAssetData }


end.

