unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, process, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ExtCtrls, LCLType, RxAboutDialog, Market,
  Page_Assets, page_Market, Page_Portfolio;

type

  { TwMain }

  TwMain = class(TForm)
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    dlgAbout: TRxAboutDialog;
    imglstMenues: TImageList;
    MenuItem1: TMenuItem;
    mnDebugZoneClear: TMenuItem;
    mnDEbughistoric: TMenuItem;
    mnDebug: TMenuItem;
    mnAssetsUpdate: TMenuItem;
    mnAssetsUpdateAll: TMenuItem;
    mnTools: TMenuItem;
    mnUpdate: TMenuItem;
    mnToolsFillList: TMenuItem;
    Separator4: TMenuItem;
    tabControl: TPageControl;
    mnUpdateAll: TMenuItem;
    mnAssets: TMenuItem;
    mnAssetsAdd: TMenuItem;
    Separator2: TMenuItem;
    mnAssetsListExport: TMenuItem;
    mnAssetsListImport: TMenuItem;
    Separator3: TMenuItem;
    mnHelpAbout: TMenuItem;
    mnFileConfiguration: TMenuItem;
    mnFileQuit: TMenuItem;
    mnFiles: TMenuItem;
    mnHelp: TMenuItem;
    mnMain: TMainMenu;
    Separator1: TMenuItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure mnAssetsClick(Sender: TObject);
    procedure mnAssetsUpdateAllClick(Sender: TObject);
    procedure mnAssetsUpdateClick(Sender: TObject);
    procedure mnAssetsAddClick(Sender: TObject);
    procedure mnAssetsListExportClick(Sender: TObject);
    procedure mnAssetsListImportClick(Sender: TObject);
    procedure mnDEbughistoricClick(Sender: TObject);
    procedure mnDebugZoneClearClick(Sender: TObject);
    procedure mnFileConfigurationClick(Sender: TObject);
    procedure mnFileQuitClick(Sender: TObject);
    procedure mnHelpAboutClick(Sender: TObject);
    procedure mnToolsFillListClick(Sender: TObject);
  private
    fAssetsPage    : TpgAssets;
    fMarketPage    : TpgMarket;
    fPortfolioPage : TpgPortfolio;

  public

  end;

var
  wMain: TwMain;

implementation

uses Data_Assets, Config, DLG_AssetAdd, DLG_Assets_FillList, Data_Provider,
  Data_Historic, Data_Assets_Const;

{$R *.lfm}

{ TwMain }

// =============================================================================
// Crear y Destruir
// =============================================================================

procedure TwMain.FormCreate(Sender: TObject);

 procedure SetPage(aPage : TForm);
 var
  tb : TTabSheet;
 begin
  tb := TTabSheet.Create(tabControl);
  tb.PageControl := tabControl;

  with aPage do
   begin
    Parent := tb;
    tb.Caption := Caption;
    BorderStyle := bsNone;
    Align := alClient;
    Show;
   end;
 end;

begin
  Caption := Application.Title;

  // Configurar dialogos ------------------------------
  with dlgSave do
   begin
    InitialDir := GetUserDir;
    Filter :='Valores separados por comas|*.csv|Todos los archivos|*.*';
    FileName := 'activos_lista.csv';
    DefaultExt := 'csv';
   end;

  with dlgOpen do
   begin
    InitialDir := dlgSave.InitialDir;
    Filter := dlgSave.Filter;
    FileName := dlgSave.FileName;
    DefaultExt := dlgSave.DefaultExt;
   end;

  // Agregar paginas ==================================
  fAssetsPage := TpgAssets.Create(Self);
  SetPage(fAssetsPage);

  fPortfolioPage := TpgPortfolio.Create(Self);
  SetPage(fPortfolioPage);

  fMarketPage := TpgMarket.Create(Self);
  SetPage(fMarketPage);
end;

procedure TwMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fAssetsPage.UpdateStop(False);
end;

procedure TwMain.FormDestroy(Sender: TObject);
begin
  // ----------------------
end;

procedure TwMain.MenuItem1Click(Sender: TObject);
 const
   InputStrs: array[0..10] of string = (
     '2254452480', '5198202880', '2151118592', '3066162688', '2930187776',
     '2626909440', '3238687232', '5337662464', '7518394368', '2903615232', '6963739648' );
   ExpectedVals: array[0..10] of UInt64 = (
     18446744071669036800, 903235584, 18446744071565702912, 18446744072480747008,
     18446744072344772096, 18446744072041493760, 18446744072653271552, 1042695168,
     18446744072638011392, 18446744072318199552, 18446744072083356672 );
 var
   i: Integer;
   ConvertedVal: UInt64;
   ErrorCode: Integer;
   StrVal: string;
 begin
   for i := 0 to High(InputStrs) do
   begin
     Val(InputStrs[i], ConvertedVal, ErrorCode);
     StrVal := IntToStr(ConvertedVal);

     Writeln('Input: ', InputStrs[i]);
     Writeln('Expected: ', ExpectedVals[i]);
     Writeln('Val Conversion: ', ConvertedVal);
     Writeln('IntToStr Result: ', StrVal);

     if ConvertedVal = ExpectedVals[i] then
       Writeln('✅ Conversion OK')
     else
       Writeln('❌ Conversion Failed');

     Writeln('-------------------------');
   end;
 end;

// =============================================================================
// = MENUS                                                                     =
// =============================================================================

// * ARCHIVO *******************************************************************
procedure TwMain.mnFileConfigurationClick(Sender: TObject);
begin
  ShowMessage('TODO');
end;

procedure TwMain.mnFileQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

// * ACTIVOS *******************************************************************
procedure TwMain.mnAssetsClick(Sender: TObject);
begin
  mnAssetsUpdate.Enabled    := not fAssetsPage.Updating;
  mnAssetsUpdateAll.Enabled := mnAssetsUpdate.Enabled;
end;

procedure TwMain.mnAssetsAddClick(Sender: TObject);
var
  Asset : TAsset;
begin
  with TDLGAssetAdd.Create(Self) do
   begin

   if ShowModal = mrOK then
    with DataAssets do
     begin // Agregar --------------------------
      MarkPosition;
      for Asset in AssetsToAdd do Add(Asset);
      RestorePosition;
    end;

    Free;
   end;
end;

// * EXP. / IMP. -----------------------------------------
procedure TwMain.mnAssetsListExportClick(Sender: TObject);
begin
  with dlgSave do
   if Execute then DataAssets.ExportCSV(FileName);
end;

procedure TwMain.mnAssetsListImportClick(Sender: TObject);
begin
  with dlgOpen do
   if Execute then DataAssets.ImportCSV(FileName);
end;



// ACTUALIZACION -------------------------------------------
procedure TwMain.mnAssetsUpdateAllClick(Sender: TObject);
begin
  fAssetsPage.UpdateStart;
end;

procedure TwMain.mnAssetsUpdateClick(Sender: TObject);
begin
  fAssetsPage.UpdateStart(True);
end;

// * TOOLS ********************************************************************
procedure TwMain.mnToolsFillListClick(Sender: TObject);
begin
  with fAssetsPage do
   begin
    dsGrid.Enabled := False;
    grdAssets.DataSource := nil;

    with TDLGAssetsFillList.Create(Self) do ShowModal;

    grdAssets.DataSource := dsGrid;
    dsGrid.Enabled := True;
   end;
end;

// * AYUDA *********************************************************************
procedure TwMain.mnHelpAboutClick(Sender: TObject);
begin
  with dlgAbout do
   begin
    // --------------------
    Execute;
   end;
end;

// *****************************************************************************
// ** DEBUG
// *****************************************************************************

procedure TwMain.mnDEbughistoricClick(Sender: TObject);
var
  List : TAssetDataArray;
  Asset : TAssetID;
  i : Integer;
  TimeBegin : TDateTime;

  function Format (DT : TdateTime) : String;
  begin
    Result := FormatFloat('0.###', DT * 1e6) + ' µs';
  end;


begin
  Asset := DataAssets.GetActualAssetID;

  WriteLN(UpperCase(Asset),  ' ==================');

  TimeBegin := now;
  DataHistoric.Historic(List, Asset);
  WriteLN('1 paso ... ', Format(Now - TimeBegin));

  TimeBegin := now;
  DataHistoric.Historic(List, Asset);
  WriteLN('2 paso ... ', Format(Now - TimeBegin));

  TimeBegin := now;
  DataHistoric.Historic(List, Asset);
  WriteLN('3 paso ... ', Format(Now - TimeBegin));

end;

procedure TwMain.mnDebugZoneClearClick(Sender: TObject);
begin
  with DataAssets do
   begin
    MarkPosition;
    tblData.First;

    while not tblData.EOF do
     begin
      SetFieldInteger(FLD_MARK_ZONE,FLD_MARK_ZONE_NOVALUE);
      tblData.Next;
     end;

    RestorePosition;
   end;
end;



end.

