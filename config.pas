unit Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Graphics;

type
  TConfig = class(TObject)
  private
    fINI : TINIFile;

    fBaseDir   : TFileName;
    fBackupDir : TFileName;

    fAssetsDataFile   : TFileName;
    fHistoricDataFile : TFileName;

    fBackup   : Boolean;

    fHBSensibilityDefault: Integer;

    fCategories : TStringList;
    fZones      : TStringList;

    procedure _Save;
    procedure _Load;

    procedure _SetHBSensibilityDefault(aValue: Integer);

    function _GetAskQuestion(aID :String) : Boolean;
    procedure _SetAskQuestion(aID :String; aValue : Boolean);

    function _GetGridColumnState(aID : String): String;
    procedure _SetGridColumnState(aID : String; aValue: String);

  public
    constructor Create;
    destructor  Destroy; override;

    property INIFile : TINIFile read fINI;

    // Archivos ====================================================
    property BaseDir          : TFileName read fBaseDir;
    property AssetsDataFile   : TFileName read fAssetsDataFile;
    property HistoricDataFile : TFileName read fHistoricDataFile;

    property Backup    : Boolean   read fBackup;
    property BackupDir : TFileName read fBackupDir;

    // Heart Beat ==================================================
    property HBSensibilityDefault : Integer read fHBSensibilityDefault write _SetHBSensibilityDefault;

    // Grupos ==================================================
    property Categories : TStringList read fCategories write fCategories;
    property Zones : TStringList read fZones write fZones;

    // Preguntas ===================================================
    property Question[aID: String]: Boolean read _GetAskQuestion write _SetAskQuestion;

    // Columnas de la grilla =======================================
    property GridColumnState[aID : String] : String read _GetGridColumnState write _SetGridColumnState;


  end;

var
  Configuration: TConfig;

implementation

uses HeartBeat, Tools, LazLogger;

const
   NEW_PREFIX = 'test_';

const
  SECTION_DATA = 'Data'; // ----------------------------------------------
   DATA_ASSETS_FILE   = 'Assets';   DATA_ASSETS_FILE_DEFAULT   = NEW_PREFIX + 'assets.data';
   DATA_HISTORIC_FILE = 'Historic'; DATA_HISTORIC_FILE_DEFAULT = NEW_PREFIX + 'historic.db';

   DATA_BACKUP = 'Backup data'; DATA_BACKUP_DEFAULT = True;

  //SECTION_VIEW = 'Filter'; // -----------------------------------------------
  // VIEW_PANEL  = 'Filter panel';   VIEW_PANEL_VALUE         = True;

  SECTION_HB = 'Heart Beat'; // -------------------------------------------
   HB_SENSIBILITY = 'Default sensibility'; HB_SENSIBILITY_DEFAULT = 3; // HB_SENSIBILITY_DEFAULT;

  SECTION_QUESTIONS = 'Questions'; // -------------------------------------

  SECTION_ASSETS_GRID = 'Assets Grid';

  SECTION_ASSETS_CATEGORIES = 'Categories';

  SECTION_ZONES = 'Zones';

  SECTION_DEBUG = 'DEBUG';
   DEBUG_VERBOSITY = 'Verbosity'; DEBUG_VERBOSITY_DEFAULT = 1;
   DEBUG_LEVEL     = 'Level';     DEBUG_LEVEL_DEFAULT     = 1;

{ TConfig }

constructor TConfig.Create;
begin
  fBaseDir := GetAppConfigDir(False);
  fINI := TINIFile.Create(fBaseDir + NEW_PREFIX + ApplicationName + ConfigExtension);
  fINI.UpdateFile; // Crear directorio y archivo

  DefaultFormatSettings.DecimalSeparator  := ',';
  DefaultFormatSettings.ThousandSeparator := '.';

  // Configuracion interna ==================================
  SetLength(TrueBoolStrs, 4);   SetLength(FalseBoolStrs, 4);
  TrueBoolStrs[0] := 'T';       FalseBoolStrs[0] := 'F';
  TrueBoolStrs[1] := '1';       FalseBoolStrs[1] := '0';
  TrueBoolStrs[2] := 'V';       FalseBoolStrs[2] := 'F';
  TrueBoolStrs[3] := 'True';    FalseBoolStrs[3] := 'False';

  fCategories := TStringList.Create;
  fZones := TStringList.Create;

  _Load;
end;

destructor TConfig.Destroy;
begin
  _Save;
  fCategories.Free;
  fINI.Free;
end;

// *****************************************************************************
// * LEER                                                                      *
// *****************************************************************************

function CompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
begin
  // Comparamos los valores alfabéticamente
  with List do Result := CompareStr(ValueFromIndex[Index1], ValueFromIndex[Index2]);
end;

procedure TConfig._Load;
begin
  with fINI do
   begin
    // Datos / Archivos ========================================================
    fAssetsDataFile   := ReadString(SECTION_DATA, DATA_ASSETS_FILE, fBaseDir + DATA_ASSETS_FILE_DEFAULT);
    fHistoricDataFile := ReadString(SECTION_DATA, DATA_HISTORIC_FILE, fBasedir + DATA_HISTORIC_FILE_DEFAULT);

    fBackup := ReadBool(SECTION_DATA, DATA_BACKUP, DATA_BACKUP_DEFAULT);

    fBackupDir := fBaseDir + 'backup/';
    if not DirectoryExists(fBackupDir) then
     if not CreateDir(fBackupDir) then fBackupDir := fBaseDir;

    // Heart Beat ==============================================================
    fHBSensibilityDefault := ReadInteger(SECTION_HB, HB_SENSIBILITY, HB_SENSIBILITY_DEFAULT);

    // Filtros =================================================================
    // Categorías --------------------------------------------------------------
    ReadSectionValues(SECTION_ASSETS_CATEGORIES, fCategories, []);

    with fCategories do
     if Count = 0 then
      begin // Por defecto
       AddPair( '0', 'Bancario');        AddPair( '1', 'Financiero');
       AddPair( '2', 'Entretenimiento'); AddPair( '3', 'Medicina');
       AddPair( '4', 'Tecnología');      AddPair( '5', 'Transporte');
       AddPair( '6', 'Industríal');      AddPair( '7', 'Minería');
       AddPair( '8', 'Cripto/IA');       AddPair( '9', 'Comercial');
       AddPair('10', 'Energía');         AddPair('11', 'Empresa');
       AddPair('12', 'Indice');          AddPair('13', 'Comida');
       AddPair('14', 'Web');             AddPair('15', 'Comunicación');
       AddPair('16', 'China');
      end;

    fCategories.CustomSort(@CompareStrings); // Ordenar alfabeticamente

    // Categorías --------------------------------------------------------------
    ReadSectionValues(SECTION_ZONES, fZones, []);

    with fZones do
     if Count = 0 then
      begin // Por defecto
       AddPair( '0', 'Argentina');       AddPair( '1', 'Brasil');
       AddPair( '2', 'China');           AddPair( '3', 'EEUU');
       AddPair( '4', 'Europa');
      end;

    fZones.CustomSort(@CompareStrings); // Ordenar alfabeticamente
   end;
end;

// *****************************************************************************
// * GRABAR                                                                    *
// *****************************************************************************

procedure TConfig._Save;
var
  I : Integer;
begin
  with fINI do
   begin
    // Archivos ================================================================
    WriteBool(SECTION_DATA, DATA_BACKUP, fBackup);
    WriteString (SECTION_DATA, DATA_ASSETS_FILE, fAssetsDataFile);
    WriteString (SECTION_DATA, DATA_HISTORIC_FILE, fHistoricDataFile);

    // Heart Beat ==============================================================
    WriteInteger(SECTION_HB, HB_SENSIBILITY, fHBSensibilityDefault);

    // Categorias ==============================================================
    with fCategories do for I := 0 to Count - 1 do
      WriteString(SECTION_ASSETS_CATEGORIES, Names[I], Values[Names[I]]);
   end;
end;

procedure TConfig._SetHBSensibilityDefault(aValue: Integer);
begin
  aValue := abs(aValue);
  if not(fHBSensibilityDefault = aValue) then
   begin
    if not ((aValue >= 1) and (aValue <= 10)) then aValue := HB_SENSIBILITY_DEFAULT;
    fHBSensibilityDefault := aValue;
   end;
end;

// Preguntas ====================================================
function TConfig._GetAskQuestion(aID :String) : Boolean;
begin
  if not IsEmptyString(aID) then
   Result := fINI.ReadBool(SECTION_QUESTIONS, aID, True)
  else
   Result := True;
end;

procedure TConfig._SetAskQuestion(aID :String; aValue : Boolean);
begin
  if not IsEmptyString(aID) then fINI.WriteBool(SECTION_QUESTIONS, aID, aValue);
end;

function TConfig._GetGridColumnState(aID : String): String;
begin
  Result := Trim(fINI.ReadString(SECTION_ASSETS_GRID, aID, ''));
end;

procedure TConfig._SetGridColumnState(aID : String; aValue: String);
begin
  fINI.WriteString(SECTION_ASSETS_GRID, aID, Trim(aValue));
end;

initialization
  Configuration := TConfig.Create;

finalization
  Configuration.Free;

end.

