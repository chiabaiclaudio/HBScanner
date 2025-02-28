unit Page_Assets;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, Forms, Controls, Graphics, Dialogs,
  DBGrids, Grids, Menus, ComCtrls, ExtCtrls, StdCtrls, DBCtrls, Types,
  Thread_Update, Tab_Filter, tab_AssetData;

type

  { TpgAssets }

  TpgAssets = class(TForm)
    dbScoreExplained: TDBMemo;
    dbNav: TDBNavigator;
    dbScoreBull: TDBText;
    dbScoreBull1: TDBText;
    dbScoreBull2: TDBText;
    dbScoreBull3: TDBText;
    dbDescription: TDBText;
    dbtxtTYPE: TDBText;
    grdAssets: TDBGrid;
    dsGrid: TDataSource;
    imgLED: TImage;
    imgsLED: TImageList;
    lblRecordCount: TLabel;
    lblStsMessage: TLabel;
    LED: TImage;
    Panel1: TPanel;
    pmTitlesScore: TMenuItem;
    pnlAssetData: TPageControl;
    pmTitlesMarks: TMenuItem;
    pmViewData: TMenuItem;
    pmViewFilter: TMenuItem;
    pmZones: TMenuItem;
    pmCopyMSymbolD: TMenuItem;
    pmCopySymbol: TMenuItem;
    pmTitlesUpdate: TMenuItem;
    pmTitlesActiveColumn: TMenuItem;
    pmTitlesAsset: TMenuItem;
    pmTitlesPrices: TMenuItem;
    pmTitlesVolume: TMenuItem;
    pmTitlesHB: TMenuItem;
    pmTitlesDerivatives: TMenuItem;
    pnlFilter: TPanel;
    pnlData: TPanel;
    pmFavorite: TMenuItem;
    pmUpdate: TMenuItem;
    pmCategories: TMenuItem;
    pmBlackList: TMenuItem;
    pmCopyMSymbol: TMenuItem;
    pmDelete: TMenuItem;
    pmTitles: TPopupMenu;
    pmGrid: TPopupMenu;
    pnlStsBar: TPanel;
    pmTitlesTopSeparator: TMenuItem;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    dlgDeleteQuestion: TTaskDialog;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    pmTitlesBottomSep: TMenuItem;
    tbData: TTabSheet;
    tbPuntaje: TTabSheet;
    tmrDoneMsg: TTimer;
    procedure dsGridDataChange(Sender: TObject; Field: TField);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure grdAssetsContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure grdAssetsDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure grdAssetsDrawColumnTitle(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure grdAssetsGetCellHint(Sender: TObject; Column: TColumn;
      var AText: String);
    procedure grdAssetsKeyPress(Sender: TObject; var Key: char);
    procedure grdAssetsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure grdAssetsTitleClick(Column: TColumn);
    procedure imgLEDDblClick(Sender: TObject);
    procedure pmCopyMSymbolDClick(Sender: TObject);
    procedure pmCopySymbolClick(Sender: TObject);
    procedure pmViewDataClick(Sender: TObject);
    procedure pmViewFilterClick(Sender: TObject);
    procedure pmFavoriteClick(Sender: TObject);
    procedure pmBlackListClick(Sender: TObject);
    procedure pmCopyMSymbolClick(Sender: TObject);
    procedure pmDeleteClick(Sender: TObject);
    procedure pmTitlesPopup(Sender: TObject);
    procedure pmGridPopup(Sender: TObject);
    procedure tmrDoneMsgTimer(Sender: TObject);
  private
    fColumnCliked : Integer;
    fColumnSorted : String;
    fColumnSortedASC : Boolean;
    fColumnSortedIndex : Integer;

    fActualBookmark : TBookMark;

    fCopyText : String;

    fUpdating : Boolean;
    fUpdateThread : TThreadUpdate;
    fUpdateTimeStart : TDateTime;
    fUpdateFinish : TDateTime;

    fTabFilter : TtabFilter;
    fTabAssetData : TtabAssetData;

    // Inicializar ==========================================
    procedure _InitGridAssets;
    procedure _InitPanels;
    procedure _InitTitlesMenu;

    // Utilitarios ==========================================
    procedure _CopySymbols(aMarket: Boolean = False; aDescription: Boolean = False );

    // Columnas =============================================
    procedure _ToggleColumnVisibility(Sender: TObject);

    // Registros seleccionados ==============================
    function _DataSet : TDataSet;

    procedure _GetCursorPosition;
    procedure _RestoreCursorPosition;

    function _IsGroupSelected : Boolean;

    // Marcadores ===========================================
    procedure _CreateSubMenues(aMenu : TMenuItem; aFieldName: String; aEventClick: TNotifyEvent; aList: TStringList);

    procedure pmCategoryClick(Sender: TObject);
    procedure pmZoneClick(Sender: TObject);

    // Dibujar celdas =======================================
    procedure _DrawCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);

    procedure _DrawCellCHANGE(Column: TColumn; IsBlackListed: Boolean);
    procedure _DrawCellPORTFOLIO(aRect : TRect);
    procedure _DrawCellFAVORITE(aRect: TRect);
    procedure _DrawCellBLACKLIST(aRect : TRect);
    procedure _DrawCellSYMBOL(aRect : TRect);
    procedure _DrawCellHB(Column: TColumn; aRect : TRect; DataCol: Integer; aState: TGridDrawState);
    procedure _DrawCellVOLUME(Column: TColumn; aRect : TRect);

    // ACTUALZIACION =======================================
    procedure _UpdateKill;

    procedure _OnUpdateProgress(const aMsg : String);
    procedure _OnUpdateComplete(Sender: TObject);
    procedure _OnTerminate(Sender: TObject);

    procedure _setLED (aOn : Boolean = True);

  public
    property  Updating : Boolean read fUpdating;
    procedure UpdateStart (aFiltered : Boolean = False);
    procedure UpdateStop (Ask : Boolean = False);
  end;


implementation

uses Config, Tools, Data_Assets, Math, Clipbrd, Market, LCLType, DateUtils, Data_Assets_Const;

{$R *.lfm}


Type
  MyDBGridHack = class(TDBGrid); { #todo : Reemplazar por algun metodo standard }

{ TpgAssets }

procedure TpgAssets.FormCreate(Sender: TObject);
begin
  fUpdating := False;
  fColumnSortedIndex := 0; // Inicalizar para evitar posibles problemas
  { #todo : Resolver problema de cuenta sin filtar. }
  lblRecordCount.Caption := IntToStr(DataAssets.tblData.RecordCount) + ' activos registrados';

  _InitPanels;
  _InitGridAssets;
  _InitTitlesMenu;
end;

procedure TpgAssets.FormDestroy(Sender: TObject);
var
  I: Integer;
  aValue : String;
begin
  // Guardar el estado de cada columna
  with grdAssets.Columns do
   for I := 0 to Count - 1 do
    begin
     aValue := IntToStr(Items[I].Index) + ',' + BoolToStr(Items[I].Visible, True);
     Configuration.GridColumnState[Items[I].FieldName] := aValue;
    end;
end;


////////////////////////////////////////////////////////////////////////////////
// INICIALIZACION
//

procedure TpgAssets._InitPanels;
begin
  // Filtros ---------------------------
  with TtabFilter.Create(Self) do
   begin
    Parent := pnlFilter;
    BorderStyle := bsNone;
    Align := alClient;
    Show;
   end;

//  fTabAssetData := TtabAssetData.Create(Self);
//  addTab(fTabAssetData);
end;

procedure TpgAssets._InitGridAssets;
var
  i : Integer;
  Col : TColumn;
  ColState : String;
  CommaPos : Integer;
  TempIndex : TStringList;
  NewIndex : Integer;
begin
  // Columnas ============================================
  TempIndex := TStringList.Create;

  // Primera pasada: Aplicar visibilidad y almacenar índices temporalmente
  for i := 0 to grdAssets.Columns.Count - 1 do
   begin
    Col := grdAssets.Columns[i];
    ColState := Configuration.GridColumnState[Col.FieldName]; // Leer estado

    if not IsEmptyString(ColState) then
     begin
      CommaPos := Pos(',', ColState);

      if CommaPos > 0 then
       begin
        NewIndex := StrToIntDef(Copy(ColState, 1, CommaPos - 1), -1);
        Col.Visible := StrToBoolDef(Copy(ColState, CommaPos + 1, Length(ColState) - CommaPos), True);

        // Si el índice es válido, se reordena después de que termine el bucle
        with TempIndex do if not(NewIndex = -1) then
          AddPair(Col.FieldName, IntToStr(NewIndex)) // Usamos Col.Tag como índice temporal
         else
          AddPair(Col.FieldName, IntToStr(Col.Index)); // Dejar el índice actual si no hay cambio
       end
     end;
   end;

  // Ordenar las columnas según los valores de `Tag`
  for i := 0 to TempIndex.Count - 1 do
   begin
    Col := grdAssets.Columns.ColumnByFieldname(TempIndex.Names[i]);
    if not (Col = nil) then // Existe la columna?
     begin
      NewIndex := StrToIntDef(TempIndex.ValueFromIndex[i], -1);
      if not (NewIndex = -1) then Col.Index := NewIndex; // Cambiar la posición.
     end;
   end;

  // Inicializar mensajes ==============================
  lblStsMessage.Caption := '';
  _setLED(False);

  // INICIALIZAR ORDEN ======================================================
  dsGrid.Enabled := True; // Asegurar origen de datos activos

  // ... clikeando en la comlumna del Simbolo { #todo : Posible guardado de la columna de orden }
  DataAssets.tblData.First;
  grdAssetsTitleClick(grdAssets.Columns.ColumnByFieldName(FLD_ASSET_SYMBOL));
end;

procedure TpgAssets._InitTitlesMenu;
var
  i : Integer;
  FatherItem : TMenuItem;
  NewItem : TMenuItem;

  function VerifyPrefix(aText : String; aPrefix : String) : Boolean;
  begin
    Result := UpperCase(LeftStr(aText, Length(aPrefix))) = UpperCase(aPrefix);
  end;

begin
  // Pesos y orden (por 1000) ---------------------
  with pmTitlesActiveColumn do
   begin
    OnClick := @_ToggleColumnVisibility;
    Tag := -1; // Valor para que indica no mover o tocar
   end;

  // Crear menues y ordenar ------------------------
  for i := 0 to grdAssets.Columns.Count - 1 do
   with grdAssets do
    if not(Columns[i].FieldName = FLD_ASSET_SYMBOL) and
       not(Columns[i].FieldName = FLD_PRICE_CLOSE ) then // Evitar que simbolo y precio desaparezcan
     begin
      NewItem := TMenuItem.Create(pmTitles);
      with NewItem do
       begin
        Caption   := Columns[i].Title.Caption;
        Checked   := Columns[i].Visible;
        AutoCheck := True; ShowAlwaysCheckable:= True;
        Tag := i; // Usar Tag para almacenar el índice de la columna
        OnClick := @_ToggleColumnVisibility;
       end;

      // Ubicar el nuevo item ------------------------------
      FatherItem := nil;

      if VerifyPrefix (Columns[i].FieldName, GRP_ASSET)  then FatherItem := pmTitlesAsset;
      if VerifyPrefix (Columns[i].FieldName, GRP_MARK)   then FatherItem := pmTitlesMarks;
      if VerifyPrefix (Columns[i].FieldName, GRP_PRICE)  then FatherItem := pmTitlesPrices;
      if VerifyPrefix (Columns[i].FieldName, GRP_VOLUME) then FatherItem := pmTitlesVolume;
      if VerifyPrefix (Columns[i].FieldName, GRP_HB)     then FatherItem := pmTitlesHB;
      if VerifyPrefix (Columns[i].FieldName, GRP_DERIV)  then FatherItem := pmTitlesDerivatives;
      if VerifyPrefix (Columns[i].FieldName, GRP_UPDATE) then FatherItem := pmTitlesUpdate;
      if VerifyPrefix (Columns[i].FieldName, GRP_SCORE)  then FatherItem := pmTitlesScore;

      // Crealos y ubicarlo
      if not (FatherItem = nil) then
        FatherItem.Add(NewItem)
       else pmTitles.Items.Add(NewItem);
     end;
end;

// *****************************************************************************
// * GRILLA - TITULOS                                                          *
// *****************************************************************************

// Panel de datos =============================================================
procedure TpgAssets.dsGridDataChange(Sender: TObject; Field: TField);
begin
  tbData.Caption := _DataSet.FieldByName(FLD_ASSET_SYMBOL).AsString;
end;


// DIBUJAR =====================================================================
procedure TpgAssets.grdAssetsDrawColumnTitle(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  X, Y, TextWidth, TextHeight, TotalWidth: Integer;
  Triangle: array[0..2] of TPoint;
  TitleRect: TRect;
begin
  with grdAssets do
   if (gdFixed in State) and (Column.FieldName = fColumnSorted) then // Si es una celda de título
    begin
     TitleRect := CellRect(DataCol, 0);
     Canvas.FillRect(TitleRect); // Limpia el área del título

     TextWidth  := Canvas.TextWidth(Column.Title.Caption);
     TextHeight := Canvas.TextHeight(Column.Title.Caption);
     TotalWidth := TextWidth + 10 + 10; // Ancho del texto + espacio + ancho del triángulo

     // Calcula la posición X y Y para centrar el texto
     X := TitleRect.Left + (TitleRect.Right  - TitleRect.Left - TotalWidth) div 2;
     Y := TitleRect.Top  + (TitleRect.Bottom - TitleRect.Top  - TextHeight) div 2;

     // Dibuja el texto del título
     Canvas.TextOut(X, Y, Column.Title.Caption);

     // Calcula la posición X y Y para centrar el triángulo
     X := X + TextWidth + 10;
     Y := TitleRect.Top + (TitleRect.Bottom - TitleRect.Top) div 2;

     Canvas.Pen.Color := clBlack;
     Canvas.Brush.Color := clBlack;

     if fColumnSortedASC then
      begin
       // Dibujar triángulo ascendente
       Triangle[0] := Point(X, Y - 5);
       Triangle[1] := Point(X - 5, Y + 5);
       Triangle[2] := Point(X + 5, Y + 5);
      end
     else
      begin
       // Dibujar triángulo descendente
       Triangle[0] := Point(X, Y + 5);
       Triangle[1] := Point(X - 5, Y - 5);
       Triangle[2] := Point(X + 5, Y - 5);
      end;

     Canvas.Polygon(Triangle);
    end;
end;

// PopUp Menu ==================================================================

// PopUp Menu ==================================================================
procedure TpgAssets.pmTitlesPopup(Sender: TObject);
var
  Coord: TGridCoord;
  i: Integer;
  FoundItem: TMenuItem;

  function SearchMenuByTag(aMenu: TMenuItem; IDTag: Integer): TMenuItem;
  var
    i: Integer;
  begin
    Result := nil;

    // Recorrer los submenús en busca del tag
    for i := 0 to aMenu.Count - 1 do
     begin
      if aMenu.Items[i].Tag = IDTag then
       begin
        Result := aMenu.Items[i];
        Exit;
       end
      else
       begin
        Result := SearchMenuByTag(aMenu.Items[i], IDTag);
        if Result <> nil then Exit;
       end;
     end;
  end;

  // Función recursiva para recorrer submenús
  procedure UpdateMenuItemChecked(aItem: TMenuItem);
  var
    j : Integer;
  begin
    // Si el MenuItem tiene un Tag que coincide con una columna, actualizar su estado
    if (aItem.Tag >= 0) and (aItem.Tag < grdAssets.Columns.Count) then
      aItem.Checked := grdAssets.Columns[aItem.Tag].Visible;

    // Recorrer submenús si los tiene
    for j := 0 to aItem.Count - 1 do
      UpdateMenuItemChecked(aItem.Items[j]);
  end;

begin

  // Modificar el menu de la columna actual ---------------------------
  with grdAssets do Coord := MouseToCell(ScreenToClient(Mouse.CursorPos));

  if Coord.X >= 0 then
   begin
    FoundItem := SearchMenuByTag(pmTitles.Items, Coord.X);

    with pmTitlesActiveColumn do
     begin
      Enabled := not (FoundItem = nil);

      if Enabled then
       begin
        Caption := FoundItem.Caption;
        Tag := Coord.X;
       end
      else  Caption := '...';
     end;
  end;

  // Función recursiva para recorrer submenús
  for i := 0 to pmTitles.Items.Count - 1 do
  UpdateMenuItemChecked(pmTitles.Items[i]);
end;

procedure TpgAssets._ToggleColumnVisibility(Sender: TObject);
var
  ColIndex: Integer;
begin
  ColIndex := (Sender as TMenuItem).Tag;
  with grdAssets.Columns[ColIndex] do Visible := not Visible;
end;

// CAMBIAR ORDEN ===============================================================
procedure TpgAssets.grdAssetsTitleClick(Column: TColumn);
begin
  if Assigned(Column) then
   begin
    fColumnSorted      := Column.FieldName;
    fColumnSortedASC   := DataAssets.SetOrder(fColumnSorted);

    with grdAssets.Columns[fColumnSortedIndex].Title.Font do Style := Style - [fsBold];
    fColumnSortedIndex := Column.Index;
    with Column.Title.Font do Style := Style + [fsBold];

    // Redibujar el grid para mostrar el triángulo de ordenación
    with grdAssets do begin Repaint; Invalidate; end;
   end;
end;

procedure TpgAssets.imgLEDDblClick(Sender: TObject);
begin
  UpdateStop(False);
end;

// *****************************************************************************
// * CELDAS - GRILLA                                                           *
// *****************************************************************************

// Dibujar ---------------------------------------------------------------------
procedure TpgAssets.grdAssetsDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  with grdAssets.Canvas do
   if not _DataSet.IsEmpty then
    _DrawCell(Rect, DataCol, Column, State)
   else grdAssets.Canvas.FillRect(Rect); // Rellenar vacio
end;

procedure TpgAssets._DrawCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  DrawDefault : Boolean;
  IsSelected     : Boolean;
  IsBlackListed  : Boolean;
  IsBooleanValue : Boolean;
begin
  // Estado -------------------------------------------------
  IsSelected    := (gdSelected in State); // Seleccionado
  IsBlackListed := _Dataset.FieldByName(FLD_MARK_BLACKLIST).AsBoolean; // En Lista Negra

  // Preparar canvas =========================================================
  with grdAssets.Canvas do
   begin
    Pen.Color := IfThen(IsSelected, clHighlightText, clWindowText);

    // Setear fuente para campos de porcentaje -----------------------------
    if not IsSelected then
     begin
      if not (Pos('CHANGE', Column.FieldName) = 0)  and not IsBlackListed then
       begin
        _DrawCellCHANGE(Column, IsBlackListed);
        //with Font do Style := Style + [fsBold];
       end;
     end
    else with Font do Style := Style + [fsBold];

    if IsSelected then Font.Color := clWhite;

    // Fondo ---------------------------------------------------------------
    if IsBlackListed then
     begin // [ fsStrikeOut, fsItalic ]
      if not (Column.FieldName = FLD_ASSET_SYMBOL) then Font.Color := clMedGray;
      if not IsSelected then Brush.Color := TColor($DCDCDC); // Gris claro
     end;

    FillRect(Rect); // Cubrir fondo
   end;

  // Dibujar contenido  =====================================================
  DrawDefault := True;

  with Column.Field do
   if DataType = ftBoolean then
    IsBooleanValue := Column.Field.AsBoolean
   else IsBooleanValue := False;

  case Column.FieldName of
   FLD_ASSET_SYMBOL  : ////////////////////////////////////////////////
     if not IsSelected then
       _DrawCellSYMBOL(Rect);
     //with grdAssets.Canvas.Font do Style := Style + [fsItalic];

   FLD_MARK_PORTFOLIO : //////////////////////////////////////////////
     if IsBooleanValue then
      begin
       _DrawCellPORTFOLIO(Rect);
       DrawDefault := False;
      end;

   FLD_MARK_FAVORITE : ///////////////////////////////////////////////
     if IsBooleanValue then
      begin
       _DrawCellFAVORITE(Rect);
       DrawDefault := False;
      end;

   FLD_MARK_BLACKLIST : //////////////////////////////////////////////
     if IsBooleanValue then
      begin
       _DrawCellBLACKLIST(Rect);
       DrawDefault := False;
      end;

   FLD_HB_FORCE, FLD_HB_VOLUME, ///////////////////////////////////////////////
   FLD_DERIV_SMA, FLD_DERIV_EMA, FLD_DERIV_WMA, FLD_DERIV_SWMA  :
     if not IsSelected and not IsBlackListed then
      begin
       _DrawCellHB(Column, Rect, DataCol, State);
       DrawDefault := False;
      end;

    FLD_VOLUME, FLD_VOLUME_LAST: ////////////////////////////////
      begin
       _DrawCellVOLUME(Column, Rect);
       DrawDefault := False;
      end;
   end;

  // Dibujar por defecto -----------------------------------------
  if DrawDefault then grdAssets.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

// Cambiar color de la fuente -----------------------------------------------
procedure TpgAssets._DrawCellCHANGE(Column: TColumn; IsBlackListed: Boolean);
begin
  with grdAssets.Canvas.Font do
   if Column.Field.AsFloat = 0 then
    begin
     Color := clWindowText;
     Style := Style - [fsBold];
    end
   else // Color negativo y positivo
    Color := IfThen(Column.Field.AsFloat > 0, clGreen, clMaroon);
end;

// En Cartera --------------------------------------
procedure TpgAssets._DrawCellPORTFOLIO(aRect: TRect);
var
  XEnd : Integer;
begin
  with aRect do
   begin
    XEnd := Left + (Right - Left) div 2; // Calcula la posición media
    with grdAssets.Canvas do
     begin
      Brush.Color := Pen.Color;
      Polygon([
        Point( XEnd,      Top + 5 ),    Point( XEnd + 10, Top + 5),
        Point( XEnd + 10, Bottom - 5),  Point( XEnd + 5,  Bottom - 10),
        Point( XEnd,      Bottom - 5 ), Point( XEnd,      Top + 5)    ] );
     end;
   end;
end;

// Favorito ---------------------------------------
procedure TpgAssets._DrawCellFAVORITE(aRect: TRect);
var
  CenterX : Integer;
begin
  with aRect do
   begin
    CenterX := (Left + Right)  div 2;

    with grdAssets.Canvas do
     begin
      Brush.Color := Pen.Color;
      Ellipse(CenterX - 9, Top + 3, CenterX,     Top + 12);
      Ellipse(CenterX,     Top + 3, CenterX + 9, Top + 12);

      Polygon([
        Point( CenterX - 10, Top + 7 ),
        Point( CenterX + 10, Top + 7 ),
        Point( CenterX,      Top + 20),
        Point( CenterX - 10, Top + 7 ) ]);
     end;
  end;
end;

// Prohibido ---------------------------------------
procedure TpgAssets._DrawCellBLACKLIST(aRect: TRect);
var
  Radius, DiagonalLength : Integer;

  CenterX, CenterY : Integer;
  StartX, StartY   : Integer;
  EndX, EndY       : Integer;
begin
  // Coordenadas del centro de la celda
  with aRect do
   begin
    CenterX := (Left + Right)  div 2;
    CenterY := (Top  + Bottom) div 2;

    // Radio del círculo
    Radius := Min((Right - Left), (Bottom - Top)) div 4;
   end;

  with grdAssets.Canvas do
   begin
    // Dibuja el círculo negro =======================================
    Pen.Width := 3; // Especifica el grosor de la línea
    Arc(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius, 0, 360 * 18);

    // Calcula las coordenadas de intersección de la línea diagonal con el círculo
    DiagonalLength := Round(Radius * Sqrt(2)); // Longitud de la mitad de la diagonal del círculo
    StartX := CenterX - DiagonalLength div 2;
    StartY := CenterY - DiagonalLength div 2;
    EndX   := CenterX + DiagonalLength div 2;
    EndY   := CenterY + DiagonalLength div 2;

    // Dibuja la línea diagonal de prohibición dentro del círculo
    MoveTo(StartX, StartY);
    LineTo(EndX, EndY);
    // ===============================================================
   end;
end;

procedure TpgAssets._DrawCellSYMBOL(aRect : TRect);
begin
  // Coordenadas del centro de la celda
  with grdAssets.Canvas, aRect do
   begin
    Brush.Color := Pen.Color;

    if _Dataset.FieldByName(FLD_MARK_PORTFOLIO).AsBoolean then
     begin
      // Color para la mitad inferior derecha
      Brush.Color := TColor($DDDDDD);
      Pen.Color := Brush.Color;
      Polygon([Point(Right, Top), Point(Right, Bottom), Point(Left, Bottom)]);
     end;

    if _DataSet.FieldByName(FLD_MARK_FAVORITE).AsBoolean then
     begin
      Brush.Color := TColor($CCCEFF);
      Pen.Color := Brush.Color;
      Polygon([Point(Left, Top), Point(Right, Top), Point(Left, Bottom)]);
     end;

    // Marca de nota ----------------------------------
    if not IsEmptyString( _DataSet.FieldByName(FLD_ASSET_NOTE).AsString) then
       Polygon([Point(aRect.Right, aRect.Top),
           Point(aRect.Right - 10, aRect.Top),
           Point(aRect.Right, aRect.Top + 10)]);
   end;
end;

// Valor Heart Beat ----------------------------------------------------------
procedure TpgAssets._DrawCellHB(Column: TColumn; aRect : TRect; DataCol: Integer; aState: TGridDrawState);
const
  clBearZone    = TColor($0000AA);  clBullZone    = TColor($00AA00);
  clBearConfirm = TColor($0000C8);  clBullConfirm = TColor($00C800);
  clBear        = TColor($FFBEFA);  clBull        = TColor($90EE90);
  clBearish     = TColor($C2C2FF);  clBullish     = TColor($C0DCC0);

  clZero  = TColor($F5F5F5);

var
  HBValue  : Integer;
  aColor   : TColor;

  function GetRangePoint(aValue, aMax : Integer) : Integer;
  begin
    Result := Trunc ( Abs( specialize PercentChange<Integer> ( AValue, AMax )));
  end;

  function GetGradientColor(aOrigin, aDestiny: TColor; aRange: Real): TColor;
  var
    R1, G1, B1: Byte; // Componentes RGB de aOrigin
    R2, G2, B2: Byte; // Componentes RGB de aDestiny
    R, G, B: Byte;    // Componentes RGB del color resultante
    Ratio: Double;    // Proporción calculada
  begin
    // Asegúrate de que el rango está entre 0 y 100
    aRange := CheckRange(0, 100, Abs(aRange));

    // Convierte el rango a una proporción entre 0 y 1
    Ratio := aRange; // / 100;

    // Extrae los componentes RGB de los colores
    R1 := Red(ColorToRGB(aOrigin));
    G1 := Green(ColorToRGB(aOrigin));
    B1 := Blue(ColorToRGB(aOrigin));

    R2 := Red(ColorToRGB(aDestiny));
    G2 := Green(ColorToRGB(aDestiny));
    B2 := Blue(ColorToRGB(aDestiny));

    // Interpola los valores RGB
    R := Round(R1 + (R2 - R1) * Ratio);
    G := Round(G1 + (G2 - G1) * Ratio);
    B := Round(B1 + (B2 - B1) * Ratio);

    // Devuelve el color resultante
    Result := RGBToColor(R, G, B);
  end;

begin
  HBValue := Trunc(Column.Field.AsFloat);

  { #todo : Modificar por constantes. }
  case HBValue of
   -300 .. -151 : aColor := GetGradientColor(clBearish, clBearConfirm, (HBValue + 150) / 150);
   -150 ..  -76 : aColor := GetGradientColor(clBear,    clBearish,     (HBValue + 75)  /  75);
    -75 ..   -1 : aColor := GetGradientColor(clZero,    clBear,        (HBValue        /  75));
      0         : aColor := clZero;
      1 ..   75 : aColor := GetGradientColor(clZero,    clBullish,     (HBValue        /  75));
     76 ..  150 : aColor := GetGradientColor(clBullish, clBull,        (HBValue - 75)  /  75);
    151 ..  300 : aColor := GetGradientColor(clBull,    clBullConfirm, (HBValue - 150) / 150);
  else
   if (Abs(HBValue) > 300) then acolor := IfThen(HBValue < 0, clBearZone, clBullZone);

   //if (HBValue <  HB_OFF_LIMITS) and (HBValue > 0) then
   // Fuera de límites -----------------------------
   // if Abs(HBValue) > HB_OFF_LIMITS then aColor := clLtGray;
  end;

  // Por defecto? ---------------------------------
  with grdAssets.Canvas do
   begin
    Brush.color := aColor;
    FillRect(aRect);

    // Si la luminosidad es alta, el fondo es claro (elegimos color oscuro para el texto)
    Font.Color := IfThen(GetLuminanceFromColor(aColor) < 0.35, TColor($EFEFEF), TColor($202020));

    TextRect(aRect, 0, 0, IntToStr(HBValue));
   end;
end;

procedure TpgAssets._DrawCellVOLUME(Column: TColumn; aRect : TRect);
const
  VALUE_SPACE_UNIT = 4;
var
  Value, UnitText: string;
  DelimiterPos: Integer;

  TotalWidth, ValueWidth, UnitWidth, StartX: Integer;
begin
  // Inicilizar --------------
  Value := Column.Field.DisplayText; // Obtener el valor del campo
  UnitText := '';

  // Divide el valor y la unidad si hay un espacio
  DelimiterPos := Pos(' ', Value);
  if DelimiterPos > 0 then
   begin
    UnitText := Copy(Value, DelimiterPos + 1, Length(Value) - DelimiterPos);
    Value := Copy(Value, 1, DelimiterPos - 1);
   end;

  with grdAssets.Canvas do
   begin
    FillRect(aRect);

    // Medir segun el estilo ------------------
    ValueWidth := TextWidth(Value);
    with Font do Style := Style + [fsItalic];
    UnitWidth := TextWidth(UnitText);

    // Calcular ----------------------
    TotalWidth := ValueWidth + VALUE_SPACE_UNIT + UnitWidth; // Espacio entre número y unidad
    StartX := aRect.Left + (aRect.Width - TotalWidth) div 2;

    // Dibuja el valor (número) -----------------
    with Font do Style := Style - [fsItalic];
    TextOut(StartX, aRect.Top + (aRect.Height - TextHeight(Value)) div 2, Value);

    // Dibuja la unidad (en itálicas)
    if not IsEmptyString(UnitText) then
     begin
      with Font do Style := Style + [fsItalic];
      TextOut(StartX + VALUE_SPACE_UNIT + ValueWidth, aRect.Top + (aRect.Height - TextHeight(UnitText)) div 2, UnitText);
     end;
   end;
end;

// HINT's ======================================================================
procedure TpgAssets.grdAssetsGetCellHint(Sender: TObject; Column: TColumn;
  var AText: String);
var
  Favorite, Portfolio : boolean;
begin
  with grdAssets.Columns do
   if Assigned(Column.Field) then
    case Column.FieldName of
     FLD_ASSET_SYMBOL   :
       begin
        aText := '';
        if not(ColumnByFieldName(FLD_ASSET_DESCRIPTION).Visible) then
          aText := ColumnByFieldName(FLD_ASSET_DESCRIPTION).Field.AsString + LineEnding;

        Portfolio := ColumnByFieldName(FLD_MARK_PORTFOLIO).Field.AsBoolean;
        Favorite := ColumnByFieldName(FLD_MARK_FAVORITE).Field.AsBoolean;

        if Portfolio then aText := aText + '[ En cartera ]';
        if Portfolio and Favorite then aText := aText + ' ';
        if Favorite  then aText := aText + '[ Favorito ]';
       end;

     FLD_ASSET_TYPE     :
       aText := AssetTypeToStr(TAssetType(Column.Field.AsInteger));
     FLD_ASSET_CURRENCY :
       aText := CurrencyTypeToStr(TCurrencyType(Column.Field.AsInteger));
     FLD_PRICE_CLOSE_LAST, FLD_PRICE_CLOSE, FLD_PRICE_OPEN, FLD_PRICE_HIGH, FLD_PRICE_LOW : // Mostrar moneda/divisa
       if not(ColumnByFieldname(FLD_ASSET_CURRENCY).Visible) then
        aText := CurrencyTypeToStr(TCurrencyType(ColumnByFieldname(FLD_ASSET_CURRENCY).Field.AsInteger));
     FLD_VOLUME, FLD_VOLUME_LAST, FLD_VOLUME_ORDER,  FLD_VOLUME_ORDER_LAST: // Mostrar moneda/divisa
       aText := IfThenStr(Column.Field.AsInteger = 0, 'Sin movimiento', Column.Field.AsString);
   end;
end;


// *****************************************************************************
// * MENU - GRILLA                                                             *
// *****************************************************************************

// REQUEST POPUP ===============================================================
procedure TpgAssets.grdAssetsContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  GlobalPos: TPoint;
  GridCoord: TGridCoord;
  Step : Integer;
begin
  GlobalPos := (Sender as TControl).ClientToScreen(MousePos);
  with MousePos do GridCoord := grdAssets.MouseCoord(X, Y);

  // En el título? ---------
  with GlobalPos do
   if GridCoord.Y = 0 then
    begin
     fColumnCliked := GridCoord.X; // Columna clickeada
     pmTitles.PopUp(X, Y);
    end
   else
    begin
     { #todo : Se podrá eliminar el hack? }
     Step := GridCoord.Y - MyDBGridHack(grdAssets).Row; // RowClicked - ActualRow
     if not(GridCoord.X = -1) then
      begin
       _DataSet.MoveBy(Step); // GridsCoord devuelve en base cero.
       pmGrid.PopUp(X,Y);
      end;
    end;
end;

// UTILITARIOS =================================================================
procedure TpgAssets._CopySymbols(aMarket: Boolean = False; aDescription: Boolean = False );

  function _textCopied (aMarket : Boolean = False; aDescription : Boolean = False) : String;
  begin
   Result := '';
   with _DataSet do
    begin
     if aMarket then Result := FieldByName(FLD_ASSET_MARKET).AsString + ':';
     Result := Result + FieldByName(FLD_ASSET_SYMBOL).AsString;
     if aDescription then Result := Result + ' [' + FieldByName(FLD_ASSET_DESCRIPTION).AsString + ']';
    end;
  end;

var
  BM : TBookMark;
begin
   if _IsGroupSelected then
    begin
     _GetCursorPosition;
     fCopyText := '';
     for BM in grdAssets.SelectedRows do
      begin
       _DataSet.Bookmark := BM;
       fCopyText := fCopyText + _textCopied(aMarket, aDescription) + LineEnding;
      end;
     _RestoreCursorPosition;
    end
   else fCopyText := _textCopied(aMarket, aDescription);
end;

function TpgAssets._DataSet : TDataSet;
begin
  Result := dsGrid.DataSet;
end;

procedure TpgAssets._GetCursorPosition;
begin
  fActualBookmark := _DataSet.Bookmark;
end;

procedure TpgAssets._RestoreCursorPosition;
begin
  with _DataSet do
   if not (fActualBookMark = nil) then
    begin
     try
      Bookmark := fActualBookmark;
     finally
      fActualBookmark := nil;
     end;
    end;
end;

function TpgAssets._IsGroupSelected : Boolean;
begin
  Result := grdAssets.SelectedRows.Count > 0;
end;

// =============================================================================
// == POPUP
// =============================================================================

procedure TpgAssets._CreateSubMenues(aMenu : TMenuItem; aFieldName: String; aEventClick: TNotifyEvent; aList: TStringList);
var
  i : Integer;
  NewMenuItem : TMenuItem;
begin
  with aList do
   for i := 0 to Count - 1 do   // Crear un menu por categoría ---------------------
    begin
     NewMenuItem := TMenuItem.Create(aMenu);
     with NewMenuItem do
      begin
       Name := aMenu.Name + IntTostr(i + 1);
       Tag := StrToIntDef(Names[I], -1);
       Caption := Values[Names[I]];
       Checked := (Tag = _DataSet.FieldByName(aFieldName).AsInteger) and not _IsGroupSelected;
       OnClick := aEventClick;
       aMenu.Add(NewMenuItem);

       // Está seleccionado? Mover arriba y separar ---------------------
       if Checked then
        begin
         NewMenuItem.MenuIndex := 0;

         with aMenu do
          begin
           AddSeparator;
           Items[Count - 1].MenuIndex := 1; // Mover separador al segundo lugar
          end;
        end;
      end;
    end;
end;

procedure TpgAssets.pmGridPopup(Sender: TObject);
Var
  SelCount : Integer;
  strSelCount, Symbol : String;
  InPortfolio, Blacklisted, Favorite, Flag : Boolean;
begin
  { #todo : Desactivar si esta actualizando }
  // Cuenta de seleccionados ==============================
  Symbol := Trim(_DataSet.FieldByName(FLD_ASSET_SYMBOL).AsString);
  SelCount    := grdAssets.SelectedRows.Count;
  strSelCount := IntToStr(SelCount);

  with _DataSet do
   begin
    InPortfolio := FieldByName(FLD_MARK_PORTFOLIO).AsBoolean;
    Blacklisted := FieldByName(FLD_MARK_BLACKLIST).AsBoolean;
    Favorite    := FieldByName(FLD_MARK_FAVORITE).AsBoolean;
   end;

  // COPIAR ===============================================
  with pmCopySymbol do
   begin
    Enabled := not _DataSet.IsEmpty;
    Caption := '&Copiar ' + Pluralize(SelCount, strSelCount + ' ') + 'símbolo' + Pluralize(SelCount);
   end;

  with pmCopyMSymbol do
   begin
    Enabled := pmCopySymbol.Enabled;
    Caption := '&Copiar ' + Pluralize(SelCount, strSelCount + ' ') + 'merc.:símbolo' + Pluralize(SelCount);
   end;

  with pmCopyMSymbolD do
   begin
    Enabled := pmCopySymbol.Enabled;
    Caption := '&Copiar ' + Pluralize(SelCount, strSelCount + ' ') + 'merc.:símbolo [descrip.]' + Pluralize(SelCount);
   end;

  // EXCLUIDOS / FAVORITOS ============================================
  pmFavorite.Enabled  := pmCopySymbol.Enabled;

  if SelCount <= 1 then // Un solo registro seleccionado ................
   begin
    with pmBlacklist do
     begin
      Enabled := not ( InPortfolio or Favorite) and pmCopySymbol.Enabled;
      Checked := Blacklisted;
      Caption := IfThenStr(Checked, 'Quitar e&xclusión', 'E&xcluir');
     end;

    with pmFavorite do
     begin
      Checked := _DataSet.FieldByName(FLD_MARK_FAVORITE).AsBoolean;
      Caption := IfThenStr(Checked, 'Quitar de &favoritos', 'Marcar como &favorito');
     end;
   end
  else // Más de uno  ..................................................
   begin
    with pmBlacklist do
     begin
      Enabled := pmCopySymbol.Enabled;
      Checked := False;
      Caption := 'Cambiar e&xclusión en ' + strSelCount + ' activos';
     end;

    with pmFavorite do
     begin
      Enabled := pmCopySymbol.Enabled;
      Checked := False;
      Caption := 'Cambiar marca de &favorito ' + strSelCount + ' activos';
     end;
   end;

  // CATEGORIAS / ZONAS ================================================
  Flag := ( TAssetType(_DataSet.FieldByName(FLD_ASSET_TYPE).Asinteger) in AsseTypeCategorisable )
           and pmCopySymbol.Enabled;

  with pmCategories do
   begin
    Clear; Enabled := Flag and  not ( Configuration.Categories.Count = 0 );
    _CreateSubMenues(pmCategories, FLD_MARK_CATEGORY, @pmCategoryClick, Configuration.Categories);
   end;

  with pmZones do
   begin
    Clear; Enabled := Flag and not ( Configuration.Zones.Count = 0 );
    _CreateSubMenues(pmZones, FLD_MARK_ZONE, @pmZoneClick, Configuration.Zones);
   end;

  // BORRAR ===================================================
  with pmDelete do
   begin
    Caption := '&Borrar ' + Pluralize ( SelCount, strSelCount + ' activos', Symbol ) + ' ...';
    Enabled := pmCopyMSymbol.Enabled and not( Favorite or InPortfolio);
   end;

  // ACTUALIZAR ===============================================
  with pmUpdate do
   begin
    Caption := 'Actualizar ' + Pluralize( SelCount, 'activos' ) + ' ...';
    Enabled := pmCopyMSymbol.Enabled;
   end;

  // VER ======================================================
  with pmViewFilter do
   begin
    Checked := pnlFilter.Visible;
    Caption := IfThenStr(Checked, 'Ocultar', 'Mostrar') + ' &filtros';
   end;

  with pmViewData do
   begin
    Checked := pnlAssetData.Visible;
    Caption := IfThenStr(Checked, 'Ocultar', 'Mostrar') + ' &datos';
   end;
end;

// CATEGORIA ===================================================================
procedure TpgAssets.pmCategoryClick(Sender: TObject);
var
  BM : TBookMark;
begin
  if _IsGroupSelected then
   begin
    _GetCursorPosition;
    for BM in grdAssets.SelectedRows do
     begin
      _DataSet.Bookmark := BM;
      DataAssets.SetCategory(TMenuItem(Sender).Tag);
     end;
    _RestoreCursorPosition;
   end
  else
   DataAssets.SetCategory(TMenuItem(Sender).Tag);
end;

// ZONA ===================================================================
procedure TpgAssets.pmZoneClick(Sender: TObject);
var
  BM : TBookMark;
begin
  if _IsGroupSelected then
   begin
    _GetCursorPosition;
    for BM in grdAssets.SelectedRows do
     begin
      _DataSet.Bookmark := BM;
      DataAssets.SetZone(TMenuItem(Sender).Tag);
     end;
    _RestoreCursorPosition;
   end
  else
   DataAssets.SetZone(TMenuItem(Sender).Tag);
end;

// COPIAR ======================================================================

procedure TpgAssets.pmCopySymbolClick(Sender: TObject);
begin
  _CopySymbols;
  Clipboard.AsText := fCopyText;
end;

procedure TpgAssets.pmCopyMSymbolClick(Sender: TObject);
begin
  _CopySymbols(True);
  Clipboard.AsText := fCopyText;
end;

procedure TpgAssets.pmCopyMSymbolDClick(Sender: TObject);
begin
  _CopySymbols(True, True);
  Clipboard.AsText := fCopyText;
end;

// MARCADORES ==================================================================

procedure TpgAssets.pmBlackListClick(Sender: TObject);
var
  BM : TBookMark;
begin
  if _IsGroupSelected then
    begin
     DataAssets.tblData.DisableControls;
     _GetCursorPosition;

     for BM in grdAssets.SelectedRows do
      begin
       _DataSet.Bookmark := BM;
       DataAssets.ToggleBlackList;
      end;

     _RestoreCursorPosition;
     DataAssets.tblData.EnableControls;
    end
   else DataAssets.ToggleBlackList;
end;

procedure TpgAssets.pmFavoriteClick(Sender: TObject);
var
  BM : TBookMark;
begin
  if _IsGroupSelected then
    begin
     _GetCursorPosition;
     for BM in grdAssets.SelectedRows do
      begin
       _DataSet.Bookmark := BM;
       DataAssets.ToggleFavorite;
      end;
     _RestoreCursorPosition;
    end
   else DataAssets.ToggleFavorite;
end;

procedure TpgAssets.pmDeleteClick(Sender: TObject);
const
  ID_QUESTION = 'Grid.Delete';
var
  i, Count : Integer;
  DeleteAsset : Boolean;
begin
  Count := grdAssets.SelectedRows.Count;

  if Configuration.Question[ID_QUESTION] then
   with dlgDeleteQuestion, _DataSet do
    begin
     Caption := 'Confirmar';
     Text := '¿Borrar ';

     // Solo el cursor ------------------
     if Count = 0 then
      begin
       Title := '¿Desea borrar ' + FieldByName(FLD_ASSET_SYMBOL).AsString + ' y los datos asociados?';
       Text  := QuotedStr(FieldByName(FLD_ASSET_DESCRIPTION).AsString +
                   '[ ' + FieldByName(FLD_ASSET_TYPE).DisplayText     + ' / ' +
                          FieldByName(FLD_ASSET_CURRENCY).DisplayText + ']');
      end
     else
      begin
       Title := '¿Borrar ' + IntToStr(Count) + ' activos?';
       Text  := '¿Borrar ' + IntToStr(Count) + ' activos y los datos asociados?';
       if Count <= 10 then  // 10 seleccionados --------------
        begin _CopySymbols; Text := Text + LineEnding + ' ' + LineEnding + fCopyText; end
      end;

     // Botones -----------------------------------
     CommonButtons := [];  Buttons.Clear;
     with Buttons.Add do begin ModalResult := mrYes;    Caption := '&Borrar'; end;
     with Buttons.Add do begin ModalResult := mrCancel; Caption := 'Cancelar'; Default := True; end;

     if Execute then
      begin
       Configuration.Question[ID_QUESTION] := not(tfVerificationFlagChecked in Flags);
       DeleteAsset := (ModalResult = mrYes);
      end;
    end
   else DeleteAsset := True;

  // ===========================================================
  // == Ejecutar borrado
  if DeleteAsset then
   if _IsGroupSelected then
     begin
      //grdAssets.DataSource.Enabled := False;
      DataAssets.tblData.DisableControls;

      for i := Count - 1 downto 0 do
       begin
        _DataSet.GotoBookmark(grdAssets.SelectedRows[i]);
        DataAssets.Delete;
       end;
      grdAssets.SelectedRows.clear;

      //with DataAssets.tblAssets do begin Close; Open; end;
      DataAssets.tblData.EnableControls;
      //grdAssets.DataSource.Enabled := True;
     end
    else DataAssets.Delete;
end;

procedure TpgAssets.pmViewFilterClick(Sender: TObject);
begin
  with pnlFilter do Visible := not(Visible);
end;

procedure TpgAssets.pmViewDataClick(Sender: TObject);
begin
  with pnlAssetData do Visible := not(Visible);
end;

// =============================================================================
// == TECLAS RAPIDAS                                                          ==
// =============================================================================

procedure TpgAssets.grdAssetsKeyPress(Sender: TObject; var Key: char);
begin
  //WriteLN('Press -->', Key);
end;

procedure TpgAssets.grdAssetsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  with DataAssets do
   begin
    if UpperCase(char(Key)) = 'X' then ToggleBlackList;
    if UpperCase(char(Key)) = 'F' then ToggleFavorite;
   end;
end;

// Mensajes de progreso ------------------------------------------------------
procedure TpgAssets._setLED (aOn : boolean);
begin
  imgLED.ImageIndex := IfThen(aOn, 1, 0);
  fUpdating := aOn;
end;

// ACTUALIZACION : Disparo =====================================================
procedure TpgAssets.UpdateStart(aFiltered : Boolean);
begin
  // Crear hilo --------------------------------------------------------
  _UpdateKill;  // Verificar que no haya un hilo corriendo
  fUpdateThread:= TThreadUpdate.Create;
  with fUpdateThread do
   begin
    OnComplete  := @_OnUpdateComplete;
    OnProgress  := @_OnUpdateProgress;
    OnTerminate := @_OnTerminate;
    _setLED; // Encendido

    fUpdateTimeStart := Now;
    fUpdateThread.Start;
   end;
end;

procedure TpgAssets._UpdateKill;
begin
  if Assigned(fUpdateThread) then
   begin
    fUpdateThread.Terminate;
    fUpdateThread.WaitFor;
    FreeAndNil(fUpdateThread);
   end;
end;

procedure TpgAssets.UpdateStop(Ask : Boolean);
var
  Flag : Boolean;
begin
  Flag := True;

  if Ask then if Updating then
   begin
    Flag := Application.MessageBox(
        '¿Detener la actualización en curso? [Los datos pueden quedar inconsistentes]',
        '¿Detener actualización?', MB_YESNO) = IDYES;
   end;

  // Ejecutar cierre ------------------------------------
  if Flag then if (Updating) then fUpdateThread.Terminate;
end;

// Mensajes de prograso --------------------------------------------------------
procedure TpgAssets._OnUpdateProgress(const aMsg: String);
begin
  if not(IsEmptyString(aMsg)) then lblStsMessage.Caption := aMsg;
  Application.ProcessMessages;
end;

procedure TpgAssets._OnUpdateComplete(Sender: TObject);
begin
  _setLED(False); // Off
  lblStsMessage.Caption := 'Actualización completa.';
  fUpdateFinish := Now;
  tmrDoneMsg.Enabled := True;
end;

procedure TpgAssets._OnTerminate(Sender: TObject);
begin
  _setLED(False); // Off
  lblStsMessage.Caption := 'Actualización detenida.';
end;

procedure TpgAssets.tmrDoneMsgTimer(Sender: TObject);
var
  totalSeconds : Integer;
begin
  tmrDoneMsg.Enabled := False;
  TotalSeconds := SecondsBetween(fUpdateFinish, fUpdateTimeStart);
  lblStsMessage.Caption := 'Últ. act. ' + FormatDateTime('hh:nn:ss', fUpdateFinish) +
            ' [' + IntToStr(totalSeconds) +' seg' + Pluralize(totalSeconds,'s') + '.]';
end;

end.

