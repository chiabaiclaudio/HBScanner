unit Page_Portfolio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, Menus;

type

  { TpgPortfolio }

  TpgPortfolio = class(TForm)
    DBGrid1: TDBGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
  private

  public

  end;

var
  pgPortfolio: TpgPortfolio;

implementation

{$R *.lfm}

end.

