program hbscanner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Config, Main, page_Market, Data_Assets, Data_Historic, Data_Provider,
  HeartBeat, HTTPCodes, Market, Tools;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Heart Beat Scanner';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDataAssets, DataAssets);
  Application.CreateForm(TDataHistoric, DataHistoric);
  Application.CreateForm(TDataProvider, DataProvider);
  Application.CreateForm(TwMain, wMain);
  Application.Run;
end.

