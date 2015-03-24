unit aboutsdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TAboutProg }

  TAboutProg = class(TForm)
    MadeByLabel: TLabel;
    sDBLabel: TLabel;
  end;

var
  AboutProg: TAboutProg;

implementation

{$R *.lfm}

end.

