program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  {$IFDEF baumwollschaf}
  Extern.ApiKey in '..\..\Chatti\Project\Extern.ApiKey.pas',
  {$ENDIF}
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
