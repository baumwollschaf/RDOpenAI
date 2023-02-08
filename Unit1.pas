unit Unit1;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  rd.OpenAI.Model,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    RDChatGpt1: TRDChatGpt;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FApiKey: string;
    procedure Answer(Sender: TObject; AMessage: string);
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils;

{$R *.fmx}

procedure TForm1.Answer(Sender: TObject; AMessage: string);
begin
  Memo1.Lines.Add(AMessage);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RDChatGpt1.Question := Edit1.Text;
  RDChatGpt1.Execute;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RDChatGpt1.OnAnswer := Answer;
  ReportMemoryLeaksOnShutdown := True;
  if TFile.Exists('ApiKey.txt') then
  begin
    FApiKey := TFile.ReadAllText('ApiKey.txt');
    RDChatGpt1.ApiKey := FApiKey;
  end;
end;

end.
