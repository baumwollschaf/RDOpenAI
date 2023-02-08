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
    Memo1: TMemo;
    RDChatGpt1: TRDChatGpt;
    Edit1: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure RDChatGpt1Error(Sender: TObject; AMessage: string);
    procedure RDChatGpt1Answer(Sender: TObject; AMessage: string);
  private
    FApiKey: string;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  RDChatGpt1.Question := Edit1.Text;
  RDChatGpt1.Execute;
  Edit1.Text := '';
  Edit1.SetFocus;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  Button1.Enabled := Edit1.Text.Trim <> '';
end;

procedure TForm1.Edit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    Key := 0;
    Button1Click(nil);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  if TFile.Exists('ApiKey.txt') then
  begin
    FApiKey := TFile.ReadAllText('ApiKey.txt');
    RDChatGpt1.ApiKey := FApiKey;
  end;
end;

procedure TForm1.RDChatGpt1Answer(Sender: TObject; AMessage: string);
begin
  Memo1.Lines.Add(AMessage);
end;

procedure TForm1.RDChatGpt1Error(Sender: TObject; AMessage: string);
begin
  Memo1.Lines.Add('Error: ' + AMessage);
end;

end.
