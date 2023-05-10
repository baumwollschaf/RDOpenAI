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
  rd.OpenAI.ChatGpt.ViewModel,
  rd.OpenAI.ChatGpt.Model,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  System.Json,
  Rest.Json,
  System.Generics.Collections,
  Rest.JsonReflect,
  FMX.ListBox,
  FMX.Layouts;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    RDChatGpt1: TRDChatGpt;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Layout1: TLayout;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure RDChatGpt1Error(Sender: TObject; AMessage: string);
    procedure RDChatGpt1Answer(Sender: TObject; AMessage: string);
    procedure ComboBox1Change(Sender: TObject);
    procedure RDChatGpt1ModelsLoaded(Sender: TObject; AModels: TModels);
    procedure RDChatGpt1CompletionsLoaded(Sender: TObject; AType: TCompletions);
    procedure Button2Click(Sender: TObject);
    procedure RDChatGpt1ModerationsLoaded(Sender: TObject; AType: TModerations);
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
  RDChatGpt1.Cancel;
  if RDChatGpt1.Gpt35AndUp(RDChatGpt1.Model) then
  begin
    RDChatGpt1.Chat(Edit1.Text);
  end else begin
    RDChatGpt1.Ask(Edit1.Text);
  end;
  Edit1.SetFocus;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  RDChatGpt1.Cancel;
  RDChatGpt1.ModerationInput.Input := Edit1.Text;
  RDChatGpt1.LoadModerations;
  Edit1.SetFocus;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  if (ComboBox1.ItemIndex <> -1) and (ComboBox1.IsFocused) then
  begin
    RDChatGpt1.Model := ComboBox1.Items[ComboBox1.ItemIndex];
  end;
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
  if RDChatGpt1.ApiKey = '' then
  begin
    ShowMessage('ApiKey not set.');
  end
  else
  begin
    RDChatGpt1.LoadModels;
  end;
end;

procedure TForm1.RDChatGpt1Answer(Sender: TObject; AMessage: string);
begin
  Edit1.Text := '';
  Memo1.Lines.Add(AMessage);
end;

procedure TForm1.RDChatGpt1CompletionsLoaded(Sender: TObject; AType: TCompletions);
begin
  Caption := AType.Model; // just testing
end;

procedure TForm1.RDChatGpt1Error(Sender: TObject; AMessage: string);
begin
  Memo1.Lines.Add('Error: ' + AMessage);
end;

procedure TForm1.RDChatGpt1ModelsLoaded(Sender: TObject; AModels: TModels);
begin
  Assert(AModels <> nil);
  ComboBox1.Items.Clear;
  for var i: Integer := 0 to AModels.Data.Count - 1 do
  begin
    ComboBox1.Items.Add(AModels.Data[i].Root);
  end;
  ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(RDChatGpt1.Model);
  ComboBox1.Enabled := ComboBox1.ItemIndex <> -1;
end;

procedure TForm1.RDChatGpt1ModerationsLoaded(Sender: TObject; AType: TModerations);
begin
  if AType <> nil then
  begin
    if AType.Results.Count > 0 then
    begin
      // example of how to use
      if AType.Results[0].Categories.Hate then
      begin
        Memo1.Lines.Add('Hate included');
      end else begin
        Memo1.Lines.Add('No Hate');
      end;

      if AType.Results[0].Categories.HateThreatening then
      begin
        Memo1.Lines.Add('HateThreatening included');
      end else begin
        Memo1.Lines.Add('No HateThreatening');
      end;

      if AType.Results[0].Categories.Sexual then
      begin
        Memo1.Lines.Add('Sexual included');
      end else begin
        Memo1.Lines.Add('No Sexual');
      end;
    end;
  end;
end;

end.
