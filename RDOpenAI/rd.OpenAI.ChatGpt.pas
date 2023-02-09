unit rd.OpenAI.ChatGpt;

interface

uses
  Pkg.Json.DTO,
  System.Generics.Collections,
  REST.Json.Types;

{$M+}

type
  TUsage = class
  private
    [JSONName('completion_tokens')]
    FCompletionTokens: Integer;
    [JSONName('prompt_tokens')]
    FPromptTokens: Integer;
    [JSONName('total_tokens')]
    FTotalTokens: Integer;
  published
    property CompletionTokens: Integer read FCompletionTokens write FCompletionTokens;
    property PromptTokens: Integer read FPromptTokens write FPromptTokens;
    property TotalTokens: Integer read FTotalTokens write FTotalTokens;
  end;

  TChoices = class
  private
    [JSONName('finish_reason')]
    FFinishReason: string;
    FIndex: Integer;
    FText: string;
  published
    property FinishReason: string read FFinishReason write FFinishReason;
    property index: Integer read FIndex write FIndex;
    property Text: string read FText write FText;
  end;

  TCompletions = class(TJsonDTO)
  private
    [JSONName('choices'), JSONMarshalled(False)]
    FChoicesArray: TArray<TChoices>;
    [GenericListReflect]
    FChoices: TObjectList<TChoices>;
    FCreated: Integer;
    FId: string;
    FModel: string;
    FObject: string;
    FUsage: TUsage;
    function GetChoices: TObjectList<TChoices>;
  protected
    function GetAsJson: string; override;
  published
    property Choices: TObjectList<TChoices> read GetChoices;
    property Created: Integer read FCreated write FCreated;
    property Id: string read FId write FId;
    property Model: string read FModel write FModel;
    property &Object: string read FObject write FObject;
    property Usage: TUsage read FUsage;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TQuestion = class(TJsonDTO)
  private
    FModel: string;
    FPrompt: string;
    FTemperature: Double;
    [JSONName('max_tokens')]
    FMaxTokens: Integer;
  published
    property Model: string read FModel write FModel;
    property Prompt: string read FPrompt write FPrompt;
    property Temperature: Double read FTemperature write FTemperature;
    property MaxTokens: Integer read FMaxTokens write FMaxTokens;
  end;

implementation

constructor TCompletions.Create;
begin
  inherited;
  FUsage := TUsage.Create;
end;

destructor TCompletions.Destroy;
begin
  FUsage.Free;
  GetChoices.Free;
  inherited;
end;

function TCompletions.GetChoices: TObjectList<TChoices>;
begin
  Result := ObjectList<TChoices>(FChoices, FChoicesArray);
end;

function TCompletions.GetAsJson: string;
begin
  RefreshArray<TChoices>(FChoices, FChoicesArray);
  Result := inherited;
end;

end.
