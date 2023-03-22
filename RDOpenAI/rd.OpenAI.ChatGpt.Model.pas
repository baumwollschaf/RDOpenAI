unit rd.OpenAI.ChatGpt.Model;

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

  TInstruction = class(TJsonDTO)
  private
    FInput: string;
    FInstruction: string;
    FModel: string;
  published
    property Input: string read FInput write FInput;
    property Instruction: string read FInstruction write FInstruction;
    property Model: string read FModel write FModel;
  end;

type
  TPermission = class
  private
    [JSONName('allow_create_engine')]
    FAllowCreateEngine: Boolean;
    [JSONName('allow_fine_tuning')]
    FAllowFineTuning: Boolean;
    [JSONName('allow_logprobs')]
    FAllowLogprobs: Boolean;
    [JSONName('allow_sampling')]
    FAllowSampling: Boolean;
    [JSONName('allow_search_indices')]
    FAllowSearchIndices: Boolean;
    [JSONName('allow_view')]
    FAllowView: Boolean;
    FCreated: Integer;
    FId: string;
    [JSONName('is_blocking')]
    FIsBlocking: Boolean;
    FObject: string;
    FOrganization: string;
  published
    property AllowCreateEngine: Boolean read FAllowCreateEngine write FAllowCreateEngine;
    property AllowFineTuning: Boolean read FAllowFineTuning write FAllowFineTuning;
    property AllowLogprobs: Boolean read FAllowLogprobs write FAllowLogprobs;
    property AllowSampling: Boolean read FAllowSampling write FAllowSampling;
    property AllowSearchIndices: Boolean read FAllowSearchIndices write FAllowSearchIndices;
    property AllowView: Boolean read FAllowView write FAllowView;
    property Created: Integer read FCreated write FCreated;
    property Id: string read FId write FId;
    property IsBlocking: Boolean read FIsBlocking write FIsBlocking;
    property &Object: string read FObject write FObject;
    property Organization: string read FOrganization write FOrganization;
  end;

  TData = class(TJsonDTO)
  private
    FCreated: Integer;
    FId: string;
    FObject: string;
    [JSONName('owned_by')]
    FOwnedBy: string;
    [JSONName('permission'), JSONMarshalled(False)]
    FPermissionArray: TArray<TPermission>;
    [GenericListReflect]
    FPermission: TObjectList<TPermission>;
    FRoot: string;
    function GetPermission: TObjectList<TPermission>;
  protected
    function GetAsJson: string; override;
  published
    property Created: Integer read FCreated write FCreated;
    property Id: string read FId write FId;
    property &Object: string read FObject write FObject;
    property OwnedBy: string read FOwnedBy write FOwnedBy;
    property Permission: TObjectList<TPermission> read GetPermission;
    property Root: string read FRoot write FRoot;
  public
    destructor Destroy; override;
  end;

  TModels = class(TJsonDTO)
  private
    [JSONName('data'), JSONMarshalled(False)]
    FDataArray: TArray<TData>;
    [GenericListReflect]
    FData: TObjectList<TData>;
    FObject: string;
    function GetData: TObjectList<TData>;
  protected
    function GetAsJson: string; override;
  published
    property Data: TObjectList<TData> read GetData;
    property &Object: string read FObject write FObject;
  public
    destructor Destroy; override;
  end;

  TCategoryScores = class
  private
    FHate: Double;
    [JSONName('hate/threatening')]
    FHateThreatening: Double;
    [JSONName('self-harm')]
    FSelfHarm: Double;
    FSexual: Double;
    [JSONName('sexual/minors')]
    FSexualMinors: Double;
    FViolence: Double;
    [JSONName('violence/graphic')]
    FViolenceGraphic: Double;
  published
    property Hate: Double read FHate write FHate;
    property HateThreatening: Double read FHateThreatening write FHateThreatening;
    property SelfHarm: Double read FSelfHarm write FSelfHarm;
    property Sexual: Double read FSexual write FSexual;
    property SexualMinors: Double read FSexualMinors write FSexualMinors;
    property Violence: Double read FViolence write FViolence;
    property ViolenceGraphic: Double read FViolenceGraphic write FViolenceGraphic;
  end;

  TCategories = class
  private
    FHate: Boolean;
    [JSONName('hate/threatening')]
    FHateThreatening: Boolean;
    [JSONName('self-harm')]
    FSelfHarm: Boolean;
    FSexual: Boolean;
    [JSONName('sexual/minors')]
    FSexualMinors: Boolean;
    FViolence: Boolean;
    [JSONName('violence/graphic')]
    FViolenceGraphic: Boolean;
  published
    property Hate: Boolean read FHate write FHate;
    property HateThreatening: Boolean read FHateThreatening write FHateThreatening;
    property SelfHarm: Boolean read FSelfHarm write FSelfHarm;
    property Sexual: Boolean read FSexual write FSexual;
    property SexualMinors: Boolean read FSexualMinors write FSexualMinors;
    property Violence: Boolean read FViolence write FViolence;
    property ViolenceGraphic: Boolean read FViolenceGraphic write FViolenceGraphic;
  end;

  TResults = class
  private
    FCategories: TCategories;
    [JSONName('category_scores')]
    FCategoryScores: TCategoryScores;
    FFlagged: Boolean;
  published
    property Categories: TCategories read FCategories;
    property CategoryScores: TCategoryScores read FCategoryScores;
    property Flagged: Boolean read FFlagged write FFlagged;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TModerations = class(TJsonDTO)
  private
    FId: string;
    FModel: string;
    [JSONName('results'), JSONMarshalled(False)]
    FResultsArray: TArray<TResults>;
    [GenericListReflect]
    FResults: TObjectList<TResults>;
    function GetResults: TObjectList<TResults>;
  protected
    function GetAsJson: string; override;
  published
    property Id: string read FId write FId;
    property Model: string read FModel write FModel;
    property Results: TObjectList<TResults> read GetResults;
  public
    destructor Destroy; override;
  end;

  TModerationInput = class(TJsonDTO)
  private
    FInput: string;
  published
    property Input: string read FInput write FInput;
  end;

  TImageData = class
  private
    [JSONName('b64_json')]
    FB64Json: string;
    FUrl: string;
  published
    property B64Json: string read FB64Json write FB64Json;
    property Url: string read FUrl write FUrl;
  end;

  TDallEGenImage = class(TJsonDTO)
  private
    FCreated: Integer;
    [JSONName('data'), JSONMarshalled(False)]
    FDataArray: TArray<TImageData>;
    [GenericListReflect]
    FData: TObjectList<TImageData>;
    function GetData: TObjectList<TImageData>;
  protected
    function GetAsJson: string; override;
  published
    property Created: Integer read FCreated write FCreated;
    property Data: TObjectList<TImageData> read GetData;
  public
    destructor Destroy; override;
  end;

  TInputDallEGenImage = class(TJsonDTO)
  private
    FN: Integer;
    FPrompt: string;
    [JSONName('response_format')]
    FResponseFormat: string;
    FSize: string;
  published
    property N: Integer read FN write FN;
    property Prompt: string read FPrompt write FPrompt;
    property ResponseFormat: string read FResponseFormat write FResponseFormat;
    property Size: string read FSize write FSize;
  end;

implementation

destructor TDallEGenImage.Destroy;
begin
  GetData.Free;
  inherited;
end;

function TDallEGenImage.GetData: TObjectList<TImageData>;
begin
  Result := ObjectList<TImageData>(FData, FDataArray);
end;

function TDallEGenImage.GetAsJson: string;
begin
  RefreshArray<TImageData>(FData, FDataArray);
  Result := inherited;
end;

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

destructor TData.Destroy;
begin
  GetPermission.Free;
  inherited;
end;

function TData.GetPermission: TObjectList<TPermission>;
begin
  Result := ObjectList<TPermission>(FPermission, FPermissionArray);
end;

function TData.GetAsJson: string;
begin
  RefreshArray<TPermission>(FPermission, FPermissionArray);
  Result := inherited;
end;

{ TModels }

destructor TModels.Destroy;
begin
  GetData.Free;
  inherited;
end;

function TModels.GetData: TObjectList<TData>;
begin
  Result := ObjectList<TData>(FData, FDataArray);
end;

function TModels.GetAsJson: string;
begin
  RefreshArray<TData>(FData, FDataArray);
  Result := inherited;
end;

{ TModerations }

constructor TResults.Create;
begin
  inherited;
  FCategories := TCategories.Create;
  FCategoryScores := TCategoryScores.Create;
end;

destructor TResults.Destroy;
begin
  FCategories.Free;
  FCategoryScores.Free;
  inherited;
end;

{ TRoot }

destructor TModerations.Destroy;
begin
  GetResults.Free;
  inherited;
end;

function TModerations.GetResults: TObjectList<TResults>;
begin
  Result := ObjectList<TResults>(FResults, FResultsArray);
end;

function TModerations.GetAsJson: string;
begin
  RefreshArray<TResults>(FResults, FResultsArray);
  Result := inherited;
end;

end.
