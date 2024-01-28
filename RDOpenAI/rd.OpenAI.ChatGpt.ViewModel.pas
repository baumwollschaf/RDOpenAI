unit rd.OpenAI.ChatGpt.ViewModel;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  REST.JSON.Types,
  System.JSON,
  System.Rtti,
  System.TypInfo,
  System.Character,
  System.Math,
  System.DateUtils,
  IPPeerClient,
  REST.Client,
  REST.Authenticator.Basic,
  Data.Bind.ObjectScope,
  REST.Response.Adapter,
  REST.Types,
  REST.JSON,
  rd.OpenAI.ChatGpt.Model,
  System.Generics.Collections;
{$METHODINFO ON}
{$M+}

type
  RDOpenAIException = class(Exception);

  TGetOrFinish = (gfGet, gfFinish);
  TRequestInfoProc = procedure(AURL: string; AGetOrFinish: TGetOrFinish) of object;
  TMessageEvent = procedure(Sender: TObject; AMessage: string) of object;

  // Returns any Model-Classes
  TTypedEvent<T: class> = procedure(Sender: TObject; AType: T) of object;

  TRDOpenAIConnection = class abstract(TComponent)
  public type
    TFinishReason = (frNone, frStop, frLength);
  public const
    cVERSION = '1.10';
  private
    function StrToFinishReason(AValue: string): TFinishReason;
  private const
    cBEARER = 'Bearer';
    cDEF_MAX_TOKENS = 2048; // 1024
    cDEF_URL = 'https://api.openai.com/v1';
    cDEF_TEMP = 0.1;
    cDEF_MODEL = 'gpt-3.5-turbo';
  public const
    cDEFAULT_USER_AGENT = 'RD OPEN AI CONNECT';
    cJSON_OPTIONS = [JoDateIsUTC, JoDateFormatISO8601, JoIgnoreEmptyArrays];
  private
    FRESTRequestParameter, FRESTRequestParameter2: TRESTRequestParameter;
    FApiKey: string;
    FTemperature: double;
    FModel: string;
    FMaxTokens: Integer;
    procedure SetApiKey(const Value: string);
    function GetVersion: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Version: String read GetVersion;
    property ApiKey: string read FApiKey write SetApiKey;
    property Temperature: double read FTemperature write FTemperature;
    property Model: string read FModel write FModel;
    property MaxTokens: Integer read FMaxTokens write FMaxTokens default cDEF_MAX_TOKENS;
  end;

  TRDOpenAIRestClient = class abstract(TRDOpenAIConnection)
  strict private
    function GetAccept: string;
    procedure SetAccept(const Value: string);
    function GetAcceptCharset: string;
    procedure SetAcceptCharset(const Value: string);
    function GetAcceptEncoding: string;
    procedure SetAcceptEncoding(const Value: string);
    function GetBaseURL: string;
    procedure SetBaseURL(const Value: string);
    function GetProxy: string;
    procedure SetProxy(const Value: string);
    function GetProxyPort: Integer;
    procedure SetProxyPort(const Value: Integer);
  protected
    FRestClient: TCustomRESTClient;
    property BaseURL: string read GetBaseURL write SetBaseURL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property RestClient: TCustomRESTClient read FRestClient;
  published
    property Accept: string read GetAccept write SetAccept stored True;
    property AcceptCharset: string read GetAcceptCharset write SetAcceptCharset stored True;
    property AcceptEncoding: string read GetAcceptEncoding write SetAcceptEncoding stored True;
    property Proxy: string read GetProxy write SetProxy;
    property ProxyPort: Integer read GetProxyPort write SetProxyPort default 0;
  end;

  TRDOpenAI = class abstract(TRDOpenAIRestClient)
  private type
    TModelEndPoint = (meCompletions, meChatCompletions, meModerations, meInstructions, meDallEGen, meModels);
  strict private
    FLastError: string;
    FResponse: TRESTResponse;
    FRequest: TRESTRequest;
    FOnAnswer: TMessageEvent;
    FOnError: TMessageEvent;
    FOnModelsLoaded: TTypedEvent<TModels>;
    FOnCompletionsLoaded: TTypedEvent<TCompletions>;
    FOnChatCompletionsLoaded: TTypedEvent<TCompletions>;
    FOnInstrCompletionsLoaded: TTypedEvent<TCompletions>;
    FOnModerationsLoaded: TTypedEvent<TModerations>;
    FOnDallEGenImageLoaded: TTypedEvent<TDallEGenImage>;

    FIgnoreReturns: Boolean;

    FCompletions: TCompletions;
    FChatCompletions: TCompletions;
    FModels: TModels;
    FModerations: TModerations;
    FInstrCompletions: TCompletions;
    FDallEGenImage: TDallEGenImage;

    FRequestInfoProc: TRequestInfoProc;
    procedure ProtocolError(Sender: TCustomRESTRequest);
    procedure ProtocolErrorClient(Sender: TCustomRESTClient);
    function GetURL: string;
    procedure SetURL(const Value: string);
    function GetEndPoint(AModelEndPoint: TModelEndPoint): String;
  strict private
    function CreateRequest: TRESTRequest;
  protected
    FBusy: Boolean;
    FQuestionSettings: TQuestion;
    FInstructionSettings: TInstruction;
    FInputDallEImage: TInputDallEGenImage;
    FInputChatCompletion: TInputChatCompletion;
    procedure RefreshCompletions;
    procedure RefreshChatCompletions;
    procedure RefreshInstrCompletions;
    procedure CompletionCallback;
    procedure ChatCompletionCallback;
    procedure InstrCompletionCallback;

    procedure RefreshModels;
    procedure ModelsCallback;

    procedure RefreshModerations;
    procedure ModerationsCallback;

    procedure RefreshDallEGenImage;
    procedure DallEGenImageCallback;

    function RemoveEmptyLinesWithReturns(AText: string): string;
  private
    FModerationInput: TModerationInput;
    FTimeOutSeconds: Integer;
    procedure CheckApiKey;
    procedure CheckDallEGenInput;
    procedure CheckModel;
    procedure CheckQuestion;
    procedure CheckInstruction;
    procedure CheckModerationInput;
    procedure CheckContentAndRole;
    procedure SetTimeOutSeconds(const Value: Integer);
  protected
    FAsynchronous: Boolean;
    procedure DoAnswer(AMessage: string); virtual;
    procedure DoError(AMessage: string); virtual;
    procedure DoModelsLoad(AModels: TModels); virtual;
    procedure DoCompletionsLoad(ACompletions: TCompletions); virtual;
    procedure DoChatCompletionsLoad(ACompletions: TCompletions); virtual;
    procedure DoInstrCompletionsLoad(ACompletions: TCompletions); virtual;
    procedure DoModerationsLoad(AModerations: TModerations); virtual;
    procedure DoDallEImageGenLoad(ADallEImageGen: TDallEGenImage); virtual;

    procedure DoCompletionHandlerWithError(AObject: TObject);
  strict private
    procedure SetAsynchronous(const Value: Boolean);
    procedure SetModerationInput(const Value: TModerationInput);
  public
    function Gpt35AndUp(AModel: String): Boolean;

    property ModerationInput: TModerationInput read FModerationInput write SetModerationInput;
    property InputDallEImage: TInputDallEGenImage read FInputDallEImage write FInputDallEImage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Cancel;
    procedure Assign(Source: TPersistent); override;
  strict private
    property Asynchronous: Boolean read FAsynchronous write SetAsynchronous default True;
  published
    property URL: string read GetURL write SetURL stored True;
    property IgnoreReturns: Boolean read FIgnoreReturns write FIgnoreReturns default True;
    property OnAnswer: TMessageEvent read FOnAnswer write FOnAnswer;
    property OnError: TMessageEvent read FOnError write FOnError;
    property OnModelsLoaded: TTypedEvent<TModels> read FOnModelsLoaded write FOnModelsLoaded;
    property OnCompletionsLoaded: TTypedEvent<TCompletions> read FOnCompletionsLoaded write FOnCompletionsLoaded;
    property OnChatCompletionsLoaded: TTypedEvent<TCompletions> read FOnChatCompletionsLoaded write FOnChatCompletionsLoaded;
    property OnInstrCompletionsLoaded: TTypedEvent<TCompletions> read FOnInstrCompletionsLoaded write FOnInstrCompletionsLoaded;
    property OnModerationsLoaded: TTypedEvent<TModerations> read FOnModerationsLoaded write FOnModerationsLoaded;
    property OnDallEGenImageLoaded: TTypedEvent<TDallEGenImage> read FOnDallEGenImageLoaded write FOnDallEGenImageLoaded;
    property TimeOutSeconds: Integer read FTimeOutSeconds write SetTimeOutSeconds default 30;
  end;

  TRDChatGpt = class(TRDOpenAI)
  private
  public
    procedure Ask(AQuestion: string = ''); overload;
    procedure Chat(AContent: string; ARole: String = 'user'); overload;
    procedure Instruct(AInput: String; AInstruction: String);
    procedure LoadModels;
    procedure LoadModerations(AInput: string = '');
    procedure GenerateImage(APrompt: String; ASize: String = '1024x1024'; AFormat: string = 'url'); // 'b64_json'
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RD OpenAI', [TRDChatGpt]);
end;

constructor TRDOpenAIConnection.Create(AOwner: TComponent);
begin
  inherited;
  FRESTRequestParameter := TRESTRequestParameter.Create(nil);
  FRESTRequestParameter.Kind := PkHTTPHEADER;
  FRESTRequestParameter.Name := 'Authorization';
  FRESTRequestParameter.Options := [PoDoNotEncode];
  FRESTRequestParameter.Value := '';

  FRESTRequestParameter2 := TRESTRequestParameter.Create(nil);
  FRESTRequestParameter2.Kind := pkREQUESTBODY;
  FRESTRequestParameter2.Name := 'AnyBody';
  FRESTRequestParameter2.Value := '';
  FRESTRequestParameter2.ContentType := 'application/json';

  FTemperature := cDEF_TEMP;
  FModel := '';
  FMaxTokens := cDEF_MAX_TOKENS;
end;

destructor TRDOpenAIConnection.Destroy;
begin
  FreeAndNil(FRESTRequestParameter);
  FreeAndNil(FRESTRequestParameter2);
  inherited;
end;

function TRDOpenAIConnection.GetVersion: String;
begin
  Result := cVERSION;
end;

procedure TRDOpenAIConnection.SetApiKey(const Value: string);
begin
  if FApiKey <> Value then
  begin
    FApiKey := Value;
    FRESTRequestParameter.Value := cBEARER + ' ' + Value;
  end;
end;

function TRDOpenAIConnection.StrToFinishReason(AValue: string): TFinishReason;
begin
  Result := frNone;
  AValue := AValue.ToLower;
  if AValue = 'stop' then
    Exit(frStop)
  else if AValue = 'length' then
    Exit(frLength);
end;

{ TRDOpenAIRestClient }

constructor TRDOpenAIRestClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRestClient := TCustomRESTClient.Create(Self);
  FRestClient.UserAgent := cDEFAULT_USER_AGENT;
end;

destructor TRDOpenAIRestClient.Destroy;
begin
  FreeAndNil(FRestClient);
  inherited Destroy;
end;

function TRDOpenAIRestClient.GetAccept: string;
begin
  Result := FRestClient.Accept;
end;

function TRDOpenAIRestClient.GetAcceptCharset: string;
begin
  Result := FRestClient.AcceptCharset;
end;

function TRDOpenAIRestClient.GetAcceptEncoding: string;
begin
  Result := FRestClient.AcceptEncoding;
end;

function TRDOpenAIRestClient.GetBaseURL: string;
begin
  Result := FRestClient.BaseURL;
end;

function TRDOpenAIRestClient.GetProxy: string;
begin
  Result := FRestClient.ProxyServer;
end;

function TRDOpenAIRestClient.GetProxyPort: Integer;
begin
  Result := FRestClient.ProxyPort;
end;

procedure TRDOpenAIRestClient.SetAccept(const Value: string);
begin
  FRestClient.Accept := Value;
end;

procedure TRDOpenAIRestClient.SetAcceptCharset(const Value: string);
begin
  FRestClient.AcceptCharset := Value;
end;

procedure TRDOpenAIRestClient.SetAcceptEncoding(const Value: string);
begin
  FRestClient.AcceptEncoding := Value;
end;

procedure TRDOpenAIRestClient.SetBaseURL(const Value: string);
begin
  FRestClient.BaseURL := Value;
end;

procedure TRDOpenAIRestClient.SetProxy(const Value: string);
begin
  FRestClient.ProxyServer := Value;
end;

procedure TRDOpenAIRestClient.SetProxyPort(const Value: Integer);
begin
  FRestClient.ProxyPort := Value;
end;

procedure TRDOpenAI.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  var
    OpenAI: TRDOpenAI := nil;
  if Source is TRDOpenAI then
    OpenAI := TRDOpenAI(Source);

  if OpenAI = nil then
    Exit;

  Self.Asynchronous := OpenAI.FAsynchronous;
  Self.URL := OpenAI.URL;
  Self.Model := OpenAI.Model;
  Self.Temperature := OpenAI.Temperature;
  Self.MaxTokens := OpenAI.MaxTokens;
  Self.TimeOutSeconds := OpenAI.TimeOutSeconds;
  Self.IgnoreReturns := OpenAI.IgnoreReturns;
end;

procedure TRDOpenAI.Cancel;
begin
  FBusy := False;
  if FRequest <> nil then
  begin
    FRequest.Cancel;
  end;
end;

procedure TRDOpenAI.ChatCompletionCallback;
var
  JsonObj: TJSONObject;
begin
  try
    if FRequest = nil then
      Exit;
    if FResponse = nil then
      Exit;

    if FResponse.StatusCode <> 200 then
    begin
      FLastError := FResponse.StatusText;
      DoError(FLastError);
      Exit;
    end;

    if assigned(FRequestInfoProc) then
      FRequestInfoProc(FRequest.Resource, gfFinish);

    JsonObj := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(FResponse.Content), 0) as TJSONObject;
    if JsonObj = nil then
      Exit;

    try
      try
        FreeAndNil(FChatCompletions);
        FChatCompletions := TJson.JsonToObject<TCompletions>(TJSONObject(JsonObj), cJSON_OPTIONS);
        DoChatCompletionsLoad(FChatCompletions);
        if (FChatCompletions <> nil) and (FChatCompletions.Choices.Count > 0) then
        begin
          case StrToFinishReason(FChatCompletions.Choices[0].FinishReason) of
            frStop, frLength:
              begin
                DoAnswer(FChatCompletions.Choices[0].Message.Content);
              end;
          end;
        end;
      finally
        JsonObj.Free;
      end;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        DoError(FLastError);
      end;
    end;
  finally
    FBusy := False;
  end;
end;

procedure TRDOpenAI.CheckApiKey;
begin
  if FApiKey = '' then
    raise RDOpenAIException.Create('ApiKey not set.');
end;

procedure TRDOpenAI.CheckContentAndRole;
begin
  if FInputChatCompletion.Messages.Count < 1 then
    raise RDOpenAIException.Create('InputChatCompletion.Messages.Count < 1');

end;

procedure TRDOpenAI.CheckDallEGenInput;
begin
  if FInputDallEImage.Prompt = '' then
    raise RDOpenAIException.Create('InputDallEImage.Prompt not set.');
  if FInputDallEImage.Size = '' then
    raise RDOpenAIException.Create('InputDallEImage.Size not set.');
end;

procedure TRDOpenAI.CheckModel;
begin
  if FModel = '' then
    raise RDOpenAIException.Create('Model not set.');
end;

procedure TRDOpenAI.CheckModerationInput;
begin
  if FModerationInput.Input = '' then
    raise RDOpenAIException.Create('ModerationInput.Input not set.');
end;

procedure TRDOpenAI.CheckQuestion;
begin
  if FQuestionSettings.Prompt = '' then
    raise RDOpenAIException.Create('Question not set.');
end;

procedure TRDOpenAI.CheckInstruction;
begin
  if FInstructionSettings.Instruction = '' then
    raise RDOpenAIException.Create('InstructionSettings.Instruction not set.');
end;

constructor TRDOpenAI.Create(AOwner: TComponent);
begin
  inherited;
  FInputChatCompletion := TInputChatCompletion.Create;
  FRestClient.OnHTTPProtocolError := ProtocolErrorClient;
  FAsynchronous := True;
  FTimeOutSeconds := 30; // in seconds
  URL := cDEF_URL;
  FQuestionSettings := TQuestion.Create;
  FInstructionSettings := TInstruction.Create;
  FModerationInput := TModerationInput.Create;
  FInputDallEImage := TInputDallEGenImage.Create;
  FIgnoreReturns := True;
end;

function TRDOpenAI.CreateRequest: TRESTRequest;
begin
  Result := TRESTRequest.Create(nil);
  Result.OnHTTPProtocolError := ProtocolError;
  Result.Client := FRestClient;
  Result.SynchronizedEvents := FAsynchronous;
  Result.Timeout := FTimeOutSeconds * 1000;
end;

procedure TRDOpenAI.DallEGenImageCallback;
var
  JsonObj: TJSONObject;
begin
  try
    if FRequest = nil then
      Exit;
    if FResponse = nil then
      Exit;

    if FResponse.StatusCode <> 200 then
    begin
      FLastError := FResponse.StatusText;
      DoError(FLastError);
      Exit;
    end;

    if assigned(FRequestInfoProc) then
      FRequestInfoProc(FRequest.Resource, gfFinish);

    JsonObj := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(FResponse.Content), 0) as TJSONObject;
    if JsonObj = nil then
      Exit;

    try
      try
        FreeAndNil(FDallEGenImage);
        FDallEGenImage := TJson.JsonToObject<TDallEGenImage>(TJSONObject(JsonObj), cJSON_OPTIONS);
        DoDallEImageGenLoad(FDallEGenImage);
      finally
        JsonObj.Free;
      end;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        DoError(FLastError);
      end;
    end;
  finally
    FBusy := False;
  end;
end;

destructor TRDOpenAI.Destroy;
begin
  Cancel;
  FreeAndNil(FInputChatCompletion);
  FreeAndNil(FInputDallEImage);
  FreeAndNil(FInstructionSettings);
  FreeAndNil(FQuestionSettings);
  FreeAndNil(FCompletions);
  FreeAndNil(FChatCompletions);
  FreeAndNil(FInstrCompletions);
  FreeAndNil(FModerationInput);
  FreeAndNil(FModels);
  FreeAndNil(FModerations);
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
  inherited;
end;

procedure TRDOpenAI.DoAnswer(AMessage: string);
begin
  if assigned(FOnAnswer) then
  begin
    if FIgnoreReturns then
    begin
      AMessage := RemoveEmptyLinesWithReturns(AMessage);
    end;

    FOnAnswer(Self, AMessage);
  end;
end;

procedure TRDOpenAI.DoError(AMessage: string);
begin
  if assigned(FOnError) then
  begin
    if FIgnoreReturns then
    begin
      AMessage := RemoveEmptyLinesWithReturns(AMessage);
    end;

    FOnError(Self, AMessage);
  end;
end;

procedure TRDOpenAI.DoModelsLoad(AModels: TModels);
begin
  if assigned(FOnModelsLoaded) then
  begin
    FOnModelsLoaded(Self, AModels);
  end;
end;

procedure TRDOpenAI.DoModerationsLoad(AModerations: TModerations);
begin
  if assigned(FOnModerationsLoaded) then
  begin
    FOnModerationsLoaded(Self, AModerations);
  end;
end;

procedure TRDOpenAI.DoChatCompletionsLoad(ACompletions: TCompletions);
begin
  if assigned(FOnChatCompletionsLoaded) then
  begin
    FOnChatCompletionsLoaded(Self, ACompletions);
  end;
end;

procedure TRDOpenAI.DoCompletionHandlerWithError(AObject: TObject);
begin
  try
    DoError(Exception(AObject).Message);
  except
    ;
  end;
end;

procedure TRDOpenAI.DoCompletionsLoad(ACompletions: TCompletions);
begin
  if assigned(FOnCompletionsLoaded) then
  begin
    FOnCompletionsLoaded(Self, ACompletions);
  end;
end;

procedure TRDOpenAI.DoDallEImageGenLoad(ADallEImageGen: TDallEGenImage);
begin
  if assigned(FOnDallEGenImageLoaded) then
  begin
    FOnDallEGenImageLoaded(Self, ADallEImageGen);
  end;
end;

procedure TRDOpenAI.DoInstrCompletionsLoad(ACompletions: TCompletions);
begin
  if assigned(FOnInstrCompletionsLoaded) then
  begin
    FOnInstrCompletionsLoaded(Self, ACompletions);
  end;
end;

function TRDOpenAI.GetEndPoint(AModelEndPoint: TModelEndPoint): String;
begin
  Result := '';
  case AModelEndPoint of
    meCompletions:
      Result := 'completions';
    meChatCompletions:
      Result := 'chat/completions';
    meModerations:
      Result := 'moderations';
    meInstructions:
      Result := 'edits';
    meDallEGen:
      Result := 'images/generations';
    meModels:
      Result := 'models';
  end;
  Assert(Result <> '');
end;

function TRDOpenAI.GetURL: string;
begin
  Result := BaseURL;
end;

function TRDOpenAI.Gpt35AndUp(AModel: String): Boolean;
begin
  Result := False;
  AModel := AModel.ToLower.Trim;
  const
    Models: TArray<String> = ['gpt-4', 'gpt-4-0314', 'gpt-4-32k', 'gpt-4-32k-0314', 'gpt-3.5-turbo', 'gpt-3.5-turbo-0301'];
  for var s in Models do
  begin
    if AModel.Contains(s) then
      Exit(True);
  end;
end;

procedure TRDOpenAI.RefreshChatCompletions;
begin
  CheckApiKey;
  CheckModel;
  CheckContentAndRole;
  if FResponse = nil then
  begin
    FResponse := TRESTResponse.Create(nil);
  end;
  FResponse.RootElement := '';
  if FRequest = nil then
  begin
    FRequest := CreateRequest;
  end;
  FRequest.Method := rmPOST;

  FRequest.Body.ClearBody;
  var
    s: string;
  s := FInputChatCompletion.AsJson;

  FRequest.Params.AddItem.Assign(FRESTRequestParameter);
  FRESTRequestParameter2.Value := s; // Body !
  FRequest.Params.AddItem.Assign(FRESTRequestParameter2);

  FRequest.Resource := GetEndPoint(meChatCompletions);

  FRequest.Response := FResponse;

  FBusy := True;

  if assigned(FRequestInfoProc) then
    FRequestInfoProc(FRequest.Resource, gfGet);

  if FAsynchronous then
  begin
    FRequest.ExecuteAsync(ChatCompletionCallback, True, True, DoCompletionHandlerWithError);
    Exit;
  end else begin
    FRequest.Execute;
    CompletionCallback;
  end;

end;

procedure TRDOpenAI.RefreshCompletions;
begin
  CheckApiKey;
  CheckModel;
  CheckQuestion;
  if FResponse = nil then
  begin
    FResponse := TRESTResponse.Create(nil);
  end;
  FResponse.RootElement := '';
  if FRequest = nil then
  begin
    FRequest := CreateRequest;
  end;
  FRequest.Method := rmPOST;

  FRequest.Body.ClearBody;
  var
    s: string;
  s := FQuestionSettings.AsJson;

  FRequest.Params.AddItem.Assign(FRESTRequestParameter);
  FRESTRequestParameter2.Value := s; // Body !
  FRequest.Params.AddItem.Assign(FRESTRequestParameter2);

  FRequest.Resource := GetEndPoint(meCompletions);

  FRequest.Response := FResponse;

  FBusy := True;

  if assigned(FRequestInfoProc) then
    FRequestInfoProc(FRequest.Resource, gfGet);

  if FAsynchronous then
  begin
    FRequest.ExecuteAsync(CompletionCallback, True, True, DoCompletionHandlerWithError);
    Exit;
  end else begin
    FRequest.Execute;
    CompletionCallback;
  end;

end;

procedure TRDOpenAI.RefreshDallEGenImage;
begin
  CheckApiKey;
  CheckModel;
  CheckDallEGenInput;
  if FResponse = nil then
  begin
    FResponse := TRESTResponse.Create(nil);
  end;
  FResponse.RootElement := '';
  if FRequest = nil then
  begin
    FRequest := CreateRequest;
  end;
  FRequest.Method := rmPOST;

  FRequest.Body.ClearBody;
  var
    s: string;
  s := FInputDallEImage.AsJson;

  FRequest.Params.AddItem.Assign(FRESTRequestParameter);
  FRESTRequestParameter2.Value := s; // Body !
  FRequest.Params.AddItem.Assign(FRESTRequestParameter2);

  FRequest.Resource := GetEndPoint(meDallEGen);
  FRequest.Response := FResponse;

  FBusy := True;

  if assigned(FRequestInfoProc) then
    FRequestInfoProc(FRequest.Resource, gfGet);

  if FAsynchronous then
  begin
    FRequest.ExecuteAsync(DallEGenImageCallback, True, True, DoCompletionHandlerWithError);
    Exit;
  end else begin
    FRequest.Execute;
    DallEGenImageCallback;
  end;
end;

procedure TRDOpenAI.RefreshInstrCompletions;
begin
  CheckApiKey;
  CheckModel;
  CheckInstruction;
  if FResponse = nil then
  begin
    FResponse := TRESTResponse.Create(nil);
  end;
  FResponse.RootElement := '';
  if FRequest = nil then
  begin
    FRequest := CreateRequest;
  end;
  FRequest.Method := rmPOST;

  FRequest.Body.ClearBody;
  var
    s: string;
  s := FInstructionSettings.AsJson;

  FRequest.Params.AddItem.Assign(FRESTRequestParameter);
  FRESTRequestParameter2.Value := s; // Body !
  FRequest.Params.AddItem.Assign(FRESTRequestParameter2);

  FRequest.Resource := GetEndPoint(meInstructions);
  FRequest.Response := FResponse;

  FBusy := True;

  if assigned(FRequestInfoProc) then
    FRequestInfoProc(FRequest.Resource, gfGet);

  if FAsynchronous then
  begin
    FRequest.ExecuteAsync(InstrCompletionCallback, True, True, DoCompletionHandlerWithError);
    Exit;
  end else begin
    FRequest.Execute;
    InstrCompletionCallback;
  end;

end;

procedure TRDOpenAI.RefreshModerations;
begin
  CheckApiKey;
  CheckModel;
  CheckModerationInput;
  if FResponse = nil then
  begin
    FResponse := TRESTResponse.Create(nil);
  end;
  FResponse.RootElement := '';
  if FRequest = nil then
  begin
    FRequest := CreateRequest;
  end;
  FRequest.Method := rmPOST;

  FRequest.Body.ClearBody;
  var
    s: string;
  s := FModerationInput.AsJson;

  FRequest.Params.AddItem.Assign(FRESTRequestParameter);
  FRESTRequestParameter2.Value := s; // Body !
  FRequest.Params.AddItem.Assign(FRESTRequestParameter2);

  FRequest.Resource := GetEndPoint(meModerations);
  FRequest.Response := FResponse;

  FBusy := True;

  if assigned(FRequestInfoProc) then
    FRequestInfoProc(FRequest.Resource, gfGet);

  if FAsynchronous then
  begin
    FRequest.ExecuteAsync(ModerationsCallback, True, True, DoCompletionHandlerWithError);
    Exit;
  end else begin
    FRequest.Execute;
    ModerationsCallback;
  end;

end;

procedure TRDOpenAI.RefreshModels;
begin
  CheckApiKey;
  if FResponse = nil then
  begin
    FResponse := TRESTResponse.Create(nil);
  end;
  FResponse.RootElement := '';
  if FRequest = nil then
  begin
    FRequest := CreateRequest;
  end;
  FRequest.Method := rmGET;

  FRequest.Body.ClearBody;

  FRequest.Params.AddItem.Assign(FRESTRequestParameter);

  FRequest.Resource := GetEndPoint(meModels);
  FRequest.Response := FResponse;

  FBusy := True;

  if assigned(FRequestInfoProc) then
    FRequestInfoProc(FRequest.Resource, gfGet);

  if FAsynchronous then
  begin
    FRequest.ExecuteAsync(ModelsCallback, True, True, DoCompletionHandlerWithError);
    Exit;
  end else begin
    FRequest.Execute;
    ModelsCallback;
  end;

end;

procedure TRDOpenAI.CompletionCallback;
var
  JsonObj: TJSONObject;
begin
  try
    if FRequest = nil then
      Exit;
    if FResponse = nil then
      Exit;

    if FResponse.StatusCode <> 200 then
    begin
      FLastError := FResponse.StatusText;
      DoError(FLastError);
      Exit;
    end;

    if assigned(FRequestInfoProc) then
      FRequestInfoProc(FRequest.Resource, gfFinish);

    JsonObj := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(FResponse.Content), 0) as TJSONObject;
    if JsonObj = nil then
      Exit;

    try
      try
        FreeAndNil(FCompletions);
        FCompletions := TJson.JsonToObject<TCompletions>(TJSONObject(JsonObj), cJSON_OPTIONS);
        DoCompletionsLoad(FCompletions);
        if (FCompletions <> nil) and (FCompletions.Choices.Count > 0) then
        begin
          case StrToFinishReason(FCompletions.Choices[0].FinishReason) of
            frStop, frLength:
              begin
                DoAnswer(FCompletions.Choices[0].Text);
              end;
          end;
        end;
      finally
        JsonObj.Free;
      end;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        DoError(FLastError);
      end;
    end;
  finally
    FBusy := False;
  end;
end;

procedure TRDOpenAI.InstrCompletionCallback;
var
  JsonObj: TJSONObject;
begin
  try
    if FRequest = nil then
      Exit;
    if FResponse = nil then
      Exit;

    if FResponse.StatusCode <> 200 then
    begin
      FLastError := FResponse.StatusText;
      DoError(FLastError);
      Exit;
    end;

    if assigned(FRequestInfoProc) then
      FRequestInfoProc(FRequest.Resource, gfFinish);

    JsonObj := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(FResponse.Content), 0) as TJSONObject;
    if JsonObj = nil then
      Exit;

    try
      try
        FreeAndNil(FInstrCompletions);
        FInstrCompletions := TJson.JsonToObject<TCompletions>(TJSONObject(JsonObj), cJSON_OPTIONS);
        DoInstrCompletionsLoad(FInstrCompletions);
        if (FInstrCompletions <> nil) and (FInstrCompletions.Choices.Count > 0) then
        begin
          DoAnswer(FInstrCompletions.Choices[0].Text);
        end;
      finally
        JsonObj.Free;
      end;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        DoError(FLastError);
      end;
    end;
  finally
    FBusy := False;
  end;
end;

procedure TRDOpenAI.ModelsCallback;
var
  JsonObj: TJSONObject;
begin
  try
    if FRequest = nil then
      Exit;
    if FResponse = nil then
      Exit;

    if FResponse.StatusCode <> 200 then
    begin
      FLastError := FResponse.StatusText;
      DoError(FLastError);
      Exit;
    end;

    if assigned(FRequestInfoProc) then
      FRequestInfoProc(FRequest.Resource, gfFinish);

    JsonObj := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(FResponse.Content), 0) as TJSONObject;
    if JsonObj = nil then
      Exit;

    try
      try
        FreeAndNil(FModels);
        FModels := TJson.JsonToObject<TModels>(TJSONObject(JsonObj), cJSON_OPTIONS);
        DoModelsLoad(FModels);
      finally
        JsonObj.Free;
      end;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        DoError(FLastError);
      end;
    end;

  finally
    FBusy := False;
  end;
end;

procedure TRDOpenAI.ModerationsCallback;
var
  JsonObj: TJSONObject;
begin
  try
    if FRequest = nil then
      Exit;
    if FResponse = nil then
      Exit;

    if FResponse.StatusCode <> 200 then
    begin
      FLastError := FResponse.StatusText;
      DoError(FLastError);
      Exit;
    end;

    if assigned(FRequestInfoProc) then
      FRequestInfoProc(FRequest.Resource, gfFinish);

    JsonObj := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(FResponse.Content), 0) as TJSONObject;
    if JsonObj = nil then
      Exit;

    try
      try
        FreeAndNil(FModerations);
        FModerations := TJson.JsonToObject<TModerations>(TJSONObject(JsonObj), cJSON_OPTIONS);
        DoModerationsLoad(FModerations);
      finally
        JsonObj.Free;
      end;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        DoError(FLastError);
      end;
    end;
  finally
    FBusy := False;
  end;
end;

procedure TRDOpenAI.ProtocolError(Sender: TCustomRESTRequest);
begin
  DoError(FLastError);
end;

procedure TRDOpenAI.ProtocolErrorClient(Sender: TCustomRESTClient);
begin
  DoError(FLastError);
end;

procedure TRDOpenAI.SetAsynchronous(const Value: Boolean);
begin
  if FAsynchronous <> Value then
  begin
    FAsynchronous := Value;
    if FRequest <> nil then
    begin
      FRequest.SynchronizedEvents := FAsynchronous;
    end;
  end;
end;

procedure TRDOpenAI.SetModerationInput(const Value: TModerationInput);
begin
  FModerationInput := Value;
end;

procedure TRDOpenAI.SetURL(const Value: string);
begin
  BaseURL := Value;
end;

function TRDOpenAI.RemoveEmptyLinesWithReturns(AText: string): string;
begin
  Result := AText;
  Result := Result.Trim([#13, #10]);
  Result := Result.Trim([#10, #13]);
end;

{ TRDChatGpt }

procedure TRDChatGpt.Ask(AQuestion: string);
begin
  FQuestionSettings.Prompt := AQuestion;
  FQuestionSettings.Model := FModel;
  FQuestionSettings.Temperature := FTemperature;
  FQuestionSettings.MaxTokens := FMaxTokens;
  Cancel;
  RefreshCompletions;
end;

procedure TRDChatGpt.Chat(AContent, ARole: String);
begin
  FInputChatCompletion.Model := FModel;
  FInputChatCompletion.Messages.Clear;
  var
    Msg: TMessage := TMessage.Create;
  Msg.Content := AContent;
  Msg.Role := ARole;
  FInputChatCompletion.Messages.Add(Msg);
  Cancel;
  RefreshChatCompletions;
end;

procedure TRDChatGpt.GenerateImage(APrompt, ASize, AFormat: string);
begin
  FInputDallEImage.Prompt := APrompt;
  FInputDallEImage.N := 1;
  FInputDallEImage.ResponseFormat := AFormat;
  FInputDallEImage.Size := ASize;
  Cancel;
  RefreshDallEGenImage;
end;

procedure TRDChatGpt.Instruct(AInput: String; AInstruction: String);
begin
  FInstructionSettings.Model := 'text-davinci-edit-001'; // hard coded
  FInstructionSettings.Input := AInput;
  FInstructionSettings.Instruction := AInstruction;
  Cancel;
  RefreshInstrCompletions;
end;

procedure TRDOpenAI.SetTimeOutSeconds(const Value: Integer);
begin
  if FTimeOutSeconds <> Value then
  begin
    FTimeOutSeconds := Value;
    if FRequest <> nil then
    begin
      FRequest.Timeout := FTimeOutSeconds * 1000;
    end;
  end;
end;

procedure TRDChatGpt.LoadModels;
begin
  RefreshModels;
end;

procedure TRDChatGpt.LoadModerations(AInput: string);
begin
  Cancel;
  if AInput <> '' then
  begin
    FModerationInput.Input := AInput;
  end;
  RefreshModerations;
end;

end.
