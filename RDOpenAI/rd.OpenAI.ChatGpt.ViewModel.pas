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
  private
    function StrToFinishReason(AValue: string): TFinishReason;
  private const
    cBEARER = 'Bearer';
    cDEF_MAX_TOKENS = 2048; // 1024
    cDEF_URL = 'https://api.openai.com/v1';
    cDEF_TEMP = 0.1;
    cDEF_MODEL = 'text-davinci-003';
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
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
  strict private
    FLastError: string;
    FResponse: TRESTResponse;
    FRequest: TRESTRequest;
    FOnAnswer: TMessageEvent;
    FOnError: TMessageEvent;
    FOnModelsLoaded: TTypedEvent<TModels>;
    FOnCompletionsLoaded: TTypedEvent<TCompletions>;
    FOnModerationsLoaded: TTypedEvent<TModerations>;

    FIgnoreReturns: Boolean;

    FCompletions: TCompletions;
    FModels: TModels;
    FModerations: TModerations;

    FRequestInfoProc: TRequestInfoProc;
    procedure ProtocolError(Sender: TCustomRESTRequest);
    procedure ProtocolErrorClient(Sender: TCustomRESTClient);
    function GetURL: string;
    procedure SetURL(const Value: string);
  protected
    FBusy: Boolean;
    FQuestionSettings: TQuestion;
    procedure RefreshCompletions;
    procedure CompletionCallback;

    procedure RefreshModels;
    procedure ModelsCallback;

    procedure RefreshModerations;
    procedure ModerationsCallback;

    function GetModels: TModels;
    function GetCompletions: TCompletions;
    function GetModerations: TModerations;
    function RemoveEmptyLinesWithReturns(AText: string): string;
  private
    FModerationInput: TModerationInput;
    FTimeOutSeconds: Integer;
    procedure CheckApiKey;
    procedure CheckModel;
    procedure CheckQuestion;
    procedure CheckModerationInput;
    procedure SetTimeOutSeconds(const Value: Integer);
  protected
    FQuestion: string;
    FAsynchronous: Boolean;
    FShowQuestionInAnswer: Boolean;
    procedure DoAnswer(AMessage: string); virtual;
    procedure DoError(AMessage: string); virtual;
    procedure DoModelsLoad(AModels: TModels); virtual;
    procedure DoCompletionsLoad(ACompletions: TCompletions); virtual;
    procedure DoModerationsLoad(AModerations: TModerations); virtual;

    procedure DoCompletionHandlerWithError(AObject: TObject);
  strict private
    procedure SetAsynchronous(const Value: Boolean);
    procedure SetQuestion(const Value: string);
    procedure SetModerationInput(const Value: TModerationInput);
  public
{$IFDEF MSWINDOWS}
    // not async
    // mobile stuff via events
    property Completions: TCompletions read GetCompletions;
    property Models: TModels read GetModels;
    property Moderations: TModerations read GetModerations;
{$ENDIF}
    property ModerationInput: TModerationInput read FModerationInput write SetModerationInput;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Cancel;
    procedure Assign(Source: TPersistent); override;
  published
    property URL: string read GetURL write SetURL stored True;
    property IgnoreReturns: Boolean read FIgnoreReturns write FIgnoreReturns default True;
    property OnAnswer: TMessageEvent read FOnAnswer write FOnAnswer;
    property OnError: TMessageEvent read FOnError write FOnError;
    property OnModelsLoaded: TTypedEvent<TModels> read FOnModelsLoaded write FOnModelsLoaded;
    property OnCompletionsLoaded: TTypedEvent<TCompletions> read FOnCompletionsLoaded write FOnCompletionsLoaded;
    property OnModerationsLoaded: TTypedEvent<TModerations> read FOnModerationsLoaded write FOnModerationsLoaded;
    property Asynchronous: Boolean read FAsynchronous write SetAsynchronous default
{$IFDEF MSWINDOWS}False{$ELSE}True{$ENDIF};
    property TimeOutSeconds: Integer read FTimeOutSeconds write SetTimeOutSeconds default 30;
    property ShowQuestionInAnswer: Boolean read FShowQuestionInAnswer write FShowQuestionInAnswer default False;
    property Question: string read FQuestion write SetQuestion;
  end;

  TRDChatGpt = class(TRDOpenAI)
  private
  public
    procedure Ask(AQuestion: string = '');
    procedure LoadModels;
    procedure LoadModerations(AInput: string = '');
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
  Self.Question := OpenAI.Question;
end;

procedure TRDOpenAI.Cancel;
begin
  FBusy := False;
  if FRequest <> nil then
  begin
    FRequest.Cancel;
  end;
end;

procedure TRDOpenAI.CheckApiKey;
begin
  if FApiKey = '' then
    raise RDOpenAIException.Create('ApiKey not set.');
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
  if FQuestion = '' then
    raise RDOpenAIException.Create('Question not set.');
end;

constructor TRDOpenAI.Create(AOwner: TComponent);
begin
  inherited;
  FRestClient.OnHTTPProtocolError := ProtocolErrorClient;
{$IFDEF MSWINDOWS}
  FAsynchronous := False;
{$ELSE}
  FAsynchronous := True;
{$ENDIF}
  FTimeOutSeconds := 30; // in seconds
  FShowQuestionInAnswer := False;
  URL := cDEF_URL;
  FQuestionSettings := TQuestion.Create;
  FModerationInput := TModerationInput.Create;
  FIgnoreReturns := True;
end;

destructor TRDOpenAI.Destroy;
begin
  Cancel;
  FreeAndNil(FQuestionSettings);
  FreeAndNil(FCompletions);
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

    if FShowQuestionInAnswer then
    begin
      AMessage := FQuestion + #13#10 + AMessage;
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

function TRDOpenAI.GetCompletions: TCompletions;
begin
  if FCompletions = nil then
  begin
    var
      WasAsync: Boolean := FAsynchronous;
      // not asynchronous in this case!
    Asynchronous := False;
    try
      RefreshCompletions;
    finally
      Asynchronous := WasAsync;
    end;
  end;
  Result := FCompletions;
end;

function TRDOpenAI.GetModels: TModels;
begin
  if FModels = nil then
  begin
    var
      WasAsync: Boolean := FAsynchronous;
      // not asynchronous in this case!
    Asynchronous := False;
    try
      RefreshModels;
    finally
      Asynchronous := WasAsync;
    end;
  end;
  Result := FModels;
end;

function TRDOpenAI.GetModerations: TModerations;
begin
  if FModerations = nil then
  begin
    var
      WasAsync: Boolean := FAsynchronous;
      // not asynchronous in this case!
    Asynchronous := False;
    try
      RefreshModerations;
    finally
      Asynchronous := WasAsync;
    end;
  end;
  Result := FModerations;
end;

function TRDOpenAI.GetURL: string;
begin
  Result := BaseURL;
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
    FRequest := TRESTRequest.Create(nil);
    FRequest.OnHTTPProtocolError := ProtocolError;
    FRequest.Client := FRestClient;
    FRequest.SynchronizedEvents := FAsynchronous;
    FRequest.Timeout := FTimeOutSeconds * 1000;
  end;
  FRequest.Method := rmPOST;

  FRequest.Body.ClearBody;
  var
    s: string;
  s := FQuestionSettings.AsJson;

  FRequest.Params.AddItem.Assign(FRESTRequestParameter);
  FRESTRequestParameter2.Value := s; // Body !
  FRequest.Params.AddItem.Assign(FRESTRequestParameter2);

  FRequest.Resource := 'completions';
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
    FRequest := TRESTRequest.Create(nil);
    FRequest.OnHTTPProtocolError := ProtocolError;
    FRequest.Client := FRestClient;
    FRequest.SynchronizedEvents := FAsynchronous;
    FRequest.Timeout := FTimeOutSeconds * 1000;
  end;
  FRequest.Method := rmPOST;

  FRequest.Body.ClearBody;
  var
    s: string;
  s := FModerationInput.AsJson;

  FRequest.Params.AddItem.Assign(FRESTRequestParameter);
  FRESTRequestParameter2.Value := s; // Body !
  FRequest.Params.AddItem.Assign(FRESTRequestParameter2);

  FRequest.Resource := 'moderations';
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
    FRequest := TRESTRequest.Create(nil);
    FRequest.OnHTTPProtocolError := ProtocolError;
    FRequest.Client := FRestClient;
    FRequest.SynchronizedEvents := FAsynchronous;
    FRequest.Timeout := FTimeOutSeconds * 1000;
  end;
  FRequest.Method := rmGET;

  FRequest.Body.ClearBody;

  FRequest.Params.AddItem.Assign(FRESTRequestParameter);

  FRequest.Resource := 'models';
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
  if AQuestion <> '' then
  begin
    Question := AQuestion;
  end;
  Cancel;
  RefreshCompletions;
end;

procedure TRDOpenAI.SetQuestion(const Value: string);
begin
  if FQuestion <> Value then
  begin
    FQuestion := Value;
    FQuestionSettings.Prompt := FQuestion;
    FQuestionSettings.Model := FModel;
    FQuestionSettings.Temperature := FTemperature;
    FQuestionSettings.MaxTokens := FMaxTokens;
  end;
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
