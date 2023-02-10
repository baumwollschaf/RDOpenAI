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
  TGetOrFinish = (gfGet, gfFinish);
  TRequestInfoProc = procedure(AURL: string; AGetOrFinish: TGetOrFinish) of object;
  TMessageEvent = procedure(Sender: TObject; AMessage: string) of object;

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

    FIgnoreReturns: Boolean;

    FCompletions: TCompletions;
    FModels: TModels;
    FRequestInfoProc: TRequestInfoProc;
    function GetURL: string;
    procedure SetURL(const Value: string);
  protected
    FBusy: Boolean;
    FQuestionSettings: TQuestion;
    procedure RefreshCompletions;
    procedure CompletionCallback;
    procedure ModelsCallback;
    function GetModels: TModels;
    procedure RefreshModels;
    function RemoveEmptyLinesWithReturns(AText: string): string;
  private
    procedure CheckApiKey;
    procedure CheckModel;
  protected
    FQuestion: string;
    FAsynchronous: Boolean;
    FShowQuestionInAnswer: Boolean;
    procedure DoAnswer(AMessage: string); virtual;
    procedure DoError(AMessage: string); virtual;
  strict private
    function GetCompletions: TCompletions;
    procedure SetAsynchronous(const Value: Boolean);
    procedure SetQuestion(const Value: string);
  public
    property Completions: TCompletions read GetCompletions;
    property Models: TModels read GetModels;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; virtual; abstract;
  published
    property URL: string read GetURL write SetURL stored True;
    property IgnoreReturns: Boolean read FIgnoreReturns write FIgnoreReturns default True;
    property OnAnswer: TMessageEvent read FOnAnswer write FOnAnswer;
    property OnError: TMessageEvent read FOnError write FOnError;
    property Asynchronous: Boolean read FAsynchronous write SetAsynchronous default False;
    property ShowQuestionInAnswer: Boolean read FShowQuestionInAnswer write FShowQuestionInAnswer default False;
    property Question: string read FQuestion write SetQuestion;
  end;

  TRDChatGpt = class(TRDOpenAI)
  public
    procedure Execute; override;
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
  FRESTRequestParameter2.ContentTypeStr := 'application/json';

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
    Exit(frStop);
  if AValue = 'length' then
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

procedure TRDOpenAI.CheckApiKey;
begin
  if FApiKey = '' then
    raise Exception.Create('ApiKey not set.');
end;

procedure TRDOpenAI.CheckModel;
begin
  if FModel = '' then
    raise Exception.Create('Model not set.');
end;

constructor TRDOpenAI.Create(AOwner: TComponent);
begin
  inherited;
  FAsynchronous := False;
  FShowQuestionInAnswer := False;
  URL := cDEF_URL;
  FQuestionSettings := TQuestion.Create;
  FIgnoreReturns := True;
end;

destructor TRDOpenAI.Destroy;
begin
  FreeAndNil(FQuestionSettings);
  FreeAndNil(FCompletions);
  FreeAndNil(FModels);
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

function TRDOpenAI.GetCompletions: TCompletions;
begin
  if FCompletions = nil then
  begin
    RefreshCompletions;
  end;
  Result := FCompletions;
end;

function TRDOpenAI.GetModels: TModels;
begin
  if FModels = nil then
  begin
    RefreshModels;
  end;
  Result := FModels;
end;

function TRDOpenAI.GetURL: string;
begin
  Result := BaseURL;
end;

procedure TRDOpenAI.RefreshCompletions;
begin
  CheckApiKey;
  CheckModel;
  if FResponse = nil then
  begin
    FResponse := TRESTResponse.Create(nil);
  end;
  FResponse.RootElement := '';
  if FRequest = nil then
  begin
    FRequest := TRESTRequest.Create(nil);
    FRequest.Client := FRestClient;
    FRequest.SynchronizedEvents := FAsynchronous;
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
    FRequest.ExecuteAsync(CompletionCallback);
    Exit;
  end
  else
  begin
    FRequest.Execute;
    CompletionCallback;
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
    FRequest.Client := FRestClient;
    FRequest.SynchronizedEvents := FAsynchronous;
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
    FRequest.ExecuteAsync(ModelsCallback);
    Exit;
  end
  else
  begin
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
      finally
        JsonObj.Free;
      end;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
      end;
    end;
  finally
    FBusy := False;
  end;
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

procedure TRDOpenAI.SetURL(const Value: string);
begin
  BaseURL := Value;
end;

function TRDOpenAI.RemoveEmptyLinesWithReturns(AText: string): string;
begin
  Result := AText;
  Result := Result.Replace(#13#10, '', [rfReplaceAll]);
  Result := Result.Replace(#13, '', [rfReplaceAll]);
  Result := Result.Replace(#10, '', [rfReplaceAll]);
end;

{ TRDChatGpt }

procedure TRDChatGpt.Execute;
begin
  if FBusy then
  begin
{$IFDEF DEBUG}
    Beep;
{$ENDIF}
    Exit;
  end;
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

end.
