unit KeyValueServerUnit;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs, System.Variants;

type
  TChangeRecord = class
    ChangeId: Int64;
    Timestamp: TDateTime;
    Key: string;
    Value: Variant;
    SourceId: string;
  end;

  TKeyValueChangedEvent = procedure(Sender: TObject; const Key: string; const Value: Variant; const SourceId: string) of object;

  IKeyValueStore = interface
    ['{F8F7B2A1-D3E4-4C5D-B6A7-8C9D0E1F2A3B}']
    function GetValue(const Key: string; out Value: Variant): Boolean;
    procedure SetValue(const Key: string; const Value: Variant; const SourceId: string = '');
    procedure SetValues(const Updates: array of TPair<string, Variant>; const SourceId: string = '');
    procedure GetAll(var Output: TArray<string>);
    procedure GetChangesSince(LastId: Int64; const SourceId: string; out Changes: TArray<TChangeRecord>);
    function GetOnValueChanged: TKeyValueChangedEvent;
    procedure AddChange(const Key: string; const Value: Variant; const SourceId: string);
    procedure SetOnValueChanged(const Value: TKeyValueChangedEvent);
    function GetLatestChangeId: Int64;
    property OnValueChanged: TKeyValueChangedEvent read GetOnValueChanged write SetOnValueChanged;
  end;

  TDictionaryKeyValueStore = class(TInterfacedObject, IKeyValueStore)
  private
    FData: TDictionary<string, Variant>;
    FDataLock: TCriticalSection;
    FOnValueChanged: TKeyValueChangedEvent;
    // Change log
    FChangeLog: TList<TChangeRecord>;
    FNextChangeId: Int64;
    procedure PruneChangeLog;
  protected
    function GetOnValueChanged: TKeyValueChangedEvent;
    procedure SetOnValueChanged(const Value: TKeyValueChangedEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChange(const Key: string; const Value: Variant; const SourceId: string);

    // IKeyValueStore implementation
    function GetValue(const Key: string; out Value: Variant): Boolean;
    procedure SetValue(const Key: string; const Value: Variant; const SourceId: string = '');
    procedure SetValues(const Updates: array of TPair<string, Variant>; const SourceId: string = '');
    procedure GetAll(var Output: TArray<string>);
    procedure GetChangesSince(LastId: Int64; const SourceId: string; out Changes: TArray<TChangeRecord>);
    function GetLatestChangeId: Int64;

    property OnValueChanged: TKeyValueChangedEvent read GetOnValueChanged write SetOnValueChanged;
    property Data: TDictionary<string, Variant> read FData;
    property DataLock: TCriticalSection read FDataLock;
  end;

// Factory function to create default key/value store implementation
function CreateDefaultKeyValueStore: IKeyValueStore;

implementation

const
  CHANGELOG_PRUNE_COUNT = 2000; // How many to keep
  CHANGELOG_MAX_RESULTS = 1000;

function CreateDefaultKeyValueStore: IKeyValueStore;
begin
  Result := TDictionaryKeyValueStore.Create;
end;

{ TDictionaryKeyValueStore }

constructor TDictionaryKeyValueStore.Create;
begin
  inherited;
  FData := TDictionary<string, Variant>.Create;
  FDataLock := TCriticalSection.Create;
  FChangeLog := TList<TChangeRecord>.Create;
  FNextChangeId := 1;
end;

destructor TDictionaryKeyValueStore.Destroy;
begin
  FChangeLog.Free;
  FDataLock.Free;
  FData.Free;
  inherited;
end;

function TDictionaryKeyValueStore.GetValue(const Key: string; out Value: Variant): Boolean;
begin
  FDataLock.Acquire;
  try
    Result := FData.TryGetValue(Key, Value);
  finally
    FDataLock.Release;
  end;
end;

procedure TDictionaryKeyValueStore.SetValue(const Key: string; const Value: Variant; const SourceId: string = '');
var
  Changed: Boolean;
begin
  FDataLock.Acquire;
  try
    Changed := (not FData.ContainsKey(Key)) or (FData[Key] <> Value);
    FData.AddOrSetValue(Key, Value);
    if Changed then
      AddChange(Key, Value, SourceId);
  finally
    FDataLock.Release;
  end;
  if Changed and Assigned(FOnValueChanged) then
    FOnValueChanged(Self, Key, Value, SourceId);
end;

procedure TDictionaryKeyValueStore.SetValues(const Updates: array of TPair<string, Variant>; const SourceId: string = '');
var
  i: Integer;
  Changed: Boolean;
begin
  if Length(Updates) = 0 then
    Exit;

  FDataLock.Acquire;
  try
    for i := 0 to High(Updates) do
    begin
      Changed := (not FData.ContainsKey(Updates[i].Key)) or (FData[Updates[i].Key] <> Updates[i].Value);
      FData.AddOrSetValue(Updates[i].Key, Updates[i].Value);
      if Changed then
        AddChange(Updates[i].Key, Updates[i].Value, SourceId);
    end;
  finally
    FDataLock.Release;
  end;

  // Notify changes outside the lock
  if Assigned(FOnValueChanged) then
    for i := 0 to High(Updates) do
      FOnValueChanged(Self, Updates[i].Key, Updates[i].Value, SourceId);
end;

procedure TDictionaryKeyValueStore.GetAll(var Output: TArray<string>);
var
  Pair: TPair<string, Variant>;
  List: TList<string>;
begin
  List := TList<string>.Create;
  FDataLock.Acquire;
  try
    for Pair in FData do
      List.Add(Pair.Key + '=' + VarToStr(Pair.Value));
    Output := List.ToArray;
  finally
    FDataLock.Release;
    List.Free;
  end;
end;

procedure TDictionaryKeyValueStore.AddChange(const Key: string; const Value: Variant; const SourceId: string);
var
  Change: TChangeRecord;
begin
  Change := TChangeRecord.Create;
  Change.ChangeId := FNextChangeId;
  Inc(FNextChangeId);
  Change.Timestamp := Now;
  Change.Key := Key;
  Change.Value := Value;
  Change.SourceId := SourceId;
  FChangeLog.Add(Change);
  PruneChangeLog;
end;

procedure TDictionaryKeyValueStore.PruneChangeLog;
begin
  while FChangeLog.Count > CHANGELOG_PRUNE_COUNT do
    FChangeLog.Delete(0); // delete oldest
end;

procedure TDictionaryKeyValueStore.GetChangesSince(LastId: Int64; const SourceId: string; out Changes: TArray<TChangeRecord>);
var
  i, cnt: Integer;
  Tmp: TList<TChangeRecord>;
begin
  Tmp := TList<TChangeRecord>.Create;
  FDataLock.Acquire;
  try
    cnt := 0;
    for i := 0 to FChangeLog.Count-1 do
      if (FChangeLog[i].ChangeId > LastId) and (FChangeLog[i].SourceId <> SourceId) then
      begin
        Tmp.Add(FChangeLog[i]);
        Inc(cnt);
        if cnt >= CHANGELOG_MAX_RESULTS then
          Break;
      end;
    Changes := Tmp.ToArray;
  finally
    FDataLock.Release;
    Tmp.Free;
  end;
end;

function TDictionaryKeyValueStore.GetOnValueChanged: TKeyValueChangedEvent;
begin
  Result := FOnValueChanged;
end;

procedure TDictionaryKeyValueStore.SetOnValueChanged(const Value: TKeyValueChangedEvent);
begin
  FOnValueChanged := Value;
end;

function TDictionaryKeyValueStore.GetLatestChangeId: Int64;
begin
  FDataLock.Acquire;
  try
    if FChangeLog.Count > 0 then
      Result := FChangeLog[FChangeLog.Count - 1].ChangeId
    else
      Result := 0;
  finally
    FDataLock.Release;
  end;
end;

end.

