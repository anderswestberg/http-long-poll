unit KeyValueStore;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs, System.Variants,
  SeqLogger, ChangeRecordPool;

type

  TKeyValueChangedEvent = procedure(Sender: TObject; const Key: string; const Value: Variant; const SourceId: string) of object;

  IKeyValueStore = interface
    ['{F8F7B2A1-D3E4-4C5D-B6A7-8C9D0E1F2A3B}']
    function GetValue(const Key: string; out Value: Variant): Boolean;
    procedure SetValue(const Key: string; const Value: Variant; const SourceId: string = '');
    procedure SetValues(const Updates: array of TPair<string, Variant>; const SourceId: string = '');
    procedure GetAll(var Output: TArray<TPair<string, variant>>);
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
    procedure GetAll(var Output: TArray<TPair<string, variant>>);
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
  TSeqLogger.Logger.Log(Information, 'Key/Value store created');
end;

destructor TDictionaryKeyValueStore.Destroy;
var
  i: Integer;
begin
  TSeqLogger.Logger.Log(Information, 'Key/Value store destroyed');
  // Return all change records to the pool
  for i := 0 to FChangeLog.Count - 1 do
    _ChangeRecordPool.Release(FChangeLog[i]);
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
  StoredValue: Variant;
begin
  FDataLock.Acquire;
  try
    StoredValue := Value;
    Changed := (not FData.ContainsKey(Key)) or (FData[Key] <> Value);
    FData.AddOrSetValue(Key, Value);

    if Changed then
    begin
      AddChange(Key, StoredValue, SourceId);
      TSeqLogger.Logger.Log(Information, 'Store: Value changed key={Key} value={Value} source={Source}', ['Key', Key, 'Value', VarToStr(StoredValue), 'Source', SourceId]);
    end;
  finally
    FDataLock.Release;
  end;
  if Changed and Assigned(FOnValueChanged) then
    FOnValueChanged(Self, Key, StoredValue, SourceId);
end;

procedure TDictionaryKeyValueStore.SetValues(const Updates: array of TPair<string, Variant>; const SourceId: string = '');
var
  i: Integer;
  Changed: Boolean;
  StoredValue: Variant;
begin
  if Length(Updates) = 0 then
    Exit;

  TSeqLogger.Logger.Log(Information, 'Store: Batch update of {Count} values from source {Source}', ['Count', Length(Updates), 'Source', SourceId]);

  FDataLock.Acquire;
  try
    for i := 0 to High(Updates) do
    begin
      StoredValue := Updates[i].Value;
      Changed := (not FData.ContainsKey(Updates[i].Key)) or (FData[Updates[i].Key] <> Updates[i].Value);
      FData.AddOrSetValue(Updates[i].Key, Updates[i].Value);

      if Changed then
      begin
        AddChange(Updates[i].Key, StoredValue, SourceId);
        TSeqLogger.Logger.Log(Information, 'Store: Batch value changed key={Key} value={Value} source={Source}',
          ['Key', Updates[i].Key, 'Value', VarToStr(Updates[i].Value), 'Source', SourceId]);
      end;
    end;
  finally
    FDataLock.Release;
  end;

  // Notify changes outside the lock
  if Assigned(FOnValueChanged) then
    for i := 0 to High(Updates) do
      FOnValueChanged(Self, Updates[i].Key, StoredValue, SourceId);
end;

procedure TDictionaryKeyValueStore.GetAll(var Output: TArray<TPair<string, variant>>);
var
  Pair: TPair<string, Variant>;
  List: TList<TPair<string, variant>>;
begin
  List := TList<TPair<string, variant>>.Create;
  FDataLock.Acquire;
  try
    for Pair in FData do
      List.Add(Pair);
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
  Change := _ChangeRecordPool.Acquire;
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
var
  PrunedCount, i: Integer;
  OldRecords: TArray<TChangeRecord>;
begin
  PrunedCount := FChangeLog.Count - CHANGELOG_PRUNE_COUNT;
  if PrunedCount > 0 then
  begin
    // Save references to records we'll remove
    SetLength(OldRecords, PrunedCount);
    for i := 0 to PrunedCount - 1 do
      OldRecords[i] := FChangeLog[i];

    // Remove old records from changelog
    while FChangeLog.Count > CHANGELOG_PRUNE_COUNT do
      FChangeLog.Delete(0);

    // Return old records to the pool
    for i := 0 to PrunedCount - 1 do
      _ChangeRecordPool.Release(OldRecords[i]);

    TSeqLogger.Logger.Log(Information, 'Store: Pruned {Count} old changes from changelog', ['Count', PrunedCount]);
  end;
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
    TSeqLogger.Logger.Log(Information, 'Store: Retrieved {Count} changes since ID {LastId} for source {Source}',
      ['Count', Length(Changes), 'LastId', LastId, 'Source', SourceId]);
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

