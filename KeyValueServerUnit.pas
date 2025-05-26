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

  TKeyValueServer = class
  private
    FData: TDictionary<string, Variant>;
    FDataLock: TCriticalSection;
    FOnValueChanged: TKeyValueChangedEvent;
    // Change log
    FChangeLog: TList<TChangeRecord>;
    FNextChangeId: Int64;
    procedure AddChange(const Key: string; const Value: Variant; const SourceId: string);
    procedure PruneChangeLog;
  public
    constructor Create;
    destructor Destroy; override;

    function GetValue(const Key: string; out Value: Variant): Boolean;
    procedure SetValue(const Key: string; const Value: Variant; const SourceId: string = '');
    procedure GetAll(var Output: TArray<string>);
    procedure GetChangesSince(LastId: Int64; const SourceId: string; out Changes: TArray<TChangeRecord>);

    property OnValueChanged: TKeyValueChangedEvent read FOnValueChanged write FOnValueChanged;
    property Data: TDictionary<string, Variant> read FData;
    property DataLock: TCriticalSection read FDataLock;
  end;

implementation

const
  CHANGELOG_PRUNE_COUNT = 2000; // How many to keep
  CHANGELOG_MAX_RESULTS = 1000;

constructor TKeyValueServer.Create;
begin
  inherited;
  FData := TDictionary<string, Variant>.Create;
  FDataLock := TCriticalSection.Create;
  FChangeLog := TList<TChangeRecord>.Create;
  FNextChangeId := 1;
end;

destructor TKeyValueServer.Destroy;
begin
  FChangeLog.Free;
  FDataLock.Free;
  FData.Free;
  inherited;
end;

function TKeyValueServer.GetValue(const Key: string; out Value: Variant): Boolean;
begin
  FDataLock.Acquire;
  try
    Result := FData.TryGetValue(Key, Value);
  finally
    FDataLock.Release;
  end;
end;

procedure TKeyValueServer.SetValue(const Key: string; const Value: Variant; const SourceId: string = '');
var
  Changed: Boolean;
  OldValue: Variant;
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

procedure TKeyValueServer.GetAll(var Output: TArray<string>);
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

procedure TKeyValueServer.AddChange(const Key: string; const Value: Variant; const SourceId: string);
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

procedure TKeyValueServer.PruneChangeLog;
begin
  while FChangeLog.Count > CHANGELOG_PRUNE_COUNT do
    FChangeLog.Delete(0); // delete oldest
end;

procedure TKeyValueServer.GetChangesSince(LastId: Int64; const SourceId: string; out Changes: TArray<TChangeRecord>);
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

end.

