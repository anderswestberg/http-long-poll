unit ChangeRecordPool;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections,
  System.Variants;

type
  TChangeRecord = class
    ChangeId: Int64;
    Timestamp: TDateTime;
    Key: string;
    Value: Variant;
    SourceId: string;
    procedure Clear;
  end;

  TChangeRecordPool = class
  private
    const
      MAX_POOL_SIZE = 5000; // Maximum number of records to keep in the pool
    var
      FPool: TObjectList<TChangeRecord>;
      FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    
    /// <summary>
    /// Gets a TChangeRecord from the pool or creates a new one if pool is empty
    /// </summary>
    function Acquire: TChangeRecord;
    
    /// <summary>
    /// Returns a TChangeRecord to the pool. If pool is at max size, the record is freed
    /// </summary>
    procedure Release(ARecord: TChangeRecord);
    
    /// <summary>
    /// Returns current number of records in the pool
    /// </summary>
    function PoolSize: Integer;
  end;

var
  _ChangeRecordPool: TChangeRecordPool;

implementation

procedure TChangeRecord.Clear;
begin
  ChangeId := 0;
  Timestamp := 0;
  Key := '';
  VarClear(Value);
  SourceId := '';
end;

{ TChangeRecordPool }

constructor TChangeRecordPool.Create;
begin
  inherited Create;
  FPool := TObjectList<TChangeRecord>.Create(True); // Owns objects
  FLock := TCriticalSection.Create;
end;

destructor TChangeRecordPool.Destroy;
begin
  FLock.Free;
  FPool.Free;
  inherited;
end;

function TChangeRecordPool.Acquire: TChangeRecord;
begin
  FLock.Acquire;
  try
    if FPool.Count > 0 then
    begin
      Result := FPool.Last;
      FPool.Delete(FPool.Count - 1);
    end
    else
      Result := TChangeRecord.Create;
    
    Result.Clear; // Ensure the record is clean
  finally
    FLock.Release;
  end;
end;

procedure TChangeRecordPool.Release(ARecord: TChangeRecord);
begin
  if ARecord = nil then
    Exit;
    
  FLock.Acquire;
  try
    if FPool.Count < MAX_POOL_SIZE then
      FPool.Add(ARecord)
    else
      ARecord.Free;
  finally
    FLock.Release;
  end;
end;

function TChangeRecordPool.PoolSize: Integer;
begin
  FLock.Acquire;
  try
    Result := FPool.Count;
  finally
    FLock.Release;
  end;
end;

initialization
  _ChangeRecordPool := TChangeRecordPool.Create;

finalization
  _ChangeRecordPool.Free;

end. 