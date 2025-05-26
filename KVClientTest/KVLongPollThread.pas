unit KVLongPollThread;

interface

uses
  System.Classes, KVHttpClient, System.SysUtils;

type
  TChangeBatchProc = procedure(const Changes: TArray<TChangeItem>) of object;

  TKVLongPollThread = class(TThread)
  private
    FClient: TKVHttpClient;
    FLastId: Int64;
    FOnUpdate: TChangeBatchProc;
    FStopRequested: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Client: TKVHttpClient; StartId: Int64; OnUpdate: TChangeBatchProc);
    procedure Stop;
  end;

implementation

constructor TKVLongPollThread.Create(Client: TKVHttpClient; StartId: Int64; OnUpdate: TChangeBatchProc);
begin
  inherited Create(False); // auto start
  FreeOnTerminate := False;
  FClient := Client;
  FLastId := StartId;
  FOnUpdate := OnUpdate;
  FStopRequested := False;
end;

procedure TKVLongPollThread.Stop;
begin
  FStopRequested := True;
  if Assigned(FClient) then
    FClient.CancelLongPoll;
end;

procedure TKVLongPollThread.Execute;
var
  Batch: TArray<TChangeItem>;
  i: Integer;
begin
  while not Terminated and not FStopRequested do
  begin
    try
      if FClient.LongPoll(FLastId, Batch) then
      begin
        if Length(Batch) > 0 then
        begin
          // Update FLastId to highest returned
          for i := 0 to High(Batch) do
            if Batch[i].Id > FLastId then
              FLastId := Batch[i].Id;
          if Assigned(FOnUpdate) then
            Synchronize(procedure
              begin
                FOnUpdate(Batch);
              end);
        end;
      end;
    except
      on E: Exception do
        if not FStopRequested and Assigned(FOnUpdate) then
          Synchronize(procedure
            begin
              FOnUpdate([]);
            end);
    end;
    if not FStopRequested then
      Sleep(10);
  end;
end;

end.

