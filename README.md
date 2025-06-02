# HTTP Key-Value Store

A Delphi library that provides a key-value store with HTTP long-polling capabilities for real-time updates. The library is built with flexibility in mind, allowing custom key-value store implementations while providing a default in-memory implementation.

## Features

- In-memory key-value store with support for various data types (strings, numbers, booleans, JSON)
- HTTP long-polling mechanism for real-time updates
- Support for batch operations
- Thread-safe operations
- Extensible architecture through interfaces
- Change tracking with source identification
- Client ID support to prevent echo effects
- Automatic connection management and retry

## Installation

1. Add the following files to your project:
   - `src/server/HttpServer.pas`
   - `src/server/KeyValueHttpBridge.pas`
   - `src/server/KeyValueStore.pas`
   - `src/client/KeyValueClient.pas`
   - `src/client/KeyValueHttpClient.pas`
   - `src/common/SeqLogger.pas`

2. Ensure you have the following dependencies:
   - Indy components (IndySystem, IndyCore, IndyProtocols)
   - mORMot framework (SynCommons)

Alternatively, you can install the `HttpKeyValueStore` package which contains all the necessary components.

## Quick Start

### Server Side
```delphi
uses
  KeyValueStore, KeyValueHttpBridge;

var
  Store: IKeyValueStore;
  Bridge: TKeyValueHttpBridge;
begin
  // Create the key-value store
  Store := CreateDefaultKeyValueStore;
  
  // Create and start the HTTP bridge on port 8868
  Bridge := TKeyValueHttpBridge.Create(8868, Store);
  Bridge.Start;
  try
    // Use the store...
  finally
    Bridge.Free;
  end;
end;
```

### Client Side
```delphi
uses
  KeyValueClient, KeyValueHttpClient;

var
  Client: TKeyValueClient;
begin
  Client := TKeyValueClient.Create('http://localhost:8868');
  try
    // The client automatically connects and maintains connection
    Client.OnStateChange := HandleStateChange;
    Client.OnValueChange := HandleValueChange;
    
    // Use the client
    Client.SetValue('myKey', 'myValue');
    Value := Client.GetValue('myKey');
  finally
    Client.Free;
  end;
end;
```

## HTTP API

### Get Value
```
GET /values?key=<key>&clientId=<clientId>
```
Response:
```json
{
  "value": "myValue"
}
```

### Set Value
```
POST /values?clientId=<clientId>
Content-Type: application/json

{
  "key": "myKey",
  "value": "myValue"
}
```
Response:
```json
{
  "status": "ok"
}
```

### Batch Update
```
POST /values/batch?clientId=<clientId>
Content-Type: application/json

[
  {"key": "key1", "value": "value1"},
  {"key": "key2", "value": 42}
]
```
Response:
```json
{
  "status": "ok"
}
```

### Get All Values
```
GET /values/all?clientId=<clientId>
```
Response:
```json
[
  {
    "key": "key1",
    "value": "value1"
  },
  {
    "key": "key2",
    "value": 42
  }
]
```

### Get Latest Change ID
```
GET /changes/latest?clientId=<clientId>
```
Response:
```json
{
  "id": 123
}
```

### Long Polling for Changes
```
GET /changes?since=<lastChangeId>&clientId=<clientId>
```
Response:
```json
[
  {
    "id": 123,
    "key": "myKey",
    "value": "myValue",
    "timestamp": "2024-03-21T15:30:45Z"
  }
]
```

## IKeyValueStore Interface

The key-value store interface has been updated to include batch operations and better change tracking:

```delphi
type
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
```

## Client Features

The client implementation (`TKeyValueClient`) provides:

- Automatic connection management
- Automatic reconnection on failure
- Thread-safe operations
- Batch operations support
- Real-time updates through long polling
- Client state management (Connected/Connecting/Disconnected)
- Event-based notification of changes
- Configurable retry intervals

## Thread Safety

Both server and client implementations are fully thread-safe:

- Server uses critical sections to protect the key-value store and change log
- Client uses critical sections for all operations
- Long polling is handled in separate threads
- Connection management runs in a dedicated thread
- Event notifications are properly queued to the main thread

## Change Tracking

The system maintains a robust change tracking mechanism:
- Unique monotonic change IDs
- Timestamps for each change
- Source ID tracking to prevent echo effects
- Support for batch changes
- Configurable change log size (default: 2000 entries)
- Maximum changes per request (default: 1000)

## Performance Considerations

The library includes performance optimizations:
- Batch operations for better throughput
- Connection pooling in the HTTP client
- Configurable timeouts and intervals
- Efficient change tracking with pruning
- Optimized long polling with proper cancellation

## Demo Applications

1. `demo/KeyValueServerDemo`: Server demo showing:
   - Basic server setup
   - Key-value store operations
   - Change monitoring

2. `demo/KeyValueClientDemo`: Client demo featuring:
   - Connection management
   - Real-time updates
   - Stress testing capabilities
   - Batch operations
   - Performance monitoring

## License

MIT
