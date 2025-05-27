# HTTP Long Poll Key-Value Store

A Delphi library that provides a key-value store with HTTP long-polling capabilities for real-time updates. The library is built with flexibility in mind, allowing custom key-value store implementations while providing a default in-memory implementation.

## Features

- In-memory key-value store with support for various data types (strings, numbers, booleans, JSON)
- HTTP long-polling mechanism for real-time updates
- Support for batch operations
- Thread-safe operations
- Extensible architecture through interfaces
- Change tracking with source identification
- Client ID support to prevent echo effects
- Automatic connection management
- Built with Indy components for HTTP server functionality

## Installation

1. Add the following files to your project:
   - `src/HttpServerUnit.pas`
   - `src/KeyValueHTTPBridgeUnit.pas`
   - `src/KeyValueServerUnit.pas`

2. Ensure you have the following dependencies:
   - Indy components (IndySystem, IndyCore, IndyProtocols)
   - mORMot framework (SynCommons, SynLZ)

3. Add the library's package (`HttpLongPoll.dpk`) to your project if you want to use it as a runtime package.

## Quick Start

```delphi
uses
  KeyValueServerUnit, KeyValueHTTPBridgeUnit;

var
  KVStore: IKeyValueStore;
  Bridge: TKeyValueHTTPBridge;
begin
  // Create the key-value store
  KVStore := CreateDefaultKeyValueStore;
  
  // Create and start the HTTP bridge on port 8080
  Bridge := TKeyValueHTTPBridge.Create(8080, KVStore);
  Bridge.Start;
  try
    // Use the store...
  finally
    Bridge.Free;
  end;
end;
```

## HTTP API

### Get Value
```
GET /data?key=<key>
```
Response:
```json
{
  "key": "myKey",
  "value": "myValue"
}
```

### Set Value
```
POST /data
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
POST /batch
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

### Long Polling
```
GET /longpoll?since=<lastChangeId>
X-Client-ID: <clientId>
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

## Custom Key-Value Store Implementation

You can create your own key-value store implementation by implementing the `IKeyValueStore` interface:

```delphi
type
  IKeyValueStore = interface
    ['{F8F7B2A1-D3E4-4C5D-B6A7-8C9D0E1F2A3B}']
    function GetValue(const Key: string; out Value: Variant): Boolean;
    procedure SetValue(const Key: string; const Value: Variant; const SourceId: string = '');
    procedure SetValues(const Updates: array of TPair<string, Variant>; const SourceId: string = '');
    procedure GetAll(var Output: TArray<string>);
    procedure GetChangesSince(LastId: Int64; const SourceId: string; out Changes: TArray<TChangeRecord>);
    function GetOnValueChanged: TKeyValueChangedEvent;
    procedure SetOnValueChanged(const Value: TKeyValueChangedEvent);
    property OnValueChanged: TKeyValueChangedEvent read GetOnValueChanged write SetOnValueChanged;
  end;
```

Example usage with a custom implementation:

```delphi
type
  TMyCustomStore = class(TInterfacedObject, IKeyValueStore)
    // Implement the interface methods...
  end;

var
  CustomStore: IKeyValueStore;
  Bridge: TKeyValueHTTPBridge;
begin
  CustomStore := TMyCustomStore.Create;
  Bridge := TKeyValueHTTPBridge.Create(8080, CustomStore);
  Bridge.Start;
  try
    // Use the custom store...
  finally
    Bridge.Free;
  end;
end;
```

## Thread Safety

The default implementation (`TDictionaryKeyValueStore`) is thread-safe, using a critical section to protect access to the underlying dictionary and change log. Custom implementations should also ensure thread safety as the HTTP bridge handles requests in separate threads.

## Change Tracking

The library maintains a change log to support long-polling operations. Changes are tracked with:
- Unique change IDs
- Timestamps
- Source IDs (to prevent echo effects)
- Key and value pairs

The default implementation keeps the last 2000 changes and returns up to 1000 changes per request.

## Client Implementation

For client implementations, consider:
1. Generate a unique client ID
2. Include the client ID in requests via the `X-Client-ID` header
3. Track the last seen change ID for long-polling requests
4. Handle connection timeouts and reconnection logic

See the `demo/KVClientTest` directory for a complete client implementation example.

## Demo Applications

The library includes two demo applications:
1. `demo/TestLongPoll`: A server application demonstrating the key-value store
2. `demo/KVClientTest`: A client application showing how to interact with the server

## License

[Your license information here]
