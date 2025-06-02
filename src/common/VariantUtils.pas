unit VariantUtils;

interface

uses
  System.SysUtils, System.Variants, SynCommons;

/// <summary>
/// Converts a string value to the most appropriate variant type:
/// - Integer if the string can be parsed as Int64
/// - Float if the string can be parsed as Double
/// - Boolean if the string is 'true' or 'false' (case-insensitive)
/// - JSON object/array if the string starts/ends with {}/[]
/// - String otherwise
/// </summary>
function StringToTypedVariant(const Value: string; out ConversionError: string): Variant;

/// <summary>
/// Returns a string representation of a variant value with its type
/// </summary>
function VariantToStringWithType(const Value: Variant): string;

implementation

function StringToTypedVariant(const Value: string; out ConversionError: string): Variant;
var
  IntValue: Int64;
  FloatValue: Double;
begin
  ConversionError := '';

  // Try integer first
  if TryStrToInt64(Value, IntValue) then
  begin
    Result := IntValue;
    Exit;
  end;

  // Try float
  if TryStrToFloat(Value, FloatValue) then
  begin
    Result := FloatValue;
    Exit;
  end;

  // Try boolean
  if SameText(Value, 'true') then
  begin
    Result := True;
    Exit;
  end;
  if SameText(Value, 'false') then
  begin
    Result := False;
    Exit;
  end;

  // Try JSON
  if (Value.StartsWith('{') and Value.EndsWith('}')) or
     (Value.StartsWith('[') and Value.EndsWith(']')) then
  begin
    try
      Result := _JSON(RawUTF8(Value)); // Use _JSON to directly create a variant
      Exit;
    except
      on E: Exception do
      begin
        ConversionError := Format('Failed to parse JSON: %s', [E.Message]);
        Result := Value; // Fall back to string
      end;
    end;
  end;

  // Default to string
  Result := Value;
end;

function VariantToStringWithType(const Value: Variant): string;
begin
  Result := Format('%s (%s)', [VarToStr(Value), VarTypeAsText(VarType(Value))]);
end;

end. 