using System.Globalization;

namespace TestLanguageImplementation.Interpreted;

/// <summary>
/// Value for a variable
/// </summary>
public class Value
{
    public Value(double value)
    {
        NumericValue = value;
        StringValue = value.ToString(CultureInfo.InvariantCulture);
        Kind = ValueKind.Numeric;
    }

    public Value(string value)
    {
        StringValue = value;
        Kind = ValueKind.String;
    }

    public Value()
    {
        Kind = ValueKind.Invalid;
    }

    /// <summary>
    /// Type of the value
    /// </summary>
    public ValueKind Kind { get; set; }

    /// <summary>
    /// Numeric value. Not defined if type is not numeric
    /// </summary>
    public double NumericValue { get; set; }

    /// <summary>
    /// String value. Not defined if type is not string
    /// </summary>
    public string StringValue { get; set; } = "";

    /// <summary>
    /// Boolean logic value. Not defined if type is not boolean
    /// </summary>
    public bool BoolValue { get; set; }

    /// <summary>
    /// Cast the value to string
    /// </summary>
    public override string ToString()
    {
        return Kind switch
        {
            ValueKind.Invalid => "<invalid>",
            ValueKind.Numeric => NumericValue.ToString(CultureInfo.InvariantCulture),
            ValueKind.String => StringValue,
            ValueKind.Boolean => BoolValue.ToString(),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    /// <summary>
    /// Cast the value to boolean
    /// </summary>
    /// <returns></returns>
    /// <exception cref="NotImplementedException"></exception>
    public bool ToBool()
    {
        return Kind switch
        {
            ValueKind.Invalid => false,
            ValueKind.Numeric => NumericValue is > 0.00001 or < -0.00001,
            ValueKind.String => !string.IsNullOrWhiteSpace(StringValue),
            ValueKind.Boolean => BoolValue,
            _ => throw new ArgumentOutOfRangeException()
        };
    }
}