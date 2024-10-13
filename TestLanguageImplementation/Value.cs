using System.Globalization;

namespace TestLanguageImplementation;

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


    public override string ToString()
    {
        switch (Kind)
        {
            case ValueKind.Invalid:
                return "<invalid>";
            case ValueKind.Numeric:
                return NumericValue.ToString(CultureInfo.InvariantCulture);
            case ValueKind.String:
                return "'" + StringValue + "'";
            default:
                throw new ArgumentOutOfRangeException();
        }
    }
}