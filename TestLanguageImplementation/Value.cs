namespace TestLanguageImplementation;

/// <summary>
/// Value for a variable
/// </summary>
public class Value
{
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
}