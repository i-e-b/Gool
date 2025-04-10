using Gool.Results;

namespace Gool.Parsers.Capabilities;

/// <summary>
/// Capability to produce a numeric output from parser match
/// </summary>
public interface IParseNumericValue
{
    /// <summary>
    /// Return parsed value as an integer
    /// </summary>
    public long AsInteger(ParserMatch match);

    /// <summary>
    /// Return parsed value as an floating point value
    /// </summary>
    public double AsFloat(ParserMatch match);
}