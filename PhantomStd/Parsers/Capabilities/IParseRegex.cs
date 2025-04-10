using Gool.Results;

namespace Gool.Parsers.Capabilities;

/// <summary>
/// Parsers that can expose internals of a regular expression
/// </summary>
public interface IParseRegex
{
    /// <summary>
    /// Return parsed value as an integer
    /// </summary>
    public System.Text.RegularExpressions.Match RegexMatch(ParserMatch match);
}