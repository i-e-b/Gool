using Phantom.Results;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// A placeholder that represents no parser.
/// </summary>
public class NullParser : IParser
{
    /// <inheritdoc />
    public ParserMatch Parse(IScanner scan, ParserMatch? previousMatch = null)
    {
        throw new System.InvalidOperationException();
    }

    /// <inheritdoc />
    public string? Tag { get; set; }
    
    /// <inheritdoc />
    public ScopeType Scope { get; set; }

    /// <inheritdoc />
    public bool HasMetaData() => false;

    /// <inheritdoc />
    public string ShortDescription(int depth) => "Null parser";
}