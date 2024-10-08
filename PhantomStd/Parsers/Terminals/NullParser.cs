using System.Collections.Generic;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// A placeholder that represents no parser.
/// </summary>
public class NullParser : IParser
{
    private readonly string _source;

    /// <summary>
    /// Named null-parser
    /// </summary>
    public NullParser(string source)
    {
        _source = source;
    }

    /// <inheritdoc />
    public bool IsOptional() => false;

    /// <inheritdoc />
    public ParserMatch Parse(IScanner scan, ParserMatch? previousMatch = null, bool allowAutoAdvance = true)
    {
        throw new System.InvalidOperationException();
    }

    /// <inheritdoc />
    public IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public string? Tag { get; set; }
    
    /// <inheritdoc />
    public ScopeType Scope { get; set; }

    /// <inheritdoc />
    public bool HasMetaData() => false;

    /// <inheritdoc />
    public override string ToString() => $"Null parser from {_source}";

    /// <inheritdoc />
    public string ShortDescription(int depth) => $"Null parser from {_source}";
}