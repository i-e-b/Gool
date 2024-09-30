using System;
using System.Collections.Generic;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that matches an exact string sequence
/// </summary>
public class LiteralString : Parser
{
    private readonly string           _test;
    private readonly StringComparison _comparisonType;

    /// <summary>
    /// Parser that matches an exact string sequence
    /// </summary>
    public LiteralString(string toMatch, StringComparison comparisonType = StringComparison.Ordinal)
    {
        _test = toMatch;
        _comparisonType = comparisonType;
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override bool IsOptional() => false;

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;

        var compare = scan.Substring(offset, _test.Length);

        return compare.Equals(_test, _comparisonType)
            ? scan.CreateMatch(this, offset, _test.Length)
            : scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "\"" + _test + "\"";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}