using System;
using System.Collections.Generic;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that matches any input upto but not including a terminating string
/// </summary>
public class StringTerminatedString : Parser
{
    private readonly string           _terminator;
    private readonly StringComparison _comparisonType;

    /// <summary>
    /// Parser that matches an exact string sequence
    /// </summary>
    public StringTerminatedString(string terminator, StringComparison comparisonType = StringComparison.Ordinal)
    {
        _terminator = terminator;
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

        var index = scan.IndexOf(offset, _terminator, _comparisonType);

        return index >= offset
            ? scan.CreateMatch(this, offset, index - offset, previousMatch)
            : scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "Str(..."+_terminator+")";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}