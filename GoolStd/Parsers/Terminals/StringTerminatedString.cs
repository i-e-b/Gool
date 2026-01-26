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
    private readonly string[]         _terminators;
    private readonly bool             _endOfInputIsMatch;
    private readonly StringComparison _comparisonType;

    /// <summary>
    /// Parser that matches any of the exact string sequences provided
    /// </summary>
    public StringTerminatedString(string[] terminators, bool endOfInputIsMatch = false, StringComparison comparisonType = StringComparison.Ordinal)
    {
        _terminators = terminators;
        _endOfInputIsMatch = endOfInputIsMatch;
        _comparisonType = comparisonType;
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        var offset = previousMatch?.Right ?? 0;

        foreach (var terminator in _terminators)
        {
            var index = scan.IndexOf(offset, terminator, _comparisonType);

            if (index >= offset) return scan.CreateMatch(this, offset, index - offset, previousMatch);
        }

        if (_endOfInputIsMatch && scan.RemainingAfter(offset) > 0)
        {
            return scan.CreateMatch(this, offset, scan.RemainingAfter(offset), previousMatch);
        }

        return scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "Str(...'"+string.Join("', '", _terminators)+"')";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}