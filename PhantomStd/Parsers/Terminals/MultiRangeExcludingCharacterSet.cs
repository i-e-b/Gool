using System.Collections.Generic;
using System.Linq;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Match a single character OUTSIDE ALL of the inclusive ranges
/// </summary>
public class MultiRangeExcludingCharacterSet : Parser
{
    private readonly BNF.CharacterRange[] _ranges;

    /// <summary>
    /// Match a single character that is in NONE of the given ranges
    /// </summary>
    public MultiRangeExcludingCharacterSet(BNF.CharacterRange[] ranges)
    {
        _ranges = ranges;
    }

    /// <inheritdoc />
    public override bool IsOptional() => false;

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(offset)) return scan.NoMatch(this, previousMatch);

        char c = scan.Peek(offset);

        foreach (var range in _ranges)
        {
            if (c >= range.Lower && c <= range.Upper) return scan.NoMatch(this, previousMatch);
        }

        // None of the ranges matched
        return scan.CreateMatch(this, offset, 1, previousMatch);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "[^" + string.Join("", _ranges.Select(r => r.Lower + '-' + r.Upper)) + ']';

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}