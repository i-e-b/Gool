using System.Collections.Generic;
using System.Linq;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Match a single character inside any of the inclusive ranges
/// </summary>
public class MultiRangeCharacterSet : Parser
{
    private readonly BNF.CharacterRange[] _ranges;

    private readonly char _lowest;
    private readonly char _highest;

    /// <summary>
    /// Match a single character that is in any of the given ranges
    /// </summary>
    public MultiRangeCharacterSet(BNF.CharacterRange[] ranges)
    {
        _ranges = ranges;
        _lowest = ranges.Min(r => r.Lower);
        _highest = ranges.Max(r => r.Upper);
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        var offset = previousMatch?.Right ?? 0;

        var c = scan.Peek(offset);
        if (c == 0 || c < _lowest || c > _highest) return scan.NoMatch(this, previousMatch); // can't be in any of the ranges

        foreach (var range in _ranges)
        {
            if (c >= range.Lower && c <= range.Upper) return scan.CreateMatch(this, offset, 1, previousMatch);
        }

        // None of the ranges matched
        return scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers()
    {
        yield break;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = '[' + string.Join("", _ranges.Select(r => r.ToString())) + ']';

        if (Tag is null) return desc;
        return desc + " Tag=‘" + Tag + "’";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}