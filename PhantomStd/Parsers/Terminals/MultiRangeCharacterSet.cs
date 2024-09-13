using System.Linq;
using Gool.Parsers.Interfaces;
using Gool.Results;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Match a single character inside any of the inclusive ranges
/// </summary>
public class MultiRangeCharacterSet : Parser, IMatchingParser
{
    private readonly BNF.CharacterRange[] _ranges;

    /// <summary>
    /// Match a single character that is in any of the given ranges
    /// </summary>
    public MultiRangeCharacterSet(BNF.CharacterRange[] ranges)
    {
        _ranges = ranges;
    }

    /// <inheritdoc />
    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(offset)) return scan.NoMatch(this, previousMatch);

        char c = scan.Peek(offset);

        foreach (var range in _ranges)
        {
            if (c >= range.Lower && c <= range.Upper) return scan.CreateMatch(this, offset, 1);
        }

        // None of the ranges matched
        return scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = '[' + string.Join("", _ranges.Select(r => r.Lower + '-' + r.Upper)) + ']';

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}