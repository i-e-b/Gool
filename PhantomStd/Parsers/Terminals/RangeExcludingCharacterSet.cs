using System.Collections.Generic;
using System.Linq;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Match a single character that is in a range, and is not in the list of exclusions
/// </summary>
public class RangeExcludingCharacterSet : Parser
{
    private readonly char _lower;
    private readonly char _upper;
    private readonly char[] _exclusions;

    /// <summary>
    /// Match a single character that is between <paramref name="lower"/>
    /// and <paramref name="upper"/> (inclusive), which is not in the list of exclusions
    /// </summary>
    public RangeExcludingCharacterSet(char lower, char upper, params char[] exclusions)
    {
        _lower = lower;
        _upper = upper;
        _exclusions = exclusions;
    }

    /// <inheritdoc />
    public override bool IsOptional() => false;

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(offset)) return scan.NoMatch(this, previousMatch);

        char c = scan.Peek(offset);

        
        if (c < _lower || c > _upper|| _exclusions.Contains(c)) return scan.NoMatch(this, previousMatch);

        // if we arrive at this point, we have a match
        return scan.CreateMatch(this, offset, 1);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "["+_lower+"-"+_upper+"'; NOT "
                   + string.Join("",_exclusions.Select(c=>c.ToString())) + "']";

        if (Tag is null) return desc;
        return desc + " Tag=‘" + Tag + "’";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}