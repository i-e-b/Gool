using System.Collections.Generic;
using System.Linq;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that matches a single character from a set
/// </summary>
public class LiteralCharacterSet : Parser
{
    private readonly char[] _test;
    private readonly char   _lowest;
    private readonly char   _highest;

    /// <summary>
    /// Parser that matches a single character from a set
    /// </summary>
    public LiteralCharacterSet(params char[] c)
    {
        _test = c;
        _lowest = _test.Min();
        _highest = _test.Max();
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        var offset = previousMatch?.Right ?? 0;

        char c = scan.Peek(offset);
        if (c == 0 || c < _lowest || c > _highest) return scan.NoMatch(this, previousMatch); // can't be in any of the ranges

        if (!_test.Contains(c)) return scan.NoMatch(this, previousMatch);

        // if we arrive at this point, we have a match
        return scan.CreateMatch(this, offset, 1, previousMatch);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "[" + string.Join("",_test.Select(c=>c.ToString())) + "]";

        if (Tag is null) return desc;
        return desc + " Tag=‘" + Tag + "’";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}