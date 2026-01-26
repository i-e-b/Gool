using System.Collections.Generic;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that matches a single character
/// </summary>
public class LiteralCharacter : Parser
{
    private readonly char _test;

    /// <summary>
    /// Parser that matches a single character from a set
    /// </summary>
    public LiteralCharacter(char c)
    {
        _test = c;
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        var offset = previousMatch?.Right ?? 0;

        if (scan.Peek(offset) == _test) return scan.CreateMatch(this, offset, 1, previousMatch);

        return scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "'" + _test + "'";

        if (Tag is null) return desc;
        return desc + " Tag=‘" + Tag + "’";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}