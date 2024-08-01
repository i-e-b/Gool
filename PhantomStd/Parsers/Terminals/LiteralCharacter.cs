using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// Parser that matches a single exact character
/// </summary>
public class LiteralCharacter : Parser, IMatchingParser
{
    private readonly char _test;

    /// <summary>
    /// Parser that matches a single exact character
    /// </summary>
    public LiteralCharacter(char c)
    {
        _test = c;
    }

    /// <inheritdoc />
    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(offset)) return scan.NoMatch;

        char c = scan.Peek(offset);

        if (c != _test) return scan.NoMatch;

        // if we arrive at this point, we have a match
        return scan.CreateMatch(this, offset, 1);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "'" + _test + "'";

        if (TagValue is null) return desc;
        return desc + " Tag='" + TagValue + "'";
    }
}