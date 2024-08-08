using Phantom.Parsers.Interfaces;
using Phantom.Results;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// Parser that matches a single character from a set
/// </summary>
public class Whitespace : Parser, IMatchingParser
{
    /// <summary>
    /// Parser that matches a single exact character
    /// </summary>
    public Whitespace() { }

    /// <inheritdoc />
    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(offset)) return scan.NoMatch;

        if (char.IsWhiteSpace(scan.Peek(offset))) return scan.NoMatch;

        // if we arrive at this point, we have a match
        return scan.CreateMatch(this, offset, 1);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "<ws>";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }
}