using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// Parser that will match any one character.
/// </summary>
public class AnyCharacter : Parser, IMatchingParser
{
    /// <inheritdoc />
    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var left = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(left)) return scan.NoMatch;

        var m = scan.CreateMatch(this, left, 1);
        return m;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = ".";

        if (TagValue is null) return desc;
        return desc + " Tag='" + TagValue + "'";
    }
}