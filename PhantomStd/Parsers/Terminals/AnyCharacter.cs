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
        return scan.EndOfInput(left)
            ? scan.NoMatch
            : scan.CreateMatch(this, left, 1);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = ".";

        if (TagValue is null) return desc;
        return desc + " Tag='" + TagValue + "'";
    }
}