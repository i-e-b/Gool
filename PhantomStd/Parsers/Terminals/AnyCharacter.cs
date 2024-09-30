using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that will match any one character.
/// </summary>
public class AnyCharacter : Parser
{
    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var left = previousMatch?.Right ?? 0;
        return scan.EndOfInput(left)
            ? scan.NoMatch(this, previousMatch)
            : scan.CreateMatch(this, left, 1);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = ".";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}