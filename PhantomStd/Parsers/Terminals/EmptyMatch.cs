using Phantom.Parsers.Interfaces;
using Phantom.Results;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// Parser that represents no input.
/// Always returns an empty success match
/// </summary>
public class EmptyMatch : Parser, IMatchingParser
{
    /// <inheritdoc />
    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        return scan.CreateMatch(this, previousMatch?.Right ?? 0, 0);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "(empty)";

        if (TagValue is null) return desc;
        return desc + " Tag='" + TagValue + "'";
    }
}