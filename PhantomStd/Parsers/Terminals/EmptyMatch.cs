using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that represents no input.
/// Always returns an empty success match
/// </summary>
public class EmptyMatch : Parser
{
    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        return scan.CreateMatch(this, previousMatch?.Right ?? 0, 0);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "(empty)";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}