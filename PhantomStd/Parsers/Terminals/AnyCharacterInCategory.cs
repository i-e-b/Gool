using System.Collections.Generic;
using System.Globalization;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that will match any one character.
/// </summary>
public class AnyCharacterInCategory : Parser
{
    private readonly UnicodeCategory _category;

    /// <summary>
    /// Create a matcher for a single unicode category.
    /// </summary>
    public AnyCharacterInCategory(UnicodeCategory category)
    {
        _category = category;
    }

    /// <inheritdoc />
    public override bool IsOptional() => false;

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(offset)) return scan.NoMatch(this, previousMatch);

        char c = scan.Peek(offset);

        if (char.GetUnicodeCategory(c) != _category) return scan.NoMatch(this, previousMatch);

        // if we arrive at this point, we have a match
        return scan.CreateMatch(this, offset, 1, previousMatch);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "["+_category+"]";

        if (Tag is null) return desc;
        return desc + " Tag=‘" + Tag + "’";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}