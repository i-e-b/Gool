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
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        var offset = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(offset)) return scan.NoMatch(this, previousMatch);

        var c = scan.Peek(offset);

        return char.GetUnicodeCategory(c) != _category
            ? scan.NoMatch(this, previousMatch)
            : scan.CreateMatch(this, offset, 1, previousMatch);
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