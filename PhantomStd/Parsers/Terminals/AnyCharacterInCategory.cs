﻿using System.Globalization;
using Gool.Parsers.Interfaces;
using Gool.Results;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that will match any one character.
/// </summary>
public class AnyCharacterInCategory : Parser, IMatchingParser
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
    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(offset)) return scan.NoMatch(this, previousMatch);

        char c = scan.Peek(offset);

        if (char.GetUnicodeCategory(c) != _category) return scan.NoMatch(this, previousMatch);

        // if we arrive at this point, we have a match
        return scan.CreateMatch(this, offset, 1);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "["+_category+"]";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}