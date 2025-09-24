using System;
using System.Collections.Generic;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that will match any one character that matches the predicate function
/// </summary>
public class CharacterPredicate : Parser
{
    private readonly Func<char, bool> _predicate;

    /// <summary>
    /// Create a parser with a predicate function
    /// </summary>
    public CharacterPredicate(Func<char,bool> predicate)
    {
        _predicate = predicate;
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        var offset = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(offset)) return scan.NoMatch(this, previousMatch);

        char c = scan.Peek(offset);

        if (!_predicate(c)) return scan.NoMatch(this, previousMatch);

        // if we arrive at this point, we have a match
        return scan.CreateMatch(this, offset, 1, previousMatch);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "[?]";

        if (Tag is null) return desc;
        return desc + " Tag=‘" + Tag + "’";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}