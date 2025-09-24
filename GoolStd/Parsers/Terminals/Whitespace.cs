using System.Collections.Generic;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that matches characters from the unicode WhiteSpace category.
/// </summary>
public class Whitespace : Parser
{
    /// <summary>
    /// Maximum number of characters to match
    /// </summary>
    private readonly int _upperBound;

    /// <summary>
    /// Minimum number of characters to make a match
    /// </summary>
    private readonly int _lowerBound;

    /// <summary>
    /// Parser that matches a single whitespace character
    /// </summary>
    public Whitespace()
    {
        _upperBound = 1;
        _lowerBound = 1;
    }

    /// <summary>
    /// Parser that matches a range of whitespace
    /// </summary>
    public Whitespace(int min, int max)
    {
        _lowerBound = min;
        _upperBound = max;
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        var offset = previousMatch?.Right ?? 0;
        var result = scan.EmptyMatch(this, previousMatch?.Right ?? 0, previousMatch); // empty match with this parser

        int count = 0;

        while (count < _upperBound && !scan.EndOfInput(result.Right))
        {
            var isWhiteSpace = char.IsWhiteSpace(scan.Peek(offset));
            if (!isWhiteSpace) break; // no more matches

            count++;
            offset++;
            result.ExtendTo(offset);
        }

        if (count < _lowerBound || count > _upperBound)
        {
            return scan.NoMatch(this, result);
        }

        return result.Through(this, previousMatch);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "<ws>";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}