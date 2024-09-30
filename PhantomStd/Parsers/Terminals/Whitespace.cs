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
    public int UpperBound { get; }

    /// <summary>
    /// Minimum number of characters to make a match
    /// </summary>
    public int LowerBound { get; }

    /// <summary>
    /// Parser that matches a single whitespace character
    /// </summary>
    public Whitespace()
    {
        UpperBound = 1;
        LowerBound = 1;
    }

    /// <summary>
    /// Parser that matches a range of whitespace
    /// </summary>
    public Whitespace(int min, int max)
    {
        LowerBound = min;
        UpperBound = max;
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;
        var result = scan.EmptyMatch(this, previousMatch?.Right ?? 0); // empty match with this parser

        int count = 0;

        while (count < UpperBound && !scan.EndOfInput(result.Right))
        {
            var isWhiteSpace = char.IsWhiteSpace(scan.Peek(offset));
            if (!isWhiteSpace) break; // no more matches

            count++;
            offset++;
            result.ExtendTo(offset);
        }

        if (count < LowerBound || count > UpperBound)
        {
            return scan.NoMatch(this, result);
        }

        return result.Through(this);
    }

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