using System.Collections.Generic;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that matches characters from the unicode WhiteSpace category.
/// </summary>
public class IdentifierString : Parser
{
    private readonly bool _allowUnderscore;
    private readonly bool _allowHyphen;

    /// <summary>
    /// Parser that matches a single whitespace character
    /// </summary>
    public IdentifierString(bool allowUnderscore, bool allowHyphen)
    {
        _allowUnderscore = allowUnderscore;
        _allowHyphen = allowHyphen;
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;
        var result = scan.EmptyMatch(this, previousMatch?.Right ?? 0, previousMatch); // empty match with this parser

        int count = 0;

        while (!scan.EndOfInput(result.Right))
        {
            bool valid = true;
            var c = scan.Peek(offset);
            if (c == 0) break; // ran off end

            if (c == '_')
            {
                if (!_allowUnderscore) break;
            }
            else if (c is >= 'a' and <= 'z')
            {
            }
            else if (c is >= 'A' and <= 'Z')
            {
            }
            else if (c is >= '0' and <= '9')
            {
                if (count == 0) break;
            }
            else if (c is '-')
            {
                if (count == 0) break;
                valid = false;
            }
            else
            {
                break;
            }

            count++;
            offset++;
            if (valid) result.ExtendTo(offset); // only grow the result if we have a valid end character.
        }

        return count < 1 ? scan.NoMatch(this, result) : result.Through(this, previousMatch);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override bool IsOptional() => false;

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "<ID>";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}