using System.Linq;
using Phantom.Parsers.Interfaces;
using Phantom.Results;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// Parser that matches a single character <b>NOT</b> in a set
/// </summary>
public class ExcludingCharacterSet : Parser, IMatchingParser
{
    private readonly char[] _test;

    /// <summary>
    /// Parser that matches a single character <b>NOT</b> in a set
    /// </summary>
    public ExcludingCharacterSet(char[] c)
    {
        _test = c;
    }

    /// <inheritdoc />
    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;
        if (scan.EndOfInput(offset)) return scan.NoMatch(this, previousMatch);

        char c = scan.Peek(offset);

        if (_test.Contains(c)) return scan.NoMatch(this, previousMatch);

        // if we arrive at this point, we have a match
        return scan.CreateMatch(this, offset, 1);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "{NOT '" + string.Join("','",_test.Select(c=>c.ToString())) + "'}";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}