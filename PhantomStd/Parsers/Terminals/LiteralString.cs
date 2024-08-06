using Phantom.Parsers.Interfaces;
using Phantom.Results;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// Parser that matches an exact string sequence
/// </summary>
public class LiteralString : Parser, IMatchingParser
{
    private readonly string _test;

    /// <summary>
    /// Parser that matches an exact string sequence
    /// </summary>
    public LiteralString(string toMatch)
    {
        _test = toMatch;
    }

    /// <summary>
    /// Gets the literal string that this parser test for.
    /// </summary>
    public string MatchLiteral => _test;

    /// <inheritdoc />
    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;

        var compare = scan.Substring(offset, _test.Length);

        return compare == _test
            ? scan.CreateMatch(this, offset, _test.Length)
            : scan.NoMatch;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "\"" + _test + "\"";

        if (TagValue is null) return desc;
        return desc + " Tag='" + TagValue + "'";
    }
}