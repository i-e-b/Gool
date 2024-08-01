using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite;

/// <summary>
/// Create an Difference parser from two sub-parsers.
/// Should match left but not right.
/// </summary>
public class Difference : Binary
{
    /// <summary>
    /// Create an Difference parser from two sub-parsers.
    /// Should match left but not right.
    /// </summary>
    public Difference(IParser left, IParser right)
        : base(left, right)
    {
    }

    /// <inheritdoc />
    public override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var m = LeftParser.Parse(scan, previousMatch);

        if (!m.Success) return scan.NoMatch;

        // doing difference
        var m2 = RightParser.Parse(scan, m);
        if (m2.Success)
        {
            // fail: must match left but NOT right
            return scan.NoMatch;
        }

        // Good match
        return m;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        if (TagValue is null) return LeftParser + " not " + RightParser;
        return LeftParser + " not " + RightParser + " Tag='" + TagValue + "'";
    }
}