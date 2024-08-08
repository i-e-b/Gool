using Phantom.Parsers.Composite.Abstracts;
using Phantom.Results;

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
        var targetMatch = LeftParser.Parse(scan, previousMatch);

        if (!targetMatch.Success) return scan.NoMatch;

        // doing difference
        var refuseMatch = RightParser.Parse(scan, previousMatch);
        if (refuseMatch.Success)
        {
            // fail: must match left but NOT right
            return scan.NoMatch;
        }

        // Good match
        return targetMatch.Through(this);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        if (Tag is null) return LeftParser + " not " + RightParser;
        return LeftParser + " not " + RightParser + " Tag='" + Tag + "'";
    }
}