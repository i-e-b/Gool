using Gool.Parsers.Composite.Abstracts;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Composite;

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
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        var targetMatch = LeftParser.Parse(scan, previousMatch, allowAutoAdvance);

        if (!targetMatch.Success) return scan.NoMatch(this, previousMatch);

        // doing difference
        var refuseMatch = RightParser.Parse(scan, previousMatch, allowAutoAdvance);
        if (refuseMatch.Success)
        {
            // fail: must match left but NOT right
            return scan.NoMatch(this, targetMatch);
        }

        // Good match
        return targetMatch.Through(this, previousMatch);
    }

    /// <inheritdoc />
    public override bool IsOptional() => LeftParser.IsOptional();

    /// <inheritdoc />
    public override string ToString()
    {
        if (Tag is null) return LeftParser + " not " + RightParser;
        return LeftParser + " not " + RightParser + " Tag='" + Tag + "'";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        if (depth < 1) return GetType().Name;
        return LeftParser.ShortDescription(depth - 1) + " not " + RightParser.ShortDescription(depth - 1);
    }
}