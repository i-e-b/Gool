using System;
using Gool.Parsers.Composite.Abstracts;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Transforms;

/// <summary>
/// Inspect the previous non-empty parser match
/// against a parser pattern.
/// <p/>
/// This is non-consuming
/// </summary>
public class PreviousMatchCheck : Unary
{
    private readonly bool _matchEntire;
    private readonly Func<ParserMatch, ParserMatch?> _select;

    /// <summary>
    /// Inspect the last character of the previous parser match
    /// against a parser pattern.
    /// </summary>
    public PreviousMatchCheck(Func<ParserMatch, ParserMatch?>? select, IParser parser, bool matchEntire) : base(parser)
    {
        _matchEntire = matchEntire;
        _select = select ?? (match => match);
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        // Walk back until null or non-empty.
        while (previousMatch is not null && previousMatch.Length < 1)
        {
            previousMatch = previousMatch.Previous;
        }

        // Didn't find any previous characters
        if (previousMatch is null) return scan.NoMatch(this, null);


        var fragment = _select(previousMatch);
        if (fragment is null) return scan.NoMatch(this, previousMatch);

        var startOfPattern = scan.CreateMatch(this, fragment.Offset, 0, null);

        var check = Parser.Parse(scan, startOfPattern, allowAutoAdvance);
        if (!check.Success) return scan.NoMatch(this, previousMatch);

        if (_matchEntire) return check.Length >= fragment.Length ? scan.CreateMatch(this, previousMatch.Right, 0, previousMatch) : scan.NoMatch(this, previousMatch);
        return scan.CreateMatch(this, previousMatch.Right, 0, previousMatch);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "<--{" + Parser + "}";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return "<--{" + Parser.ShortDescription(depth) + "}";
    }
}