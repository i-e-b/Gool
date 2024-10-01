using Gool.Parsers.Composite.Abstracts;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Transforms;

/// <summary>
/// Inspect the last matched character against a parser pattern.
/// <p/>
/// This is non-consuming
/// </summary>
public class PreviousCharacterCheck : Unary
{
    /// <summary>
    /// Inspect the last character of the previous parser match
    /// against a parser pattern.
    /// </summary>
    public PreviousCharacterCheck(IParser parser) : base(parser) { }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        // Walk back until null or non-empty.
        while (previousMatch is not null && previousMatch.Length < 1)
        {
            previousMatch = previousMatch.Previous;
        }

        // Didn't find any previous characters
        if (previousMatch is null) return scan.NoMatch(this, null);

        var fakeRight = previousMatch.Right - 2; // back up 1 for the last char, another to be to the left of it
        var check     = Parser.Parse(scan, scan.CreateMatch(this, fakeRight, 1, null));

        return check.Success
            ? scan.CreateMatch(this, previousMatch.Right, 0, previousMatch)
            : scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override bool IsOptional() => false;

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "<--[" + Parser + "]";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return "<--[" + Parser.ShortDescription(depth) + "]";
    }
}