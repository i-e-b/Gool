using Gool.Parsers.Composite.Abstracts;
using Gool.Parsers.Terminals;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Composite;

/// <summary>
/// Creates a delimited list parser from two sub-parsers.
/// The list expects one or more of left parser, each
/// terminated by a single occurrence of the right parser.
/// The final element may NOT exclude it's terminator.
/// </summary>
public class TerminatedList : Binary
{
    /// <summary>
    /// Creates a delimited list parser from two sub-parsers.
    /// The list expects one or more of left parser, each
    /// terminated by a single occurrence of the right parser.
    /// The final element may NOT exclude it's terminator.
    /// </summary>
    public TerminatedList(IParser item, IParser terminator)
        : base(item, terminator)
    {
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        var result = scan.NoMatch(this, previousMatch);

        while (!scan.EndOfInput(result.Right))
        {
            var item = LeftParser.Parse(scan, result, allowAutoAdvance);

            if (!item.Success)
            {
                return result.Through(this, previousMatch);
            }

            var terminator = RightParser.Parse(scan, item, allowAutoAdvance);

            if (!terminator.Success)
            {
                return result.Through(this, previousMatch);
            }

            result = ParserMatch.Join(previousMatch, new NullParser(nameof(TerminatedList)), result, item);
            result = ParserMatch.Join(previousMatch, new NullParser(nameof(TerminatedList)), result, terminator);
        }

        return result.Through(this, previousMatch);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = LeftParser + " < " + RightParser;

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }
	
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        if (depth < 1) return GetType().Name;
        return LeftParser.ShortDescription(depth - 1) + " < " + RightParser.ShortDescription(depth - 1);
    }
}