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
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var result = scan.NullMatch(this, previousMatch?.Right ?? 0);

        while (!scan.EndOfInput(result.Right))
        {
            var item = LeftParser.Parse(scan, result);

            if (!item.Success)
            {
                return result.Through(this);
            }

            var terminator = RightParser.Parse(scan, item);

            if (!terminator.Success)
            {
                return result.Through(this);
            }

            result = ParserMatch.Join(new NullParser(nameof(TerminatedList)), result, item);
            result = ParserMatch.Join(new NullParser(nameof(TerminatedList)), result, terminator);
        }

        return result.Through(this);
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