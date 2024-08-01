using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite;

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
    public override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var result = previousMatch ?? scan.NullMatch(this, 0);

        while (!scan.EndOfInput(result.Right))
        {
            var item = LeftParser.Parse(scan, result);

            if (!item.Success)
            {
                return result;
            }

            var terminator = RightParser.Parse(scan, item);

            if (!terminator.Success)
            {
                return result;
            }

            result = ParserMatch.Join(this, item, terminator);
        }

        return result;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = LeftParser + " < " + RightParser;

        if (TagValue is null) return desc;
        return desc + " Tag='" + TagValue + "'";
    }
}