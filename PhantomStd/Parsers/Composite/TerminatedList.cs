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
        var m = previousMatch ?? scan.NullMatch(this, 0);

        while (!scan.EndOfInput(m.Right))
        {
            var a = LeftParser.Parse(scan, m);

            if (!a.Success)
            {
                return m;
            }

            var b = RightParser.Parse(scan, a);

            if (!b.Success)
            {
                return m;
            }

            m.AddSubMatch(a);
            m.AddSubMatch(b);
        }

        return m;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = LeftParser + " < " + RightParser;

        if (TagValue is null) return desc;
        return desc + " Tag='" + TagValue + "'";
    }
}