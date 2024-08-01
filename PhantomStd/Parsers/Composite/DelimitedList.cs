using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite;

/// <summary>
/// Creates a delimited list parser from two sub-parsers.
/// The list expects at least one of left parser, optionally
/// seperated by single occurrences of right parser.
/// </summary>
public class DelimitedList : Binary
{
    /// <summary>
    /// Creates a delimited list parser from two sub-parsers.
    /// </summary>
    public DelimitedList(IParser item, IParser delimiter)
        : base(item, delimiter)
    {
    }

    /// <inheritdoc />
    public override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var a = LeftParser.Parse(scan, previousMatch);
        if (!a.Success) return scan.NoMatch;

        var m = new ParserMatch(this, scan, a.Offset, a.Length);
        m.AddSubMatch(a);

        while (!scan.EndOfInput(m.Right))
        {
            var b = RightParser.Parse(scan, m);

            if (!b.Success)
            {
                return m;
            }

            a = LeftParser.Parse(scan, m);

            if (!a.Success)
            {
                return m;
            }

            m.AddSubMatch(b);
            m.AddSubMatch(a);
        }

        return m;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        if (TagValue is null) return LeftParser + " % " + RightParser;
        return LeftParser + " % " + RightParser + " Tag='" + TagValue + "'";
    }
}