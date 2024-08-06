using System.Linq;
using Phantom.Parsers.Composite.Abstracts;
using Phantom.Results;

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
        // TODO: re-do this
        var item = LeftParser.Parse(scan, previousMatch);
        if (!item.Success) return scan.NoMatch;
        
        var result = item;

        while (!scan.EndOfInput(result.Right))
        {
            var delimiter = RightParser.Parse(scan, result); // delimiter

            if (!delimiter.Success) return result;

            item = LeftParser.Parse(scan, result); // item

            if (!item.Success) return result;

            result = ParserMatch.Join(this, delimiter, item); // delimiter from prev one
        }

        return result;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        if (TagValue is null) return LeftParser + " % " + RightParser;
        return LeftParser + " % " + RightParser + " Tag='" + TagValue + "'";
    }
}