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
        var result = scan.NullMatch(this, previousMatch?.Right ?? 0); // failure until first match
        
        while (!scan.EndOfInput(result.Right))
        {
            var item = LeftParser.Parse(scan, result);

            if (!item.Success)
            {
                return result;
            }

            result = ParserMatch.Join(this, result, item);
            
            var separator = RightParser.Parse(scan, item);

            if (!separator.Success)
            {
                return result;
            }

            result = ParserMatch.Join(this, result, separator);
        }

        return result;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        if (Tag is null) return LeftParser + " % " + RightParser;
        return LeftParser + " % " + RightParser + " Tag='" + Tag + "'";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        if (depth < 1) return GetType().Name;
        return LeftParser.ShortDescription(depth - 1) + " % " + RightParser.ShortDescription(depth - 1);
    }
}