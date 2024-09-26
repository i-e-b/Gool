using System.Collections.Generic;
using System.Linq;
using Gool.Results;

namespace Gool.Parsers.Composite;

/// <summary>
/// Match an ordered sequence of sub-parsers as a single match.
/// The entire set of sub-parsers must match for a successful match.
/// </summary>
public class CompositeSequence : Parser
{
    private readonly List<IParser> _parsers;

    /// <summary>
    /// Match an ordered sequence of sub-parsers as a single match
    /// </summary>
    public CompositeSequence(IEnumerable<IParser> parsers)
    {
        _parsers = new List<IParser>(parsers);
    }

    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var cursor = previousMatch ?? scan.NoMatch(this, previousMatch);

        foreach (var parser in _parsers)
        {
            var next = parser.Parse(scan, cursor);
            if (!next.Success) return next;

            cursor = cursor.Join(parser, next);
        }

        return cursor;
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        if (depth < 1) return "Composite";
        return $"Composite({string.Join(" > ", _parsers.Select(p => p.ShortDescription(depth - 1)))})";
    }
}