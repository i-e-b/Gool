using System;
using System.Collections.Generic;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Transforms;

/// <summary>
/// Generate a contextualised parser from a previous result.
/// </summary>
public class ContextParser : Parser
{
    private readonly BNF                              _prefix;
    private readonly Func<ParserMatch, BNF>           _next;
    private readonly Func<ParserMatch, ParserMatch?> _select;

    /// <summary>
    /// Generate a contextualised parser from a previous result.
    /// </summary>
    /// <param name="prefix">
    /// Parser that reads context. This must match for the generated parser to be run.
    /// The result tree from this parser will be available to build the 'next' one.
    /// </param>
    /// <param name="select">
    /// Optional function to select parts of the match to use.
    /// If not provided, the entire match will be given.
    /// If the function is given, but returns null, the context will fail to match</param>
    /// <param name="next">
    /// Function to generate the next parser fragment
    /// </param>
    public ContextParser(BNF prefix, Func<ParserMatch, ParserMatch?>? select, Func<ParserMatch, BNF> next)
    {
        _prefix = prefix;
        _next = next;
        _select = select ?? (match => match);
    }

    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var preamble = _prefix.Parse(scan, previousMatch);
        if (!preamble.Success) return preamble;

        var fragment = _select(preamble);
        if (fragment is null) return scan.NoMatch(this, previousMatch);

        var appendix = _next(fragment);
        var result = appendix.Parse(scan, preamble);

        if (result.Success) return ParserMatch.Join(this, preamble, result);
        return result;
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; } // we treat this as empty

    /// <inheritdoc />
    public override bool IsOptional() => false; // it could be, but probably not worth calculating

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return $"Context({_prefix.ShortDescription(depth - 1)})";
    }

    /// <inheritdoc />
    public override string ToString()
    {
        return $"Context({_prefix})";
    }
}