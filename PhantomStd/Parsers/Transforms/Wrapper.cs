using System;
using Gool.Parsers.Composite.Abstracts;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Transforms;

/// <summary>
/// Wraps a single other parser.
/// <p/>
/// This is used to allow a parser pattern to be
/// reused with different tags in a larger composite;
/// And allows an optional on-match function.
/// </summary>
public class Wrapper : Unary
{
    private readonly Func<ParserMatch, ParserMatch>? _action;

    /// <summary>
    /// Create a wrapper around another parser
    /// </summary>
    public Wrapper(IParser parser) : base(parser)
    {
    }

    /// <summary>
    /// Create a wrapper around another parser
    /// </summary>
    public Wrapper(IParser parser, Func<ParserMatch, ParserMatch> action) : base(parser)
    {
        _action = action;
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        // apply the first parser
        var innerMatch = Parser.Parse(scan, previousMatch, allowAutoAdvance);

        if (innerMatch.Success && _action is not null) innerMatch = _action(innerMatch);

        return innerMatch.Success
            ? innerMatch.ReSource(this)
            : scan.NoMatch(this, innerMatch);
    }

    /// <inheritdoc />
    public override bool IsOptional() => Parser.IsOptional();

    /// <inheritdoc />
    public override string ToString()
    {
        if (Tag is null) return "{" + Parser + "}";
        return "{" + Parser + "}" + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return Parser.ShortDescription(depth);
    }
}