using System;
using Gool.Parsers.Composite.Abstracts;
using Gool.Results;

namespace Gool.Parsers.Composite;

/// <summary>
/// Wraps a single other parser.
/// <p/>
/// This is used to allow a parser pattern to be
/// reused with different tags in a larger composite;
/// And allows an optional modification function.
/// </summary>
public class Wrapper : Unary
{
    private readonly Func<string, string>? _mutator;

    /// <summary>
    /// Create a wrapper around another parser
    /// </summary>
    public Wrapper(IParser parser) : base(parser) { }
    
    /// <summary>
    /// Create a wrapper around another parser, with a function to modify the output
    /// </summary>
    public Wrapper(IParser parser, Func<string, string> mutator) : base(parser)
    {
        _mutator = mutator;
    }

    /// <inheritdoc />
    public override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        // apply the first parser
        var innerMatch = Parser.Parse(scan, previousMatch);

        return innerMatch.Success
            ? scan.CreateMatch(this, innerMatch.Offset, innerMatch.Length, _mutator)
            : scan.NoMatch(this, innerMatch);
    }
    
    
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