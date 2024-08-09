using Phantom.Parsers.Composite.Abstracts;
using Phantom.Results;

namespace Phantom.Parsers.Composite;

/// <summary>
/// Wraps a single other parser.
/// This is used to allow a parser pattern to be
/// reused with different tags in a larger composite.
/// </summary>
public class Wrapper : Unary
{
    /// <summary>
    /// Create a wrapper around another parser
    /// </summary>
    public Wrapper(IParser parser) : base(parser) { }

    /// <inheritdoc />
    public override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        // apply the first parser
        var innerMatch = Parser.Parse(scan, previousMatch);

        return innerMatch.Success
            ? scan.CreateMatch(this, innerMatch.Offset, innerMatch.Length)
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