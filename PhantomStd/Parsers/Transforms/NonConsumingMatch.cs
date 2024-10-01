using Gool.Parsers.Composite.Abstracts;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Transforms;

/// <summary>
/// Non-consuming match.
/// This checks that the child parser is a match,
/// but doesn't consume its output.
/// <p/>
/// This is mostly for validating context
/// </summary>
/// <example>
/// <code>
/// BNF
///     not_a_func = name > ~NoneOf('{', '(', '['), // the '~' means we won't consume the list separator ','
///     list_item  = not_a_func % ',';
/// </code>
/// </example>
public class NonConsumingMatch : Unary
{
    /// <summary>
    /// Create a wrapper around another parser
    /// </summary>
    public NonConsumingMatch(IParser parser) : base(parser) { }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var left = previousMatch ?? scan.EmptyMatch(this, 0, previousMatch);

        // test the first parser
        var check = Parser.Parse(scan, previousMatch);

        return check.Success
            ? scan.CreateMatch(this, left.Offset, 0, previousMatch)
            : scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override bool IsOptional() => false; // always zero-length

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "~" + Parser;
        return Tag is null ? desc : desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return Parser.ShortDescription(depth);
    }
}