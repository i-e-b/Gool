using Phantom.Parsers.Composite.Abstracts;

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
    public override ParserMatch TryMatch(IScanner scan)
    {
        // save scanner state
        int offset = scan.Offset;

        // apply the first parser
        var m = Parser.Parse(scan);

        if (m.Success) {
            return scan.CreateMatch(this, m.Offset, m.Length);
        }

        // rewind
        scan.Seek(offset);
        return scan.NoMatch;
    }
    
    
    /// <inheritdoc />
    public override string ToString()
    {
        if (TagValue is null) return "{" + Parser + "}";
        return "{" + Parser + "}" + " Tag='" + TagValue + "'";
    }
}