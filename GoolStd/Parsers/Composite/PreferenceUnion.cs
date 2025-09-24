using System.Collections.Generic;
using System.Linq;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Composite;

/// <summary>
/// Creates a Preference-Union (or 'preference-alternative') parser from two sub-parsers.
/// This returns the first successful match.
/// </summary>
public class PreferenceUnion : Parser
{
    private readonly IParser[] _parsers;

    /// <summary>
    /// Creates a Preference-Union (or 'preference-alternative') parser from two sub-parsers.
    /// </summary>
    public PreferenceUnion(IParser left, IParser right)
    {
        var parserSet = new List<IParser>();
        if (left is PreferenceUnion leftUnion)
        {
            parserSet.AddRange(leftUnion._parsers);
        }
        else
        {
            parserSet.Add(left);
        }

        parserSet.Add(right);

        _parsers = parserSet.ToArray();
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        foreach (var parser in _parsers)
        {
            var result = parser.Parse(scan, previousMatch, allowAutoAdvance);
            if (result.Success) return result.Through(this, previousMatch);
        }

        return scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() => _parsers;

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "{" + string.Join(" | ", _parsers.Select(p => p.ToString())) + "}";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        if (depth < 1) return GetType().Name;
        return "{" + string.Join(" | ", _parsers.Select(p => p.ShortDescription(depth - 1))) + "}";
    }
}