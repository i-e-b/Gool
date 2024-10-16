using System.Collections.Generic;
using Gool.Parsers.Terminals;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers;

/// <summary>
/// Base class for all parsers.
/// </summary>
public abstract class Parser : IParser
{
    private readonly IParser _skippedElementsSrc = new NullParser("Skipped elements");

    /// <inheritdoc />
    public string? Tag { get; set; }

    /// <inheritdoc />
    public ScopeType Scope { get; set; }

    /// <inheritdoc />
    public abstract bool IsOptional();

    /// <inheritdoc />
    public bool HasMetaData()
    {
        return !(Tag is null && Scope == ScopeType.None);
    }

    /// <inheritdoc />
    public abstract string ShortDescription(int depth);

    /// <inheritdoc />
    public abstract IEnumerable<IParser> ChildParsers();

    /// <summary>
    /// Public scanner method. Test scanner input for this parser's patterns.
    /// </summary>
    /// <remarks>Most parsers won't need to override this method</remarks>
    /// <param name="scan">Scanner to parse from</param>
    /// <param name="previousMatch">Match to continue from. <c>null</c> if starting</param>
    /// <param name="allowAutoAdvance">if true, auto-advance will be tried</param>
    /// <returns>Match (success of failure) of the parser against the scanner</returns>
    public ParserMatch Parse(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance = true)
    {
        var start = allowAutoAdvance ? scan.DoAutoAdvance(previousMatch) : previousMatch;

        if (Tag is not null) scan.LastTag = Tag;

        var newMatch = TryMatch(scan, start, allowAutoAdvance);
        if (newMatch.Success)
        {
            scan.AddSuccess(newMatch);

            if (scan.IncludeSkippedElements) return ParserMatch.Join(start, _skippedElementsSrc, start, newMatch);
            return newMatch;
        }

        scan.AddFailure(newMatch);
        return newMatch.Through(this, previousMatch);
    }

    private bool AnyOptionalChildren(IParser node)
    {
        if (node.IsOptional()) return true;
        foreach (var child in node.ChildParsers())
        {
            if (AnyOptionalChildren(child)) return true;
        }

        return false;
    }

    /// <summary>
    /// Try to match scanner data against the contained parser
    /// </summary>
    internal abstract ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance);
}