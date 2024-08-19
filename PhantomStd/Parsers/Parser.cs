using System;
using Phantom.Parsers.Interfaces;
using Phantom.Parsers.Terminals;
using Phantom.Results;

namespace Phantom.Parsers;

/// <summary>
/// Base class for all parsers.
/// </summary>
public abstract class Parser : IParser
{
    /// <inheritdoc />
    public string? Tag { get; set; }

    /// <inheritdoc />
    public ScopeType Scope { get; set; }

    /// <inheritdoc />
    public bool HasMetaData()
    {
        return !(Tag is null && Scope == ScopeType.None);
    }

    /// <inheritdoc />
    public abstract string ShortDescription(int depth);

    /// <summary>
    /// Public scanner method. Test scanner input for this parser's patterns.
    /// </summary>
    /// <remarks>Most parsers won't need to override this method</remarks>
    /// <param name="scan">Scanner to parse from</param>
    /// <param name="previousMatch">Match to continue from. <c>null</c> if starting</param>
    /// <returns>Match (success of failure) of the parser against the scanner</returns>
    public virtual ParserMatch Parse(IScanner scan, ParserMatch? previousMatch)
    {
        if (this is not IMatchingParser matcher) throw new Exception($"Parser '{GetType().Name}' is not capable of creating matches");

        var start = scan.AutoAdvance(previousMatch);

        var newMatch = matcher.TryMatch(scan, start);
        if (newMatch.Success)
        {
            scan.AddPath(newMatch);
            scan.ClearFailures();

            if (scan.IncludeSkippedElements) return ParserMatch.Join(new NullParser("Skipped elements"), start, newMatch);
            return newMatch;
        }

        scan.AddFailure(this, previousMatch);
        return scan.NoMatch(this, previousMatch);
    }
}