using System;
using Phantom.Parsers.Interfaces;
using Phantom.Results;

namespace Phantom.Parsers;

/// <summary>
/// Base class for all parsers.
/// </summary>
public abstract class Parser : IParser
{
	/// <summary>
	/// Optional tag value for this parser
	/// </summary>
	public string? Tag { get; set; }

	/// <summary>
	/// Optional scope behaviour
	/// </summary>
	public int ScopeSign { get; set; }

	/// <inheritdoc />
	public bool HasMetaData()
	{
		return !(Tag is null && ScopeSign == 0);
	}

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
			scan.ClearFailures();
			return newMatch;
		}

		scan.AddFailure(this, previousMatch?.Offset ?? 0, previousMatch?.Right ??0);
		return scan.NoMatch;
	}
}