using System;
using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers;

/// <summary>
/// Base class for all parsers.
/// </summary>
public abstract class Parser : IParser
{
	/// <summary>
	/// Optional tag value for this parser
	/// </summary>
	protected string? TagValue { get; private set; }

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
			//return ParserMatch.Join(this, start, newMatch);
			return newMatch;
		}
		else
		{
			scan.AddFailure(this, previousMatch?.Offset ?? 0);
			return scan.NoMatch;
		}
	}

	/// <inheritdoc />
	public void Tag(string tag)
	{
		TagValue = tag;
	}

	/// <inheritdoc />
	public string? GetTag()
	{
		return TagValue;
	}
}