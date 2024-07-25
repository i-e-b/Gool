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
	/// <returns>Match (success of failure) of the parser against the scanner</returns>
	public virtual ParserMatch Parse(IScanner scan)
	{
		scan.Normalise();

		var originalOffset = scan.Offset;
		
		var st = new System.Diagnostics.StackTrace();
		scan.StackStats(st.FrameCount);

		if (scan.RecursionCheck(this, scan.Offset) && this is not Recursion) return scan.NoMatch;

		if (this is not IMatchingParser matcher) throw new Exception($"Parser '{GetType().Name}' is not capable of matching");

		var m = matcher.TryMatch(scan);

		if (m.Success)
		{
			scan.ClearFailures();
		}
		else
		{
			scan.AddFailure(this, scan.Offset);
			scan.Seek(originalOffset);
		}

		return m;
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