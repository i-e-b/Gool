using System;
using System.Collections.Generic;
using Gool.Parsers.Interfaces;
using Gool.Results;

namespace Gool.Parsers;

/// <summary>
/// A parser implementation which contains another parser
/// and passes all parsing instructions to it.
/// This is for use with mutually dependent parser trees.
/// </summary>
public class Recursion : Parser, IMatchingParser
{
	/// <summary>
	/// Contained recursive parser
	/// </summary>
	public IParser? Source { get; set; }

	/// <summary>
	/// Try to match scanner data against the contained parser
	/// </summary>
	public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		if (Source == null) throw new Exception("Empty holding parser");
		if (Source is not IMatchingParser parser) throw new Exception("Holding parser was non terminating");
		if (parser == this) throw new Exception("Unbounded recursion in parser");

		// Recursion safety checks:
		var key = ((long)(previousMatch?.SourceParser?.GetHashCode()??0) << 32) + (previousMatch?.Right ?? 0);
		var hits = GetContext(scan);

		if (!hits.Add(key)) return scan.NoMatch(this, previousMatch); // recursion must not re-apply to same location

		var result = parser.TryMatch(scan, previousMatch);
		if (result.SameAs(previousMatch)) return scan.NoMatch(this, previousMatch); // recursion must progress

		return result;
	}

	private HashSet<long> GetContext(IScanner scan)
	{
		if (scan.GetContext(this) is HashSet<long> hits) return hits;
		
		hits = new HashSet<long>();
		scan.SetContext(this, hits);

		return hits;
	}

	/// <summary>
	/// Returns the type name of the base parser
	/// </summary>
	public override string ToString()
	{
		return $"Recursion({Source?.GetType().Name ?? "<none>"})";
	}

	/// <summary>
	/// Create a simple Recursion parser.
	/// Input to the function is a holding parser, output is the parser to hold.
	/// (i.e. they end up being the same parser)
	/// </summary>
	public static IParser Over(Func<IParser, IParser> func)
	{
		var hold = new Recursion();
		hold.Source = func(hold);
		return hold;
	}

	/// <inheritdoc />
	public override string ShortDescription(int depth)
	{
		if (depth < 1) return GetType().Name;
		return $"Recursion({Source?.ShortDescription(depth - 1) ?? "<none>"})";
	}
}