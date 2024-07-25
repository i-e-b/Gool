using System;
using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers;

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
	public ParserMatch TryMatch(IScanner scan)
	{
		if (Source == null) throw new Exception("Empty holding parser");
		if (Source is not IMatchingParser parser) throw new Exception("Holding parser was non terminating");
		if (parser == this) throw new Exception("Unbounded recursion in parser");

		return parser.TryMatch(scan);
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
}