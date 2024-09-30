using System;
using System.Collections.Generic;
using Gool.Parsers.Terminals;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers;

/// <summary>
/// A parser implementation which contains another parser
/// and passes all parsing instructions to it.
/// This is for use with mutually dependent parser trees.
/// </summary>
public class Recursion : Parser
{
	/// <summary>
	/// Set to <c>true</c> to write diagnostic output
	/// </summary>
	public static bool TraceRecursion = false;

	/// <summary> Global fall-back for an invalid recursion definition </summary>
	private static readonly IParser  _fallbackParser = new NullParser("Unassigned recursion parser");

	/// <summary> The assigned parser for this instance </summary>
	private IParser _parser = _fallbackParser;

	/// <summary>
	/// Contained recursive parser
	/// </summary>
	public IParser Source
	{
		get => _parser;
		set {
			if (value is null) throw new Exception("Invalid recursion parser (must reference a valid parser)");
			if (value == this) throw new Exception("Invalid recursion parser (must not reference self)");
			if (value is Recursion) throw new Exception("Invalid recursion parser (must not reference another recursion parser)");
			_parser = value;
		}
	}

	/// <summary>
	/// Try to match scanner data against the contained parser
	/// </summary>
	internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		// Recursion safety checks:
		var key  = ((long)(previousMatch?.SourceParser?.GetHashCode() ?? 0) << 32) + (previousMatch?.Right ?? 0);
		var hits = GetContext(scan);

		if (!hits.Add(key))
		{
			if (TraceRecursion) Console.WriteLine($"Recursion re-applied to same location at: {previousMatch?.Description() ?? "zero"}; Parser={Source.ShortDescription(5)};");
			return scan.NoMatch(this, previousMatch); // recursion must not re-apply to same location
		}

		var result = _parser.Parse(scan, previousMatch);

		if (result.SameAs(previousMatch))
		{
			if (TraceRecursion) Console.WriteLine($"Recursion did not progress from: {result.Description()}; Parser={Source.ShortDescription(5)};");
			return scan.NoMatch(this, previousMatch); // recursion must progress
		}

		return result;
	}

	/// <inheritdoc />
	public override IEnumerable<IParser> ChildParsers() { yield break; } // we treat this as empty to prevent accidental stack overflow if traversing

	/// <inheritdoc />
	public override bool IsOptional() => _parser.IsOptional();

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
		return $"Recursion({_parser.GetType().Name})";
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
		return $"Recursion({_parser.ShortDescription(depth - 1)})";
	}
}