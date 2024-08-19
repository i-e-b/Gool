using System;
using Phantom.Parsers.Composite.Abstracts;
using Phantom.Parsers.Terminals;
using Phantom.Results;

namespace Phantom.Parsers.Composite;

/// <summary>
/// Create a generalised repetition parser from a single subparser
/// </summary>
public class Repetition : Unary
{
	/// <summary>
	/// Create a generalised repetition parser from a single subparser
	/// </summary>
	public Repetition(IParser parser, uint lowerBound, uint upperBound)
		: base(parser)
	{
		SetBounds(lowerBound, upperBound);
	}

	/// <summary>
	/// The lower bound of allowed repeat count
	/// </summary>
	public uint LowerBound { get; private set; }

	/// <summary>
	/// The upper bound of allowed repeat count
	/// </summary>
	public uint UpperBound { get; private set; }

	/// <summary>
	/// Set allowable range of repeats
	/// </summary>
	public void SetBounds(uint lowerBound, uint upperBound)
	{
		if (upperBound < lowerBound)
			throw new ArgumentException("Lower bound must be less than upper bound");
		LowerBound = lowerBound;
		UpperBound = upperBound;
	}

	/// <inheritdoc />
	public override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		var result = scan.EmptyMatch(this, previousMatch?.Right ?? 0); // empty match with this parser

		int count = 0;

		while (count < UpperBound && !scan.EndOfInput(result.Right))
		{
			var after = Parser.Parse(scan, result);
			if (!after.Success) break; // no more matches
			if (after.SameAs(result)) break; // repetition must progress

			count++;
			result = ParserMatch.Join(new NullParser(nameof(Repetition)), result, after);
		}

		if (count < LowerBound || count > UpperBound)
		{
			return scan.NoMatch(this, result);
		}
		
		return result.Through(this);
	}

	/// <inheritdoc />
	public override string ToString()
	{
		string desc;
		if (LowerBound == 0 && UpperBound > 1) desc = "("+Parser + ")*";
		else if (LowerBound == 0 && UpperBound == 1) desc = "("+Parser + ")?";
		else if (LowerBound == 1 && UpperBound > 1) desc = "("+Parser + ")+";
		else desc = "[" + LowerBound + ".." + UpperBound + ":" + Parser + "]";

		if (Tag is null) return desc;
		return desc + " Tag='" + Tag + "'";
	}
	
	/// <inheritdoc />
	public override string ShortDescription(int depth)
	{
		if (depth < 1) return GetType().Name;

		const int large = int.MaxValue >> 1;

		if (LowerBound == 0 && UpperBound >= large) return "(" + Parser.ShortDescription(depth - 1) + ")*";
		if (LowerBound == 0 && UpperBound == 1)     return "(" + Parser.ShortDescription(depth - 1) + ")?";
		if (LowerBound == 1 && UpperBound >= large) return "(" + Parser.ShortDescription(depth - 1) + ")+";
		return "[" + LowerBound + ".." + UpperBound + ":" + Parser.ShortDescription(depth - 1) + "]";
	}
}