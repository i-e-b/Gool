using System;
using Phantom.Parsers.Composite.Abstracts;
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
		var result = previousMatch ?? scan.EmptyMatch(this, 0); // empty match with this parser

		int count = 0;

		while (count < UpperBound && !scan.EndOfInput(result.Right))
		{
			var after = Parser.Parse(scan, result);
			if (!after.Success) break; // no more matches
			if (after.SameAs(result)) break; // repetition must progress

			count++;
			result = ParserMatch.Join(this, result, after);
		}

		if (count < LowerBound || count > UpperBound)
		{
			return scan.NoMatch;
		}
		
		return result;
	}

	/// <inheritdoc />
	public override string ToString()
	{
		string desc;
		if (LowerBound == 0 && UpperBound > 1) desc = "("+Parser + ")*";
		else if (LowerBound == 0 && UpperBound == 1) desc = "("+Parser + ")?";
		else if (LowerBound == 1 && UpperBound > 1) desc = "("+Parser + ")+";
		else desc = "[" + LowerBound + ".." + UpperBound + ":" + Parser + "]";

		if (TagValue is null) return desc;
		return desc + " Tag='" + TagValue + "'";
	}
}