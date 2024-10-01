using System;
using Gool.Parsers.Composite.Abstracts;
using Gool.Parsers.Terminals;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Composite;

/// <summary>
/// Create a generalised repetition parser from a single sub-parser
/// </summary>
public class Repetition : Unary
{
	/// <summary>
	/// Create a generalised repetition parser from a single sub-parser
	/// </summary>
	public Repetition(IParser parser, uint lowerBound, uint upperBound)
		: base(parser)
	{
		SetBounds(lowerBound, upperBound);
	}

	/// <summary>
	/// The lower bound of allowed repeat count
	/// </summary>
	private uint _lowerBound;

	/// <summary>
	/// The upper bound of allowed repeat count
	/// </summary>
	private uint _upperBound;

	/// <summary>
	/// Set allowable range of repeats
	/// </summary>
	private void SetBounds(uint lowerBound, uint upperBound)
	{
		if (upperBound < lowerBound)
			throw new ArgumentException("Lower bound must be less than upper bound");
		_lowerBound = lowerBound;
		_upperBound = upperBound;
	}

	/// <inheritdoc />
	internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		var result = scan.EmptyMatch(this, previousMatch?.Right ?? 0, previousMatch); // empty match with this parser

		int count = 0;

		while (count < _upperBound && !scan.EndOfInput(result.Right))
		{
			var after = Parser.Parse(scan, result);
			if (!after.Success) break; // no more matches
			if (after.Right <= result.Right) break; // repetition must progress

			count++;
			result = ParserMatch.Join(previousMatch, new NullParser(nameof(Repetition)), result, after);
		}

		if (count < _lowerBound || count > _upperBound)
		{
			return scan.NoMatch(this, result);
		}
		
		return result.Through(this, previousMatch);
	}

	/// <inheritdoc />
	public override string ToString()
	{
		string desc;
		if (_lowerBound == 0 && _upperBound > 1) desc = "("+Parser + ")*";
		else if (_lowerBound == 0 && _upperBound == 1) desc = "("+Parser + ")?";
		else if (_lowerBound == 1 && _upperBound > 1) desc = "("+Parser + ")+";
		else desc = "[" + _lowerBound + ".." + _upperBound + ":" + Parser + "]";

		if (Tag is null) return desc;
		return desc + " Tag='" + Tag + "'";
	}

	/// <inheritdoc />
	public override bool IsOptional() => _lowerBound == 0 || Parser.IsOptional();

	/// <inheritdoc />
	public override string ShortDescription(int depth)
	{
		if (depth < 1) return GetType().Name;

		const int large = int.MaxValue >> 1;

		if (_lowerBound == 0 && _upperBound >= large) return "(" + Parser.ShortDescription(depth - 1) + ")*";
		if (_lowerBound == 0 && _upperBound == 1)     return "(" + Parser.ShortDescription(depth - 1) + ")?";
		if (_lowerBound == 1 && _upperBound >= large) return "(" + Parser.ShortDescription(depth - 1) + ")+";
		return "[" + _lowerBound + ".." + _upperBound + ":" + Parser.ShortDescription(depth - 1) + "]";
	}
}