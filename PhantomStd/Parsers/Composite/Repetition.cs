using System;
using Phantom.Parsers.Composite.Abstracts;

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
	public override ParserMatch TryMatch(IScanner scan)
	{
		//if (Parser == this) return scan.NoMatch; // ??

		// save scanner state
		int originalOffset = scan.Offset;

		var m = new ParserMatch(this, scan, originalOffset, 0); // empty match with this parser

		int count = 0;

		while (count < UpperBound && !scan.EndOfInput)
		{
			//var preOffset = scan.Offset;
			var maybeMatch = Parser.Parse(scan);
			if (!maybeMatch.Success)
			{
				//scan.Seek(preOffset);
				break;
			}

			count++;
			m.AddSubMatch(maybeMatch);
		}

		if (count < LowerBound || count > UpperBound)
		{
			//scan.Seek(originalOffset);
			return scan.NoMatch;
		}
		
		return m;
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