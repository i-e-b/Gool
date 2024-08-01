using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite;

/// <summary>
/// Create an Intersection parser from two sub-parsers.
/// Either first-then-second should pass, or second-then-first.
/// </summary>
public class Intersection : Binary
{
	/// <summary>
	/// Create an Intersection parser from two sub-parsers.
	/// Either first-then-second should pass, or second-then-first.
	/// </summary>
	public Intersection(IParser first, IParser second)
		: base(first, second)
	{
	}

	/// <inheritdoc />
	public override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		var left = LeftParser.Parse(scan, previousMatch);

		if (left.Success)
		{
			var right = RightParser.Parse(scan, left);
			if (right.Success)
			{
				return ParserMatch.Concat(this, left, right);
			}
		}
		else
		{
			left = RightParser.Parse(scan, previousMatch);
			if (left.Success)
			{
				var right = LeftParser.Parse(scan, left);
				if (right.Success)
				{
					return ParserMatch.Concat(this, left, right);
				}
			}
		}

		return scan.NoMatch;
	}

	/// <inheritdoc />
	public override string ToString()
	{
		if (TagValue is null) return LeftParser + " & " + RightParser;
		return LeftParser + " & " + RightParser + " Tag='" + TagValue + "'";
	}
}