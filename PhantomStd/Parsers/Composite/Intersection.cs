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
		var a = LeftParser.Parse(scan, previousMatch);

		if (a.Success)
		{
			var b = RightParser.Parse(scan, a);
			if (b.Success) return ParserMatch.Join(this, a, b);
		}
		else
		{
			a = RightParser.Parse(scan, previousMatch);
			if (!a.Success) return scan.NoMatch;
			
			var right = LeftParser.Parse(scan, a);
			if (right.Success) return ParserMatch.Join(this, a, right);
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