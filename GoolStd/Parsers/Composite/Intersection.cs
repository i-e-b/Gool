using Gool.Parsers.Composite.Abstracts;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Composite;

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
	internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
	{
		var a = LeftParser.Parse(scan, previousMatch, allowAutoAdvance);

		if (a.Success)
		{
			var b = RightParser.Parse(scan, a, allowAutoAdvance);
			if (b.Success) return ParserMatch.Join(previousMatch, this, a, b);
		}
		else
		{
			a = RightParser.Parse(scan, previousMatch, allowAutoAdvance);
			if (!a.Success) return scan.NoMatch(this, previousMatch);
			
			var right = LeftParser.Parse(scan, a, allowAutoAdvance);
			if (right.Success) return ParserMatch.Join(previousMatch, this, a, right);
		}

		return scan.NoMatch(this, previousMatch);
	}

	/// <inheritdoc />
	public override string ToString()
	{
		if (Tag is null) return LeftParser + " & " + RightParser;
		return LeftParser + " & " + RightParser + " Tag='" + Tag + "'";
	}
	
	/// <inheritdoc />
	public override string ShortDescription(int depth)
	{
		if (depth < 1) return GetType().Name;
		return LeftParser.ShortDescription(depth - 1) + " & " + RightParser.ShortDescription(depth - 1);
	}
}