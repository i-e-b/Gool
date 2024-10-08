using Gool.Parsers.Composite.Abstracts;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Composite;

/// <summary>
/// A parser which matches a left parser then a right parser.
/// </summary>
public class Sequence : Binary
{
	/// <summary>
	/// A parser which matches a left parser then a right parser.
	/// </summary>
	public Sequence(IParser left, IParser right)
		: base(left, right)
	{
	}

	/// <inheritdoc />
	internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
	{
		// apply the first parser
		var left = LeftParser.Parse(scan, previousMatch, allowAutoAdvance);

		// if left successful, do right
		if (left.Success)
		{
			var right = RightParser.Parse(scan, left, allowAutoAdvance);

			return right.Success ? ParserMatch.Join(previousMatch, this, left, right) : scan.NoMatch(this, previousMatch);
		}

		return scan.NoMatch(this, previousMatch);
	}

	/// <inheritdoc />
	public override bool IsOptional() => LeftParser.IsOptional() && RightParser.IsOptional();

	/// <inheritdoc />
	public override string ToString()
	{
		var desc = LeftParser + " > " + RightParser;
			
		if (Tag is null) return desc;
		return desc + " Tag='" + Tag + "'";
	}

	/// <inheritdoc />
	public override string ShortDescription(int depth)
	{
		if (depth < 1) return GetType().Name;
		return LeftParser.ShortDescription(depth - 1) + " " + RightParser.ShortDescription(depth - 1);
	}
}