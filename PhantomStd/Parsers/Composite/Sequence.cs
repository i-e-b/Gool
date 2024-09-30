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
	internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		// apply the first parser
		var left = LeftParser.Parse(scan, previousMatch);

		// if left successful, do right
		if (left.Success)
		{
			var right = RightParser.Parse(scan, left);

			return right.Success ? ParserMatch.Join(this, left, right) : scan.NoMatch(this, previousMatch);
		}

		return scan.NoMatch(this, previousMatch);
	}

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
		return LeftParser.ShortDescription(depth - 1) + " > " + RightParser.ShortDescription(depth - 1);
	}
}