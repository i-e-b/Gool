using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite;

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
	public override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		// apply the first parser
		var left = LeftParser.Parse(scan, previousMatch);

		// if left successful, do right
		if (left.Success)
		{
			var right = RightParser.Parse(scan, left);

			return right.Success ? ParserMatch.Join(this, left, right) : scan.NoMatch;
		}

		return scan.NoMatch;
	}

	/// <inheritdoc />
	public override string ToString()
	{
		var desc = LeftParser + " > " + RightParser;
			
		if (TagValue is null) return desc;
		return desc + " Tag='" + TagValue + "'";
	}
}