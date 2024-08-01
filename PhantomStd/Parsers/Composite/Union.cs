using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite;

/// <summary>
/// Creates a Union (or 'alternative') parser from two sub-parsers.
/// </summary>
public class Union : Binary
{
	/// <summary>
	/// Creates a Union (or 'alternative') parser from two sub-parsers.
	/// </summary>
	public Union(IParser left, IParser right)
		: base(left, right)
	{
	}

	/// <inheritdoc />
	public override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		// apply the first parser
		var m = LeftParser.Parse(scan, previousMatch);

		// apply the second parser
		var m2 = RightParser.Parse(scan, previousMatch);

		// pick the longest result
		if (m.Success || m2.Success)
		{
			return m2.Length >= m.Length ? m2 : m;
		}

		return scan.NoMatch;
	}

	/// <inheritdoc />
	public override string ToString()
	{
		var desc = LeftParser + "|" + RightParser;
			
		if (TagValue is null) return desc;
		return desc + " Tag='" + TagValue + "'";
	}
}