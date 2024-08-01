using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite;

/// <summary>
/// Create an Exclusive-OR parser from two sub-parsers.
/// Can match first or second, but not both
/// </summary>
public class Exclusive : Binary
{
	/// <summary>
	/// Create an Exclusive-OR parser from two sub-parsers.
	/// Can match first or second, but not both
	/// </summary>
	public Exclusive(IParser first, IParser second)
		: base(first, second)
	{
	}

	/// <inheritdoc />
	public override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		// apply the first parser
		var m1 = LeftParser.Parse(scan, previousMatch);

		// apply the second parser
		var m2 = RightParser.Parse(scan, previousMatch);

		if (m2.Success && m1.Success)
		{
			// FAIL! they are not exclusive
			return scan.NoMatch;
		}

		// now return whichever one succeeded
		if (m1.Success) return m1;
		if (m2.Success) return m2;

		// neither were matched!
		return scan.NoMatch;
	}

	/// <inheritdoc />
	public override string ToString()
	{
		if (TagValue is null) return LeftParser + " ^ " + RightParser;
		return LeftParser + " ^ " + RightParser + " Tag='" + TagValue + "'";
	}
}