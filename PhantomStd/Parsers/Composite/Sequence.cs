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
	public override ParserMatch TryMatch(IScanner scan)
	{
		// save scanner state
		int offset = scan.Offset;
		var m = scan.NoMatch;

		// apply the first parser
		var left = LeftParser.Parse(scan);

		// if left successful, do right
		if (left.Success)
		{
			var right = RightParser.Parse(scan);

			m = right.Success ? ParserMatch.Concat(this, left, right) : scan.NoMatch;
		}

		// restoring parser failed, rewind scanner
		if (!m.Success)
			scan.Seek(offset);

		return m;
	}

	/// <inheritdoc />
	public override string ToString()
	{
		var desc = LeftParser + " > " + RightParser;
			
		if (TagValue is null) return desc;
		return desc + " Tag='" + TagValue + "'";
	}
}