using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite;

/// <summary>
/// Creates a delimited list parser from two sub-parsers.
/// The list expects one or more of left parser, each
/// terminated by a single occurrence of the right parser.
/// The final element may NOT exclude it's terminator.
/// </summary>
public class TerminatedList : Binary
{
	/// <summary>
	/// Creates a delimited list parser from two sub-parsers.
	/// The list expects one or more of left parser, each
	/// terminated by a single occurrence of the right parser.
	/// The final element may NOT exclude it's terminator.
	/// </summary>
	public TerminatedList(IParser item, IParser terminator)
		: base(item, terminator)
	{
	}

	/// <inheritdoc />
	public override ParserMatch TryMatch(IScanner scan)
	{
		int offset = scan.Offset;

		var m = new ParserMatch(this, scan, offset, -1);

		while (!scan.EndOfInput)
		{
			offset = scan.Offset;

			var a = LeftParser.Parse(scan);

			if (!a.Success)
			{
				scan.Seek(offset);
				return m;
			}

			var b = RightParser.Parse(scan);

			if (!b.Success)
			{
				scan.Seek(offset);
				return m;
			}
				
			m.AddSubMatch(a);
			m.AddSubMatch(b);
		}

		return m;
	}

	/// <inheritdoc />
	public override string ToString()
	{
		var desc = LeftParser + " < " + RightParser;
			
		if (TagValue is null) return desc;
		return desc + " Tag='" + TagValue + "'";
	}
}