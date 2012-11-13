using Phantom.Scanners;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Creates a delimited list parser from two sub-parsers.
	/// The list expects one or more of left parser, each
	/// terminated by a single occourance of the right parser.
	/// The final element may include or exclude it's terminator.
	/// </summary>
	public class TerminatedList : Binary
	{
		public TerminatedList(IParser item, IParser terminator)
			: base(item, terminator)
		{
		}

		public override ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;

			var a = bLeftParser.Parse(scan);

			if (!a.Success)
			{
				scan.Seek(offset);
				return scan.NoMatch;
			}

			var m = new ParserMatch(this, scan, a.Offset, a.Length);
			m.AddSubmatch(a);

			while (!scan.EOF)
			{
				offset = scan.Offset;

				var b = bRightParser.Parse(scan);

				if (!b.Success)
				{
					scan.Seek(offset);
					return m;
				}

				m.AddSubmatch(b);

				a = bLeftParser.Parse(scan);

				if (!a.Success)
				{
					return m;
				}

				m.AddSubmatch(a);
			}

			return m;
		}

		public override string ToString()
		{
			return LeftParser + "<" + RightParser;
		}
	}
}