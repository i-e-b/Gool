using Phantom.Scanners;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Creates a delimited list parser from two sub-parsers.
	/// The list expects one or more of left parser, each
	/// terminated by a single occourance of the right parser.
	/// The final element may include or exclude it's terminator.
	/// </summary>
	class TerminatedList : Binary, ICompositeParser
	{
		public TerminatedList(Parser item, Parser terminator)
			: base(item, terminator)
		{
		}

		public override ParserMatch ParseMain(IScanner scan)
		{
			int offset = scan.Offset;
			ParserMatch a = scan.NoMatch;
			ParserMatch b = scan.NoMatch;

			ParserMatch m = scan.NoMatch;
			a = bLeftParser.Parse(scan);

			if (!a.Success)
			{
				scan.Seek(offset);
				return scan.NoMatch;
			}

			m = new ParserMatch(this, scan, a.Offset, a.Length);
			m.AddSubmatch(a);

			while (!scan.EOF)
			{
				offset = scan.Offset;

				b = bRightParser.Parse(scan);

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