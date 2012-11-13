using Phantom.Scanners;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Creates a delimited list parser from two sub-parsers.
	/// The list expects at least one of left parser, optionally
	/// seperated by single occurances of right parser.
	/// </summary>
	class DelimitedList : Binary
	{
		public DelimitedList(IParser item, IParser delimiter)
			: base(item, delimiter)
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

				a = bLeftParser.Parse(scan);

				if (!a.Success)
				{
					scan.Seek(offset);
					return m;
				}

				m.AddSubmatch(b);
				m.AddSubmatch(a);
			}

			return m;
		}

		public override string ToString()
		{
			return LeftParser + "%" + RightParser;
		}
	}
}