using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Creates a delimited list parser from two sub-parsers.
	/// The list expects at least one of left parser, optionally
	/// seperated by single occurrences of right parser.
	/// </summary>
	public class DelimitedList : Binary
	{
		/// <summary>
		/// Creates a delimited list parser from two sub-parsers.
		/// </summary>
		public DelimitedList(IParser item, IParser delimiter)
			: base(item, delimiter)
		{
		}

		/// <inheritdoc />
		public override ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;

			var a = LeftParser.Parse(scan);

			if (!a.Success)
			{
				scan.Seek(offset);
				return scan.NoMatch;
			}

			var m = new ParserMatch(this, scan, a.Offset, a.Length);
			m.AddSubmatch(a);

			while (!scan.EndOfInput)
			{
				offset = scan.Offset;

				var b = RightParser.Parse(scan);

				if (!b.Success)
				{
					scan.Seek(offset);
					return m;
				}

				a = LeftParser.Parse(scan);

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

		/// <inheritdoc />
		public override string ToString()
		{
			return LeftParser + " % " + RightParser;
		}
	}
}