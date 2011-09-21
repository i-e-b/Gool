using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Composite {
	/// <summary>
	/// Creates a delimited list parser from two sub-parsers.
	/// The list expects at least one of left parser, optionally
	/// seperated by single occurances of right parser.
	/// </summary>
	class DelimitedList : Binary, ICompositeParser {
		public DelimitedList(Parser item, Parser delimiter)
			: base(item, delimiter) { }

		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			int offset = scan.Offset;
			ParserMatch a = scan.NoMatch;
			ParserMatch b = scan.NoMatch;

			ParserMatch m = scan.NoMatch;
			a = bLeftParser.Parse(scan);
			
			if (!a.Success) {
				scan.Seek(offset);
				return scan.NoMatch;
			}

			m = new ParserMatch(this, scan, a.Offset, a.Length);
			m.AddSubmatch(a);

			while (!scan.EOF) {
				offset = scan.Offset;

				b = bRightParser.Parse(scan);

				if (!b.Success) {
					scan.Seek(offset);
					return m;
				}

				a = bLeftParser.Parse(scan);

				if (!a.Success) {
					scan.Seek(offset);
					return m;
				}

				m.AddSubmatch(b);
				m.AddSubmatch(a);
			}

			return m;
		}

		public override string ToString() {
			return LeftParser.ToString() + "%" + RightParser.ToString();
		}

	}
}
