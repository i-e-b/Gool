using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Composite {
	/// <summary>
	/// A parser which matches a left parser then a right parser.
	/// </summary>
	class Sequence : Binary, ICompositeParser {
		public Sequence(Parser left, Parser right)
			: base(left, right) { }

		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			// save scanner state
			int offset = scan.Offset;
			ParserMatch m = scan.NoMatch;

			// apply the first parser
			ParserMatch left = bLeftParser.Parse(scan);

			// if left successful, do right
			if (left.Success) {
				ParserMatch right = bRightParser.Parse(scan);

				if (right.Success) {
					m = ParserMatch.Concat(this, left, right);
				} else {
					m = scan.NoMatch;
				}
			}

			// restoring parser failed, rewind scanner
			if (!m.Success)
				scan.Seek(offset);

			return m;
		}

		public override string ToString() {
			return "(" + LeftParser.ToString() + " " + RightParser.ToString() + ")";
		}
	}
}
