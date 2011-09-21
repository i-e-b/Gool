using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Composite {

	/// <summary>
	/// Create an Difference parser from two sub-parsers.
	/// </summary>
	class Difference : Binary, ICompositeParser {
		public Difference(Parser left, Parser right)
			: base(left, right) { }

		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			int offset = scan.Offset;

			ParserMatch m = scan.NoMatch;

			m = bLeftParser.Parse(scan);

			int goodOffset = scan.Offset;

			if (!m.Success) {
				scan.Seek(offset);
				return scan.NoMatch;
			}

			// doing difference
			scan.Seek(offset);
			ParserMatch m2 = scan.NoMatch;
			m2 = bRightParser.Parse(scan);
			if (m2.Success) {
				// fail: must match left but NOT right
				scan.Seek(offset);
				return scan.NoMatch;
			}

			// Good match
			scan.Seek(goodOffset);
			return m;
		}
	}
}
