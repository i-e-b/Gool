using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Composite {
	/// <summary>
	/// Creates a Union (or 'alternative') parser from two sub-parsers.
	/// </summary>
	class Union : Binary, ICompositeParser {
		public Union(Parser left, Parser right)
			: base(left, right) { }

		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			// save scanner state
			int offset = scan.Offset;
			ParserMatch m = scan.NoMatch;
			ParserMatch m2 = scan.NoMatch;

			// apply the first parser
			/*if (bLeftParser != this)*/ // prevent crazy recursion
				m = bLeftParser.Parse(scan);

			// rewind
			scan.Seek(offset);

			// apply the second parser
			/*if (bRightParser != this)*/
				m2 = bRightParser.Parse(scan);

			// pick the longest result
			if (m.Success || m2.Success) {
				if (m2.Length >= m.Length) {
					scan.Seek(m2.Offset + m2.Length);
					return m2;
				} else {
					scan.Seek(m.Offset + m.Length);
					return m;
				}
			}

			// rewind to point of failure
			scan.Seek(offset);
			return scan.NoMatch;
		}

		public override string ToString() {
			return LeftParser.ToString() + " | " + RightParser.ToString();
		}
	}
}
