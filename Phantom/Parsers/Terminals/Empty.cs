using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Terminals {
	/// <summary>
	/// Parser that represents no input.
	/// Always returns an empty success match
	/// </summary>
	public class Empty :Parser {
		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			int offset = scan.Offset;
			ParserMatch m = scan.CreateMatch(this, offset, 0);
			return m;
		}

		public override string ToString() {
			return "";
		}
	}
}
