using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Terminals {
	public class EndOfInput : Parser {
		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			if (scan.EOF) return scan.EmptyMatch;
			
			return scan.NoMatch;
		}

		public override string ToString() {
			return "¦¦";
		}
	}
}
