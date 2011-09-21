using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Terminals {
	public class LiteralCharacter:Parser {
		private char test;

		public LiteralCharacter(char c) {
			test = c;
		}

		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			int offset = scan.Offset;

			if (scan.EOF) return scan.NoMatch;

			char c = (char)scan.Peek();

			if (c != test) return scan.NoMatch;

			// if we arrive at this point, we have a match
			ParserMatch m = scan.CreateMatch(this, offset, 1);

			// updating offset
			scan.Read();

			// return match
			return m;
		}

		public override string ToString() {
			return test.ToString();
		}
	}
}
