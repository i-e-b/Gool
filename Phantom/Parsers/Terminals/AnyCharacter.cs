using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Terminals {
	/// <summary>
	/// Parser that will match any one character.
	/// </summary>
	public class AnyCharacter : Parser {
		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			int offset = scan.Offset;
			ParserMatch m = scan.CreateMatch(this, offset, 1);
			scan.Read();
			return m;
		}

		public override string ToString() {
			return ".";
		}
	}
}
