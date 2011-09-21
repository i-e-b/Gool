using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Terminals {
	public class LiteralString : Parser {
		private string test;

		public LiteralString(string toMatch) {
			test = toMatch;
		}

		/// <summary>
		/// Gets the literal string that this parser test for.
		/// </summary>
		public string MatchLiteral {
			get { return test; }
		}

		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			int offset = scan.Offset;

			string compare = scan.Substring(offset, test.Length);

			if (compare == test) {
				scan.Seek(offset + test.Length);
				return scan.CreateMatch(this, offset, test.Length);
			}

			scan.Seek(offset);
			return scan.NoMatch;
		}

		public override string ToString() {
			return "\"" + test + "\"";
		}
	}
}
