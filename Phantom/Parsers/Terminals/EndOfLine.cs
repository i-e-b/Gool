using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Terminals {
	public class EndOfLine : Parser {
		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			int offset = scan.Offset;
			int len = 0;

			if (!scan.EOF && scan.Peek() == '\r')    // CR
			{
				scan.Read();
				len++;
			}

			if (!scan.EOF && scan.Peek() == '\n')    // LF
			{
				scan.Read();
				len++;
			}

			if (len > 0) {
				return scan.CreateMatch(this, offset, len);
			}

			scan.Seek(offset);
			return scan.NoMatch;	
		}

		public override string ToString() {
			return "¬";
		}
	}
}
