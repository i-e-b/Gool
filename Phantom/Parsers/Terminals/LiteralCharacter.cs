using Phantom.Scanners;

namespace Phantom.Parsers.Terminals
{
	public class LiteralCharacter : Parser
	{
		readonly char test;

		public LiteralCharacter(char c)
		{
			test = c;
		}

		public override ParserMatch ParseMain(IScanner scan)
		{
			int offset = scan.Offset;

			if (scan.EOF) return scan.NoMatch;

			char c = scan.Peek();

			if (c != test) return scan.NoMatch;

			// if we arrive at this point, we have a match
			ParserMatch m = scan.CreateMatch(this, offset, 1);

			// updating offset
			scan.Read();

			// return match
			return m;
		}

		public override string ToString()
		{
			return test.ToString();
		}
	}
}