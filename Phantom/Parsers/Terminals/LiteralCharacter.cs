namespace Phantom.Parsers.Terminals
{
	public class LiteralCharacter : Parser, ITerminal
	{
		readonly char test;

		public LiteralCharacter(char c)
		{
			test = c;
		}

		public ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;

			if (scan.EndOfInput) return scan.NoMatch;

			char c = scan.Peek();

			if (c != test) return scan.NoMatch;

			// if we arrive at this point, we have a match
			var m = scan.CreateMatch(this, offset, 1);

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