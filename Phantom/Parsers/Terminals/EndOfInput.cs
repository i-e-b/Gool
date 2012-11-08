using Phantom.Scanners;

namespace Phantom.Parsers.Terminals
{
	public class EndOfInput : Parser
	{
		public override ParserMatch TryMatch(IScanner scan)
		{
			if (scan.EOF) return scan.EmptyMatch;

			return scan.NoMatch;
		}

		public override string ToString()
		{
			return "¦¦";
		}
	}
}