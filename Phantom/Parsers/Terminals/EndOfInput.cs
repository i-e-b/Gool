using Phantom.Scanners;

namespace Phantom.Parsers.Terminals
{
	public class EndOfInput : Parser
	{
		public override ParserMatch TryMatch(IScanner scan)
		{
			return scan.EOF ? scan.EmptyMatch : scan.NoMatch;
		}

		public override string ToString()
		{
			return "¦¦";
		}
	}
}