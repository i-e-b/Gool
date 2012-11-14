using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Terminals
{
	public class EndOfInput : Parser, IMatchingParser
	{
		public ParserMatch TryMatch(IScanner scan)
		{
			return scan.EndOfInput ? scan.EmptyMatch : scan.NoMatch;
		}

		public override string ToString()
		{
			return "¦¦";
		}
	}
}