using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Terminals
{
	public class EndOfLine : Parser, IMatchingParser
	{
		public ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;
			int len = 0;

			if (!scan.EndOfInput && scan.Peek() == '\r') // CR
			{
				scan.Read();
				len++;
			}

			if (!scan.EndOfInput && scan.Peek() == '\n') // LF
			{
				scan.Read();
				len++;
			}

			if (len > 0)
			{
				return scan.CreateMatch(this, offset, len);
			}

			scan.Seek(offset);
			return scan.NoMatch;
		}

		public override string ToString()
		{
			return "¬";
		}
	}
}