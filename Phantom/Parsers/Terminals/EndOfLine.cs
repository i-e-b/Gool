using Phantom.Scanners;

namespace Phantom.Parsers.Terminals
{
	public class EndOfLine : Parser, ITerminal
	{
		public override ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;
			int len = 0;

			if (!scan.EOF && scan.Peek() == '\r') // CR
			{
				scan.Read();
				len++;
			}

			if (!scan.EOF && scan.Peek() == '\n') // LF
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