using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Terminals
{
	/// <summary>
	/// Parser that matches an end of line marker, either <c>\r</c>, <c>\n</c>, or <c>\r\n</c>
	/// </summary>
	public class EndOfLine : Parser, IMatchingParser
	{
		/// <inheritdoc />
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

		/// <inheritdoc />
		public override string ToString()
		{
			return "¬";
		}
	}
}