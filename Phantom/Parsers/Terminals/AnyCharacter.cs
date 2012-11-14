using Phantom.Scanners;

namespace Phantom.Parsers.Terminals
{
	/// <summary>
	/// Parser that will match any one character.
	/// </summary>
	public class AnyCharacter : Parser, ITerminal
	{
		public override ParserMatch TryMatch(IScanner scan)
		{
			if (scan.EndOfInput) return scan.NoMatch;

			int offset = scan.Offset;
			var m = scan.CreateMatch(this, offset, 1);
			scan.Read();
			return m;
		}

		public override string ToString()
		{
			return ".";
		}
	}
}