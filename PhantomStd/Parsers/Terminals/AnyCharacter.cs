using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Terminals
{
	/// <summary>
	/// Parser that will match any one character.
	/// </summary>
	public class AnyCharacter : Parser, IMatchingParser
	{
		/// <inheritdoc />
		public ParserMatch TryMatch(IScanner scan)
		{
			if (scan.EndOfInput) return scan.NoMatch;

			int offset = scan.Offset;
			var m = scan.CreateMatch(this, offset, 1);
			scan.Read();
			return m;
		}

		/// <inheritdoc />
		public override string ToString()
		{
			return ".";
		}
	}
}