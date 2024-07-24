using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Terminals
{
	/// <summary>
	/// Parser that matches a single exact character
	/// </summary>
	public class LiteralCharacter : Parser, IMatchingParser
	{
		private readonly char _test;

		/// <summary>
		/// Parser that matches a single exact character
		/// </summary>
		public LiteralCharacter(char c)
		{
			_test = c;
		}

		/// <inheritdoc />
		public ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;

			if (scan.EndOfInput) return scan.NoMatch;

			char c = scan.Peek();

			if (c != _test) return scan.NoMatch;

			// if we arrive at this point, we have a match
			var m = scan.CreateMatch(this, offset, 1);

			// updating offset
			scan.Read();

			// return match
			return m;
		}

		/// <inheritdoc />
		public override string ToString()
		{
			var desc = _test.ToString();
			
			if (TagValue is null) return desc;
			return desc + " Tag='" + TagValue + "'";
		}
	}
}