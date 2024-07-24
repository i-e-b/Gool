using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Terminals
{
	/// <summary>
	/// Parser that represents no input.
	/// Always returns an empty success match
	/// </summary>
	public class EmptyMatch : Parser, IMatchingParser
	{
		/// <inheritdoc />
		public ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;
			var m = scan.CreateMatch(this, offset, 0);
			return m;
		}

		/// <inheritdoc />
		public override string ToString()
		{
			return "(empty)";
		}
	}
}