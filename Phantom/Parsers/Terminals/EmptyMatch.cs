using Phantom.Scanners;

namespace Phantom.Parsers.Terminals
{
	/// <summary>
	/// Parser that represents no input.
	/// Always returns an empty success match
	/// </summary>
	public class EmptyMatch : Parser, ITerminal
	{
		public override ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;
			var m = scan.CreateMatch(this, offset, 0);
			return m;
		}

		public override string ToString()
		{
			return "(empty)";
		}
	}
}