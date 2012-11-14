using System;

namespace Phantom.Parsers
{
	/// <summary>
	/// A parser implementation which contains another parser
	/// and passes all parsing instructions to it.
	/// This is for use with mutually dependant parser trees.
	/// </summary>
	public class HoldingParser : Parser
	{
		protected IParser held_parser;

		public IParser HeldParser
		{
			get { return held_parser; }
			set
			{
				held_parser = value;
			}
		}

		public ParserMatch TryMatch(IScanner scan)
		{
			if (held_parser == null) throw new Exception("Empty holding parser");
			if (! (held_parser is ITerminal)) throw new Exception("Holding parser was non terminating");

			return (held_parser as ITerminal).TryMatch(scan);
		}

		public override ParserMatch Parse(IScanner scan)
		{
			scan.Normalise();

			if (scan.RecursionCheck(this, scan.Offset))
				return scan.NoMatch;

			var m = (held_parser is ITerminal) ? ((held_parser as ITerminal).TryMatch(scan)) : (held_parser.Parse(scan));
			if (m.Success)
			{
				scan.ClearFailures();
			}
			return m;
		}

		public override string ToString()
		{
			return held_parser.GetType().ToString();
		}
	}
}