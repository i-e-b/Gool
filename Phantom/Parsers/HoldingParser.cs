using System;
using Phantom.Scanners;

namespace Phantom.Parsers
{
	/// <summary>
	/// A parser implementation which contains another parser
	/// and passes all parsing instructions to it.
	/// This is for use with mutually dependant parser trees.
	/// </summary>
	public class HoldingParser : Parser
	{
		protected Parser held_parser;

		public Parser HeldParser
		{
			get { return held_parser; }
			set
			{
				if (held_parser == this) throw new Exception("HoldingParser can't reference itself.");
				held_parser = value;
			}
		}

		public override ParserMatch ParseMain(IScanner scan)
		{
			if (held_parser != null)
			{
				return held_parser.ParseMain(scan);
			}
			else
			{
				throw new Exception("Empty holding parser!!!");
			}
		}

		public override ParserMatch Parse(IScanner scan)
		{
			scan.Normalise();

			if (scan.RecursionCheck(this, scan.Offset))
				if (!(this is HoldingParser))
					return scan.NoMatch;

			ParserMatch m = ParseMain(scan);
			if (m.Success)
			{
				scan.ClearFailures();
			}
			else
			{
			}
			return m;
		}

		public override string ToString()
		{
			return held_parser.GetType().ToString();
		}
	}
}