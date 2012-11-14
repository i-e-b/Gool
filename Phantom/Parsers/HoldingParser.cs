using System;
using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers
{
	/// <summary>
	/// A parser implementation which contains another parser
	/// and passes all parsing instructions to it.
	/// This is for use with mutually dependant parser trees.
	/// </summary>
	public class HoldingParser : Parser
	{
		IParser heldParser;

		public IParser HeldParser
		{
			get { return heldParser; }
			set
			{
				heldParser = value;
			}
		}

		public ParserMatch TryMatch(IScanner scan)
		{
			if (heldParser == null) throw new Exception("Empty holding parser");
			if (! (heldParser is IMatchingParser)) throw new Exception("Holding parser was non terminating");

			return (heldParser as IMatchingParser).TryMatch(scan);
		}

		public override ParserMatch Parse(IScanner scan)
		{
			scan.Normalise();

			if (scan.RecursionCheck(this, scan.Offset))
				return scan.NoMatch;

			var m = (heldParser is IMatchingParser) 
				? ((heldParser as IMatchingParser).TryMatch(scan))
				: (heldParser.Parse(scan));

			if (m.Success) scan.ClearFailures();
			return m;
		}

		public override string ToString()
		{
			return heldParser.GetType().ToString();
		}
	}
}