using System;
using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers
{
	/// <summary>
	/// A parser implementation which contains another parser
	/// and passes all parsing instructions to it.
	/// This is for use with mutually dependant parser trees.
	/// </summary>
	public class Recursion : Parser
	{
		IParser source;

		public IParser Source
		{
			get { return source; }
			set
			{
				source = value;
			}
		}

		public ParserMatch TryMatch(IScanner scan)
		{
			if (source == null) throw new Exception("Empty holding parser");
			if (! (source is IMatchingParser)) throw new Exception("Holding parser was non terminating");

			return (source as IMatchingParser).TryMatch(scan);
		}

		public override ParserMatch Parse(IScanner scan)
		{
			scan.Normalise();

			if (scan.RecursionCheck(this, scan.Offset))
				return scan.NoMatch;

			var m = (source is IMatchingParser) 
				? ((source as IMatchingParser).TryMatch(scan))
				: (source.Parse(scan));

			if (m.Success) scan.ClearFailures();
			return m;
		}

		public override string ToString()
		{
			return source.GetType().ToString();
		}

		/// <summary>
		/// Create a simple Recursion parser.
		/// Input to the function is a holding parser, output is the parser to hold.
		/// (i.e. they end up being the same parser)
		/// </summary>
		public static IParser Over(Func<IParser, IParser> func)
		{
			var hold = new Recursion();
			hold.Source = func(hold);
			return hold;
		}
	}
}