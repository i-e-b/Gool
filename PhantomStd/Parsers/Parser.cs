using PhantomStd.Parsers.Interfaces;

namespace PhantomStd.Parsers
{
	/// <summary>
	/// Base class for all parsers.
	/// </summary>
	public abstract class Parser : IParser
	{
		/// <summary>
		/// Public scanner method. Test scanner input for this parser's patterns.
		/// </summary>
		/// <remarks>Most parsers won't need to override this method</remarks>
		/// <param name="scan">Scanner to parse from</param>
		/// <returns>Match (success of failure) of the parser against the scanne</returns>
		public virtual ParserMatch Parse(IScanner scan)
		{
			scan.Normalise();

			var st = new System.Diagnostics.StackTrace();
			scan.StackStats(st.FrameCount);

			if (scan.RecursionCheck(this, scan.Offset))
				if (!(this is Recursion))
					return scan.NoMatch;

			ParserMatch m;

			if (this is IMatchingParser) m = ((IMatchingParser) this).TryMatch(scan);
			else m = Parse(scan);

			if (m.Success)
			{
				scan.ClearFailures();
			}
			else
			{
				scan.AddFailure(this, scan.Offset);
			}
			return m;
		}
	}
}