using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Create an Exclusive-OR parser from two sub-parsers.
	/// Can match first or second, but not both
	/// </summary>
	public class Exclusive : Binary
	{
		/// <summary>
		/// Create an Exclusive-OR parser from two sub-parsers.
		/// Can match first or second, but not both
		/// </summary>
		public Exclusive(IParser first, IParser second)
			: base(first, second)
		{
		}

		/// <inheritdoc />
		public override ParserMatch TryMatch(IScanner scan)
		{
			// save scanner state
			int offset = scan.Offset;

			// apply the first parser
			var m1 = LeftParser.Parse(scan);
			int m1off = scan.Offset;

			// Go back and try the second
			scan.Seek(offset);

			// apply the second parser
			var m2 = RightParser.Parse(scan);
			int m2off = scan.Offset;

			if (m2.Success && m1.Success)
			{
				// FAIL! they are not exclusive
				scan.Seek(offset);
				return scan.NoMatch;
			}

			// now return whichever one suceeded
			if (m1.Success)
			{
				scan.Seek(m1off);
				return m1;
			}
			if (m2.Success)
			{
				scan.Seek(m2off);
				return m2;
			}

			// neither were matched!
			scan.Seek(offset);
			return scan.NoMatch;
		}

		/// <inheritdoc />
		public override string ToString()
		{
			return LeftParser + " ^ " + RightParser;
		}
	}
}