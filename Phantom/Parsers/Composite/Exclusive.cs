using Phantom.Scanners;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Create an Exclusive-OR parser from two sub-parsers.
	/// </summary>
	class Exclusive : Binary
	{
		public Exclusive(Parser first, Parser second)
			: base(first, second)
		{
		}

		public override ParserMatch TryMatch(IScanner scan)
		{
			// save scanner state
			int offset = scan.Offset;

			// apply the first parser
			var m1 = bLeftParser.TryMatch(scan);
			int m1off = scan.Offset;

			// Go back and try the second
			scan.Seek(offset);

			// apply the second parser
			var m2 = bRightParser.TryMatch(scan);
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

		public override string ToString()
		{
			return LeftParser + "^" + RightParser;
		}
	}
}