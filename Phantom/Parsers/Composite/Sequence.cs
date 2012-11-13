using Phantom.Parsers.Interfaces;
using Phantom.Scanners;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// A parser which matches a left parser then a right parser.
	/// </summary>
	class Sequence : Binary
	{
		public Sequence(Parser left, Parser right)
			: base(left, right)
		{
		}

		public override ParserMatch TryMatch(IScanner scan)
		{
			// save scanner state
			int offset = scan.Offset;
			ParserMatch m = scan.NoMatch;

			// apply the first parser
			ParserMatch left = bLeftParser.TryMatch(scan);

			// if left successful, do right
			if (left.Success)
			{
				ParserMatch right = bRightParser.TryMatch(scan);

				if (right.Success)
				{
					m = ParserMatch.Concat(this, left, right);
				}
				else
				{
					m = scan.NoMatch;
				}
			}

			// restoring parser failed, rewind scanner
			if (!m.Success)
				scan.Seek(offset);

			return m;
		}

		public override string ToString()
		{
			return "(" + LeftParser + " " + RightParser + ")";
		}
	}
}