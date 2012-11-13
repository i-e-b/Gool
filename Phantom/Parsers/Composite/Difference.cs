using Phantom.Scanners;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Create an Difference parser from two sub-parsers.
	/// </summary>
	class Difference : Binary
	{
		public Difference(Parser left, Parser right)
			: base(left, right)
		{
		}

		public override ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;

			var m = bLeftParser.TryMatch(scan);

			int goodOffset = scan.Offset;

			if (!m.Success)
			{
				scan.Seek(offset);
				return scan.NoMatch;
			}

			// doing difference
			scan.Seek(offset);
			var m2 = bRightParser.TryMatch(scan);
			if (m2.Success)
			{
				// fail: must match left but NOT right
				scan.Seek(offset);
				return scan.NoMatch;
			}

			// Good match
			scan.Seek(goodOffset);
			return m;
		}
	}
}