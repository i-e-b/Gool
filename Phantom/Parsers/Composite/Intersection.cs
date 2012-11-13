using Phantom.Parsers.Interfaces;
using Phantom.Scanners;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Create an Intersection parser from two sub-parsers.
	/// </summary>
	class Intersection : Binary
	{
		public Intersection(Parser first, Parser second)
			: base(first, second)
		{
		}

		public override ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;
			//ParserMatch m = scan.NoMatch;

			ParserMatch left = bLeftParser.TryMatch(scan);


			if (left.Success)
			{
				ParserMatch right = bRightParser.TryMatch(scan);
				if (right.Success)
				{
					//m.Concat(m2);
					return ParserMatch.Concat(this, left, right);
				}
			}
			else
			{
				scan.Seek(offset);
				left = bRightParser.TryMatch(scan);
				if (left.Success)
				{
					ParserMatch right = bLeftParser.TryMatch(scan);
					if (right.Success)
					{
						//m.Concat(m2);
						return ParserMatch.Concat(this, left, right);
					}
				}
			}

			scan.Seek(offset);
			return scan.NoMatch;
		}

		public override string ToString()
		{
			return LeftParser + "&" + RightParser;
		}
	}
}