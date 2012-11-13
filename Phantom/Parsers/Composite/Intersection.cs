using Phantom.Scanners;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Create an Intersection parser from two sub-parsers.
	/// </summary>
	class Intersection : Binary
	{
		public Intersection(IParser first, IParser second)
			: base(first, second)
		{
		}

		public override ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;

			var left = bLeftParser.Parse(scan);


			if (left.Success)
			{
				var right = bRightParser.Parse(scan);
				if (right.Success)
				{
					return ParserMatch.Concat(this, left, right);
				}
			}
			else
			{
				scan.Seek(offset);
				left = bRightParser.Parse(scan);
				if (left.Success)
				{
					var right = bLeftParser.Parse(scan);
					if (right.Success)
					{
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