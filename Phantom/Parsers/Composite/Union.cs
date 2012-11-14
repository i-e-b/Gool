using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Creates a Union (or 'alternative') parser from two sub-parsers.
	/// </summary>
	public class Union : Binary
	{
		public Union(IParser left, IParser right)
			: base(left, right)
		{
		}

		public override ParserMatch TryMatch(IScanner scan)
		{
			// save scanner state
			int offset = scan.Offset;

			// apply the first parser
			var m = LeftParser.Parse(scan);

			// rewind
			scan.Seek(offset);

			// apply the second parser
			var m2 = RightParser.Parse(scan);

			// pick the longest result
			if (m.Success || m2.Success)
			{
				if (m2.Length >= m.Length)
				{
					scan.Seek(m2.Offset + m2.Length);
					return m2;
				}
				scan.Seek(m.Offset + m.Length);
				return m;
			}

			// rewind to point of failure
			scan.Seek(offset);
			return scan.NoMatch;
		}

		public override string ToString()
		{
			return LeftParser + " | " + RightParser;
		}
	}
}