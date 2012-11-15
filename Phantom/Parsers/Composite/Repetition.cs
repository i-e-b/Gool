using System;
using Phantom.Parsers.Composite.Abstracts;

namespace Phantom.Parsers.Composite
{
	/// <summary>
	/// Create a generalised repetition parser from a single subparser
	/// </summary>
	public class Repetition : Unary
	{
		uint m_LowerBound;
		uint m_UpperBound;

		public Repetition(IParser parser, uint lowerBound, uint upperBound)
			: base(parser)
		{
			SetBounds(lowerBound, upperBound);
		}

		public uint LowerBound
		{
			get { return m_LowerBound; }
		}

		public uint UpperBound
		{
			get { return m_UpperBound; }
		}

		public void SetBounds(uint lb, uint ub)
		{
			if (ub < lb)
				throw new ArgumentException("Lower bound must be less than upper bound");
			m_LowerBound = lb;
			m_UpperBound = ub;
		}

		public override ParserMatch TryMatch(IScanner scan)
		{
			if (Parser == this) return scan.NoMatch;

			// save scanner state
			int offset = scan.Offset;

			var m = new ParserMatch(this, scan, 0, 0); // empty match with this parser

			// execution bound
			int count = 0;

			// lower bound, minimum number of executions
			while (count < LowerBound && !scan.EndOfInput)
			{
				var m_temp = Parser.Parse(scan);
				if (!m_temp.Success) break; // stop if not successful
				count++;
				m.AddSubmatch(m_temp);
			}

			if (count == LowerBound)
			{
				while (count < UpperBound && !scan.EndOfInput)
				{
					var m_temp = Parser.Parse(scan);
					if (!m_temp.Success) break; // stop if not successful
					count++;
					m.AddSubmatch(m_temp);
				}
			}
			else
			{
				m = scan.NoMatch;
			}

			if (m == null) m = scan.NoMatch;

			// restoring parser failed, rewind scanner
			if (!m.Success) scan.Seek(offset);

			return m;
		}

		public override string ToString()
		{
			if (LowerBound == 0 && UpperBound > 1) return Parser + "*";
			if (LowerBound == 0 && UpperBound == 1) return Parser + "?";
			if (LowerBound == 1 && UpperBound > 1) return Parser + "+";
			return "[" + LowerBound + ".." + UpperBound + ":" + Parser + "]";
		}
	}
}