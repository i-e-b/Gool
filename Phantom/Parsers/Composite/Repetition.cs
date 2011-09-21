using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers.Composite {
	/// <summary>
	/// Create a generalised repetition parser from a single subparser
	/// </summary>
	class Repetition : Unary, ICompositeParser {
		private uint m_LowerBound;
		private uint m_UpperBound;

		public Repetition(Parser parser, uint lowerBound, uint upperBound)
			: base(parser) {
			SetBounds(lowerBound, upperBound);
		}

		public uint LowerBound {
			get {
				return m_LowerBound;
			}
		}

		public uint UpperBound {
			get {
				return m_UpperBound;
			}
		}

		public void SetBounds(uint lb, uint ub) {
			if (ub < lb)
				throw new ArgumentException("Lower bound must be less than upper bound");
			m_LowerBound = lb;
			m_UpperBound = ub;
		}

		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scanner) {
			if (Parser == this) return scanner.NoMatch;

			// save scanner state
			int offset = scanner.Offset;

			ParserMatch m = new ParserMatch(this, scanner, 0, 0); // empty match with this parser
			ParserMatch m_temp = null;

			// execution bound
			int count = 0;

			// lower bound, minimum number of executions
			while (count < LowerBound && !scanner.EOF) {
				m_temp = Parser.Parse(scanner);
				if (!m_temp.Success) break; // stop if not successful
				count++;
				m.AddSubmatch(m_temp);
			}

			if (count == LowerBound) {
				while (count < UpperBound && !scanner.EOF) {
					m_temp = Parser.Parse(scanner);
					if (!m_temp.Success) break; // stop if not successful
					count++;
					m.AddSubmatch(m_temp);
				}
			} else {
				m = scanner.NoMatch;
			}

			if (m == null) m = scanner.NoMatch;

			// restoring parser failed, rewind scanner
			if (!m.Success) scanner.Seek(offset);
			
			return m;
		}

		public override string ToString() {
			return "*(" + Parser.ToString() + ")";
		}
	}
}
