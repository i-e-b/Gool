using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers {
	/// <summary>
	/// Creates and stores parser matches.
	/// </summary>
	public class Match {
		private Scanners.IScanner match_scanner;
		private int match_offset;
		private int match_length;

		/// <summary>
		/// Builds a new match
		/// </summary>
		/// <param name="scanner"></param>
		/// <param name="offset"></param>
		/// <param name="length"></param>
		public Match(Scanners.IScanner scanner, int offset, int length) {
			if (scanner == null)
				throw new ArgumentNullException("Tried to create a match from a null scanner.");

			match_scanner = scanner;
			match_offset = offset;
			match_length = length;
		}

		/// <summary>
		/// Scanner
		/// </summary>
		public Scanners.IScanner Scanner { get { return match_scanner; } }

		/// <summary>
		/// Offset
		/// </summary>
		public int Offset { get { return match_offset; } }

		/// <summary>
		/// Length
		/// </summary>
		public int Length { get { return match_length; } }

		/// <summary>
		/// Extracts the match value
		/// </summary>
		public String Value {
			get {
				if (Length < 0)
					throw new Exception("no match");
				return Scanner.Substring(Offset, Length);
			}
		}

		public override string ToString() {
			return this.Value;
		}

		/// <summary>
		/// True if match successfull
		/// </summary>
		public bool Success { get { return Length >= 0; } }

		/// <summary>
		/// True if match empty
		/// </summary>
		public bool Empty { get { return Length <= 0; } }

		/// <summary>
		/// Extends this match's range to include 'm'.
		/// 'm' must start after this match, and must share the same scanner.
		/// </summary>
		/// <param name="m">Match to include in this match</param>
		public void Concat(Match m) {
			if (m == null)
				throw new ArgumentNullException("Can't concatenate null match.");
			if (!m.Success)
				throw new ArgumentException("Can't to concatenate failure match.");
			// if other is empty, return this
			if (m.Empty)
				return;
			if (m.Offset < Offset)
				throw new ArgumentException("Can't do reverse order concatenation.");

			match_length = (int)(m.Offset - Offset) + m.Length;
		}
	}
}
