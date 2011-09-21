using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace Phantom.Parsers.Terminals {
	/// <summary>
	/// Parse for a regular expression match.
	/// NOTE: It's best to keep the regular expressions simple, and deal with any recursion in the Parser structure.
	/// TODO: Extend Parsers.Match to cope with backreferences.
	/// </summary>
	public class RegularExpression : Parser {
		private Regex test;

		/// <summary>
		/// Create a regular expression parser with default options
		/// </summary>
		/// <param name="pattern">Regex pattern to match</param>
		public RegularExpression(string pattern) {
			test = new Regex(pattern);
		}

		/// <summary>
		/// Create a regular expression parser with either
		/// default options or a common set-up.
		/// </summary>
		/// <param name="pattern">Regex pattern to match</param>
		/// <param name="CompiledMultiline">If true, the Regex will be compiled and use Multi-line syntax</param>
		public RegularExpression(string pattern, bool CompiledMultiline) {
			if (CompiledMultiline) {
				test = new Regex(pattern, RegexOptions.Multiline | RegexOptions.Compiled);
			} else {
				test = new Regex(pattern);
			}
		}

		/// <summary>
		/// Create a regular expression parser with customised options
		/// </summary>
		/// <param name="pattern">Regex pattern to match</param>
		/// <param name="options">Options set to use</param>
		public RegularExpression(string pattern, RegexOptions options) {
			test = new Regex(pattern, options);
		}

		/// <summary>
		/// Test the regular expression.
		/// </summary>
		/// <remarks>This is done on the entire input.
		/// This might cause problems with file-stream parsing.</remarks>
		public override ParserMatch ParseMain(Phantom.Scanners.IScanner scan) {
			int offset = scan.Offset;

			//TODO: Find a more elegant was of parsing for regular expressions.
			string remains = scan.RemainingData();

			System.Text.RegularExpressions.Match result = test.Match(remains);

			if (result.Success && result.Index == 0) {
				//TODO: It would probably be useful to have access to Groups and backrefs in the returned match.
				scan.Seek(offset + result.Length);
				return scan.CreateMatch(this, offset, result.Length);
			} else {
				scan.Seek(offset);
				return scan.NoMatch;
			}
		}

		public override string ToString() {
			return "#(" + test.ToString() + ")";
		}
	}
}
