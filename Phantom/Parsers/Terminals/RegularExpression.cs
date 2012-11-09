using System.Text.RegularExpressions;
using Phantom.Scanners;

namespace Phantom.Parsers.Terminals
{
	/// <summary>
	/// Parse for a regular expression match.
	/// NOTE: It's best to keep the regular expressions simple, and deal with any recursion in the Parser structure.
	/// </summary>
	public class RegularExpression : Parser
	{
		readonly Regex test;

		/// <summary>
		/// Create a regular expression parser with default options
		/// </summary>
		/// <param name="pattern">Regex pattern to match</param>
		public RegularExpression(string pattern)
		{
			test = new Regex(pattern);
		}

		/// <summary>
		/// Create a regular expression parser with either
		/// default options or a common set-up.
		/// </summary>
		/// <param name="pattern">Regex pattern to match</param>
		/// <param name="CompiledMultiline">If true, the Regex will be compiled and use Multi-line syntax</param>
		public RegularExpression(string pattern, bool CompiledMultiline)
		{
			if (CompiledMultiline)
			{
				test = new Regex(pattern, RegexOptions.Multiline | RegexOptions.Compiled);
			}
			else
			{
				test = new Regex(pattern);
			}
		}

		/// <summary>
		/// Create a regular expression parser with customised options
		/// </summary>
		/// <param name="pattern">Regex pattern to match</param>
		/// <param name="options">Options set to use</param>
		public RegularExpression(string pattern, RegexOptions options)
		{
			test = new Regex(pattern, options);
		}

		/// <summary>
		/// Test the regular expression.
		/// </summary>
		/// <remarks>This is done on the entire input.
		/// This might cause problems with file-stream parsing.</remarks>
		public override ParserMatch TryMatch(IScanner scan)
		{
			int offset = scan.Offset;

			string remains = scan.RemainingData();
			var result = test.Match(remains);

			if (result.Success && result.Index == 0)
			{
				scan.Seek(offset + result.Length);
				return scan.CreateMatch(this, offset, result.Length);
			}

			return scan.NoMatch;
		}

		public override string ToString()
		{
			return "#(" + test + ")";
		}
	}
}