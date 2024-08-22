using System.Text.RegularExpressions;
using Gool.Parsers.Interfaces;
using Gool.Results;
using JetBrains.Annotations;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parse for a regular expression match.
/// NOTE: It's best to keep the regular expressions simple, and deal with any recursion in the Parser structure.
/// </summary>
public class RegularExpression : Parser, IMatchingParser
{
	private readonly string _pattern;
	private readonly Regex _test;

	/// <summary>
	/// Create a regular expression parser with default options
	/// </summary>
	/// <param name="pattern">Regex pattern to match</param>
	public RegularExpression([RegexPattern]string pattern)
	{
		_pattern = pattern;
		_test = new Regex(pattern);
	}

	/// <summary>
	/// Create a regular expression parser with customised options
	/// </summary>
	/// <param name="pattern">Regex pattern to match</param>
	/// <param name="options">Options set to use</param>
	public RegularExpression([RegexPattern]string pattern, RegexOptions options)
	{
		_pattern = pattern;
		_test = new Regex(pattern, options);
	}

	/// <summary>
	/// Test the regular expression.
	/// </summary>
	/// <remarks>This is done on the entire input.
	/// This might cause problems with file-stream parsing.</remarks>
	public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
	{
		var offset = previousMatch?.Right ?? 0;
		var result = _test.Match(scan.TransformedString, offset);

		if (result.Success != true) return scan.NoMatch(this, previousMatch);
		if (result.Index != offset) return scan.NoMatch(this, previousMatch); // so we don't jump far into the input

		return scan.CreateMatch(this, offset, result.Length);
	}

	/// <inheritdoc />
	public override string ToString()
	{
		var desc = "/"+_pattern+"/";
			
		if (Tag is null) return desc;
		return desc + " Tag='" + Tag + "'";
	}
    
	/// <inheritdoc />
	public override string ShortDescription(int depth)
	{
		return ToString();
	}
}