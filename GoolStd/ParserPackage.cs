using Gool.Parsers;
using Gool.Parsers.Terminals;
using Gool.Results;
using Gool.Scanners;

namespace Gool;

/// <summary>
/// A parser, plus the correct scanner options.
/// <p>
/// Building a package can also make some optimisations to
/// the parser structure to improve performance.
/// </p>
/// </summary>
public class ParserPackage
{
	private readonly IParser     _parser;
    private readonly BNF.Options _options;

    /// <summary>
    /// [Optional] Parser used to skip insignificant patterns in the input.
    /// </summary>
    public IParser? AutoAdvance { get; }

    /// <summary>
    /// BNF structure, plus the correct scanner options
    /// </summary>
    internal ParserPackage(BNF bnf, BNF.Options options, IParser? autoAdvance)
    {
        _parser = bnf.InnerParser;
        _options = options;
        AutoAdvance = autoAdvance;
    }

    /// <summary>
    /// Parse an input string, returning a match tree.
    /// <p/>
    /// This can return successful matches that consume only part of the input.
    /// To ensure that the entire input matches the parser, use <see cref="ParseEntireString"/>
    /// </summary>
    /// <param name="input">The string to parse</param>
    /// <param name="offset">Optional. Position in the input to start parsing</param>
    /// <param name="diagnostics">Optional (default: true). Record diagnostic info. Parsing is faster without</param>
    public ParserMatch ParsePartialString(string input, int offset = 0, bool diagnostics = true)
    {
	    return ParseStringWithCustomOptions(input, offset, _options, AutoAdvance, diagnostics, mustConsumeAll: false);
    }

    /// <summary>
    /// Parse an input string, returning a match tree.
    /// This will return a failed match if it does not consume the entire input.
    /// <p/>
    /// To allow matches that use only part of the input, see <see cref="ParsePartialString"/>
    /// </summary>
    /// <param name="input">The string to parse</param>
    /// <param name="offset">Optional. Position in the input to start parsing</param>
    /// <param name="diagnostics">Optional (default: true). Record diagnostic info. Parsing is faster without</param>
    public ParserMatch ParseEntireString(string input, int offset = 0, bool diagnostics = true)
    {
	    return ParseStringWithCustomOptions(input, offset, _options, AutoAdvance, diagnostics, mustConsumeAll: true);
    }

	/// <summary>
	/// Parse an input string, returning a match tree.
	/// </summary>
	/// <param name="input">String to parse</param>
	/// <param name="offset">Optional: Offset into the input to start parsing</param>
	/// <param name="options">Optional: Settings for parsing, which can significantly change result</param>
	/// <param name="autoAdvance">Optional: Custom auto-advance. This overrides <c>Options.SkipWhitespace</c></param>
	/// <param name="recordDiagnostics">Optional, default <c>true</c>: If true, record diagnostics for error reporting. Parsing is faster without</param>
	/// <param name="mustConsumeAll">
	/// Optional, default = <c>false</c>.
	/// If true, parsing will fail if it does not consume all of the input.</param>
	public ParserMatch ParseStringWithCustomOptions(string input, int offset = 0, BNF.Options options = BNF.Options.None, IParser? autoAdvance = null, bool recordDiagnostics = true, bool mustConsumeAll = false)
	{
		var scanner = new ScanStrings(input, recordDiagnostics);

		if (options.HasFlag(BNF.Options.SkipWhitespace)) scanner.AutoAdvance = BNF.WhiteSpaceString;
		if (options.HasFlag(BNF.Options.IgnoreCase)) scanner.Transform = new TransformToLower();
		if (options.HasFlag(BNF.Options.IncludeSkippedElements)) scanner.IncludeSkippedElements = true;

		if (autoAdvance is not null) scanner.AutoAdvance = autoAdvance;

		// Parse as much input as we can
		var result = _parser.Parse(scanner, scanner.CreateMatch(_parser, offset, -1, null));
		if (scanner is IScanningDiagnostics diagnostics) diagnostics.Complete();

		var endPosition = result.Right;

		// If there is trailing insignificant data, consume it
		if (scanner.AutoAdvance is not null)
		{
			var trailing = scanner.AutoAdvance.Parse(scanner, result, false);
			if (trailing.Success)
			{
				if (scanner.IncludeSkippedElements) result = ParserMatch.Join(result, new NullParser("Skipped elements"), result, trailing);
				else endPosition = trailing.Right;
			}
		}

		if (mustConsumeAll && endPosition < input.Length) return scanner.NoMatch(_parser, null);

		return result;
	}
}