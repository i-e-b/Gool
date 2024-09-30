using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers;

/// <summary>
/// Interface for a parser that can interpret the content of an <see cref="IScanner"/>,
/// and can be combined with other parsers.
/// <p/>
/// Parsers should <b>NOT</b> hold internal parsing state. Include state in the scanner instead.
/// </summary>
public interface IParser
{
	/// <summary>
	/// Try to interpret the content of <paramref name="scan"/>
	/// </summary>
	ParserMatch Parse(IScanner scan, ParserMatch? previousMatch = null);

	/// <summary>
	/// Optional tag value for this parser.
	/// Tags are used to extract structure from the basic result set.
	/// </summary>
	public string? Tag { get; set; }

	/// <summary>
	/// Optional scope behaviour.
	/// Scopes are used to build result trees from <see cref="Tag"/>ged matches,
	/// using <see cref="ScopeNode.FromMatch"/>.
	/// <ul>
	/// <li>Positive values open a new scope</li>
	/// <li>Negative values close the current scope</li>
	/// <li>Zero value does not change scope (default)</li>
	/// </ul>
	/// </summary>
	public ScopeType Scope { get; set; }

	/// <summary>
	/// Returns true if this parser carries tags or scopes. False otherwise
	/// </summary>
	bool HasMetaData();

	/// <summary>
	/// Limited depth description of this parser
	/// </summary>
	string ShortDescription(int depth);
}