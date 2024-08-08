using Phantom.Parsers;
using Phantom.Results;

namespace Phantom;

/// <summary>
/// Interface for a parser that can interpret the content of an <see cref="IScanner"/>
/// </summary>
public interface IParser
{
	/// <summary>
	/// Try to interpret the content of <paramref name="scan"/>
	/// </summary>
	ParserMatch Parse(IScanner scan, ParserMatch? previousMatch = null);

	/// <summary>
	/// Optional tag value for this parser
	/// </summary>
	public string? Tag { get; set; }

	/// <summary>
	/// Optional scope behaviour
	/// <ul>
	/// <li>Positive values open a new scope</li>
	/// <li>Negative values close the current scope</li>
	/// <li>Zero value does not change scope (default)</li>
	/// </ul>
	/// </summary>
	public int ScopeSign { get; set; }

	/// <summary>
	/// Returns true if this parser carries tags or scopes. False otherwise
	/// </summary>
	bool HasMetaData();
}