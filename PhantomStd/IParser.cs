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
	/// Add a tag to this parser
	/// </summary>
	void Tag(string tag);

	/// <summary>
	/// Read the tag added to this parser, if any
	/// </summary>
	string? GetTag();

	/// <summary>
	/// Set the scope direction for this parser.
	/// <ul>
	/// <li>Positive values open a new scope</li>
	/// <li>Negative values close the current scope</li>
	/// <li>Zero value does not change scope (default)</li>
	/// </ul>
	/// </summary>
	void Scope(int sign);

	/// <summary>
	/// Get the scope direction of this parser
	/// <ul>
	/// <li>Positive values open a new scope</li>
	/// <li>Negative values close the current scope</li>
	/// <li>Zero value does not change scope (default)</li>
	/// </ul>
	/// </summary>
	int GetScope();
}