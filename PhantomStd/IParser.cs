using Phantom.Parsers;

namespace Phantom;

/// <summary>
/// Interface for a parser that can interpret the content of an <see cref="IScanner"/>
/// </summary>
public interface IParser
{
	/// <summary>
	/// Try to interpret the content of <paramref name="scan"/>
	/// </summary>
	ParserMatch Parse(IScanner scan);

	/// <summary>
	/// Add a tag to this parser
	/// </summary>
	void Tag(string tag);

	/// <summary>
	/// Read the tag added to this parser, if any
	/// </summary>
	string? GetTag();
}