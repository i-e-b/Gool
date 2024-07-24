using Phantom.Parsers;

namespace Phantom
{
	/// <summary>
	/// Interface for a parser that can interpret the content of an <see cref="IScanner"/>
	/// </summary>
	public interface IParser
	{
		/// <summary>
		/// Try to interpret the content of <paramref name="scan"/>
		/// </summary>
		ParserMatch Parse(IScanner scan);
	}
}