using Phantom.Parsers;

namespace Phantom
{
	public interface IParser
	{
		ParserMatch Parse(IScanner scan);
	}
}