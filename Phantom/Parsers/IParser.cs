using Phantom.Scanners;

namespace Phantom.Parsers
{
	public interface IParser
	{
		ParserMatch Parse(IScanner scan);
	}
}