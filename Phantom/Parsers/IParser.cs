using Phantom.Scanners;

namespace Phantom.Parsers
{
	public interface IParser
	{
		ParserMatch TryMatch(IScanner scan);
		ParserMatch Parse(IScanner scan);
	}
}