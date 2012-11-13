using Phantom.Scanners;

namespace Phantom.Parsers
{
	public interface ITerminal: IParser
	{
		ParserMatch TryMatch(IScanner scan);
	}
}