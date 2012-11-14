using System.Collections.Generic;

namespace Phantom.Parsers.Interfaces
{
	interface ICompositeParser: ITerminal
	{
		List<IParser> ChildParsers();
	}
}