using System.Collections.Generic;

namespace Phantom.Parsers.Interfaces
{
	interface ICompositeParser: IMatchingParser
	{
		List<IParser> ChildParsers();
	}
}