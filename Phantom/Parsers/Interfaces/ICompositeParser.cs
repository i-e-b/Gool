using System.Collections.Generic;

namespace Phantom.Parsers.Interfaces
{
	interface ICompositeParser
	{
		List<IParser> ChildParsers();
	}
}