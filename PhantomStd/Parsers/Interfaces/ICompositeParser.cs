using System.Collections.Generic;

namespace Phantom.Parsers.Interfaces
{
	internal interface ICompositeParser: IMatchingParser
	{
		List<IParser> ChildParsers();
	}
}