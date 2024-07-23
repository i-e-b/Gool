using System.Collections.Generic;

namespace PhantomStd.Parsers.Interfaces
{
	interface ICompositeParser: IMatchingParser
	{
		List<IParser> ChildParsers();
	}
}