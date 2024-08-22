using System.Collections.Generic;

namespace Gool.Parsers.Interfaces;

internal interface ICompositeParser: IMatchingParser
{
	List<IParser> ChildParsers();
}