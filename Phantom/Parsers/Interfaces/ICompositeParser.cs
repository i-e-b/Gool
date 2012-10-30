using System.Collections.Generic;

namespace Phantom.Parsers
{
	interface ICompositeParser
	{
		List<Parser> ChildParsers();
	}
}