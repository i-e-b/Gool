using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Parsers {
	interface ICompositeParser {

		List<Parser> ChildParsers ();
	}
}
