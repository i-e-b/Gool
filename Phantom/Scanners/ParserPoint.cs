using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom.Scanners {
	public class ParserPoint {
		public object parser;
		public int pos;

		public ParserPoint(object p, int position) {
			parser = p;
			pos = position;
		}
	}
}
