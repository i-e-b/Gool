namespace Phantom.Scanners
{
	public class ParserPoint
	{
		public readonly object parser;
		public readonly int pos;

		public ParserPoint(object p, int position)
		{
			parser = p;
			pos = position;
		}
	}
}